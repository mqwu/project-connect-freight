rm(list = ls())

library(dplyr)
library(plyr)

path = 'Y:/Public Folder/Tim/connected_freight/input_data/tour plans 08 and 09 2017 SetPower CentroAsia/CentroAsia'
months = dir(path)

all_routes = data.frame()

for(month in months){
  
  days = dir(file.path(path, month))
  
  for(day in days){
    
    ####################### read data files 
    file_post = dir(path = file.path(path, month, day), pattern = 'POST', full.names = T)
    file_get = dir(path = file.path(path, month, day), pattern = 'GET', full.names = T)
    
    Data_post = lapply(file_post, read_json, stringsAsFactors = F) %>% unique()
    Data_post = do.call(function(x, ...)mapply(c, x, ...), Data_post)

    Data_get = lapply(file_get, read_json, stringsAsFactors = F) %>% unique()
    Data_get = do.call(function(x, ...)mapply(c, x, ...), Data_get)
    
    ############################## extract list of stops
    Nstops = vapply(1:length(Data_get$tours), function(x){length(Data_get$tours[[x]]$stops)}, FUN.VALUE = 0)
    Nstops_tot = sum(Nstops)
    
    routes = data.frame(tour_num = character(Nstops_tot), courier_id = character(Nstops_tot), stop_num = character(Nstops_tot), 
                        from_lat = character(Nstops_tot), from_long = character(Nstops_tot), 
                        to_lat = character(Nstops_tot), to_long = character(Nstops_tot), stringsAsFactors = F)
    
    
    for(tr in 1:length(Nstops)){
      
      if(tr == 1){idx = 1:(Nstops[tr])}else{idx = (sum(Nstops[1:(tr - 1)]) + 1):sum(Nstops[1:tr])}
      routes$tour_num[idx] = tr
      routes$courier_id[idx] = Data_get$tours[[tr]]$courier_id
      routes$stop_num[idx] = 1:(Nstops[tr])
      
      stops = do.call(rbind, Data_get$tours[[tr]]$stops)
      
      routes$from_lat[idx[1]] =  unlist(stops[1, 'lat'])
      routes$from_long[idx[1]] = unlist(stops[1, 'lng'])
      
      routes$from_lat[idx[-1]] =  unlist(stops[-nrow(stops), 'lat'])
      routes$from_long[idx[-1]] = unlist(stops[-nrow(stops), 'lng'])
      
      routes$to_lat[idx] = unlist(stops[, 'lat'])
      routes$to_long[idx] = unlist(stops[, 'lng'])
      
    }
    
    ########################## get location ids
    locations = do.call(rbind, Data_post$locations) %>% data.frame()
    locations = lapply(locations, unlist) %>% data.frame() %>% unique()
    locations$loc_id = 1:nrow(locations)
    
    routes = merge(routes, locations, by.x = c('from_lat', 'from_long'), by.y = c('lat', 'lng'), all.x = T, all.y = F) %>%
      dplyr::rename(from_loc_id = loc_id)
    
    routes = merge(routes, locations, by.x = c('to_lat', 'to_long'), by.y = c('lat', 'lng'), all.x = T, all.y = F) %>%
      dplyr::rename(to_loc_id = loc_id)
    
    ############################### get list of couriers
    couriers = do.call(rbind, Data_post$couriers) %>% data.frame() %>% select(one_of(c('id', "cost_per_km", "cost_per_hour", 'vehicle_type'))) 
    couriers = lapply(couriers, unlist) %>% data.frame() %>% unique()
    
    routes = merge(routes, couriers, by.x = 'courier_id', by.y = 'id', all.x = T, all.y = F)
    
    ############################## get distance matrix between locations
   
    vehicle_types = names(Data_post$distance_matrix) %>% unique()
    Nloc = unlist(lapply(Data_post$distance_matrix, length))[names(Data_post$distance_matrix) == vehicle_types[1]]
    
    distances = data.frame(vehicle_type = rep(vehicle_types, each = sum(Nloc) ^ 2), from_loc_id = rep(1:sum(Nloc), each = sum(Nloc)), 
                           to_loc_id = rep(1:sum(Nloc), sum(Nloc)), time = NA, distance = NA)
    
    
    for(d in 1:length(Data_post$distance_matrix)){
      
      fl = ceiling(d/length(vehicle_types))
      if(fl == 1){locs = 1:Nloc[fl]}else{locs = (sum(Nloc[1:(fl - 1)]) + 1):sum(Nloc[1:fl])}
      idx = which(distances$vehicle_type == names(Data_post$distance_matrix)[d] & distances$from_loc_id %in% locs & distances$to_loc_id %in% locs)
      
      distances$time[idx] = vapply(idx, function(x){Data_post$distance_matrix[[d]][[which(locs == distances$from_loc_id[x])]][[which(locs == distances$to_loc_id[x])]]$time}, FUN.VALUE = 0)
      distances$distance[idx] = vapply(idx, function(x){Data_post$distance_matrix[[d]][[which(locs == distances$from_loc_id[x])]][[which(locs == distances$to_loc_id[x])]]$distance}, FUN.VALUE = 0)
      
    }
    
    routes = merge(routes, distances, all.x = T, all.y = F, by = c('vehicle_type', 'from_loc_id', 'to_loc_id'))
    
    ################################## calculate costs
    routes$cost = routes$cost_per_hour * routes$time + routes$cost_per_km * routes$distance
    routes$date = paste0('2017-', month, '-', day)
    all_routes = rbind(all_routes, routes)
    
    
  }
  
}

cost_per_day = aggregate(cbind(cost, time, distance) ~ date, all_routes, FUN = sum)  
vehicles_per_day = aggregate(courier_id ~ date, all_routes, FUN = function(x)length(unique(x)))



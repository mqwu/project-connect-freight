rm(list = ls())

## require
# 1. extract totl dist
# 2. of vehs




library(dplyr)
library(plyr)
library(jsonlite)

path = 'Y:/Public Folder/Tim/connected_freight/input_data/tour plans 08 and 09 2017 SetPower CentroAsia/CentroAsia'
months = dir(path)  

all_routes = data.frame()

for(month in months){
  
  days = dir(file.path(path, month))
  
  for(day in days){
  
    ####################### read data files 
    file_post = dir(path = file.path(path, month, day), pattern = 'POST', full.names = T)[1] ##post parameter of opt, dist betwen sites 
    file_get = dir(path = file.path(path, month, day), pattern = 'GET', full.names = T)[1]   ## get has detail of the route
    
    Data_post = read_json(file_post, stringsAsFactors = F)
    
    Data_get = read_json(file_get, stringsAsFactors = F)
               
    ############################## extract list of stops
    Nstops = vapply(1:length(Data_get$tours), function(x){length(Data_get$tours[[x]]$stops)}, FUN.VALUE = 0)  # stop in each tours, tours: one vehile one day (one days work)
    Nstops_tot = sum(Nstops)
    
    routes = data.frame(tour_num = character(Nstops_tot), courier_id = character(Nstops_tot), stop_num = character(Nstops_tot), 
                        from_lat = character(Nstops_tot), from_long = character(Nstops_tot), 
                        to_lat = character(Nstops_tot), to_long = character(Nstops_tot), stringsAsFactors = F)
    
    
    for(tr in 1:length(Nstops)){ #looping of tours on a particular day
      
      if(tr == 1){idx = 1:(Nstops[tr])}else{idx = sum(Nstops[1:(tr - 1)] + 1):sum(Nstops[1:tr])}  # each row is one stop, col: carrier
      routes$tour_num[idx] = tr
      routes$courier_id[idx] = Data_get$tours[[tr]]$courier_id  ## data str??
      routes$stop_num[idx] = 1:(Nstops[tr])
      
      stops = do.call(rbind, Data_get$tours[[tr]]$stops)
      
      routes$from_lat[idx[1]] =  unlist(stops[1, 'lat'])
      routes$from_long[idx[1]] = unlist(stops[1, 'lng'])
    
      routes$from_lat[idx[-1]] =  unlist(stops[-nrow(stops), 'lat'])
      routes$from_long[idx[-1]] = unlist(stops[-nrow(stops), 'lng'])
      
      routes$to_lat[idx] = unlist(stops[, 'lat'])
      routes$to_long[idx] = unlist(stops[, 'lng'])
        
    }
    
    ########################## get location ids (lableing each location, labeling is for a particular day: 1,2..)
    locations = do.call(rbind, Data_post$locations) %>% data.frame()
    locations = lapply(locations, unlist) %>% data.frame()
    locations$loc_id = 1:nrow(locations)
    
    routes = merge(routes, locations, by.x = c('from_lat', 'from_long'), by.y = c('lat', 'lng'), all.x = T, all.y = F) %>%
                dplyr::rename(from_loc_id = loc_id)
    
    routes = merge(routes, locations, by.x = c('to_lat', 'to_long'), by.y = c('lat', 'lng'), all.x = T, all.y = F) %>%
      dplyr::rename(to_loc_id = loc_id)
    
    ############################### get list of couriers
    couriers = do.call(rbind, Data_post$couriers) %>% data.frame() %>% select(one_of(c('id', "cost_per_km", "cost_per_hour", 'vehicle_type'))) 
    couriers = lapply(couriers, unlist) %>% data.frame()
    
    routes = merge(routes, couriers, by.x = 'courier_id', by.y = 'id', all.x = T, all.y = F)
    
    ############################## get distance matrix between locations
    Nloc = length(Data_post$distance_matrix[[1]])
    vehicle_types = names(Data_post$distance_matrix)
    distances = data.frame(vehicle_type = rep(vehicle_types, each = Nloc ^ 2), from_loc_id = rep(1:Nloc, each = Nloc),  # driving distance
                           to_loc_id = rep(1:Nloc, Nloc), time = NA, distance = NA)
    
    distances$time = vapply(1:nrow(distances), function(x){Data_post$distance_matrix[[distances$vehicle_type[x]]][[distances$from_loc_id[x]]][[distances$to_loc_id[x]]]$time}, FUN.VALUE = 0)
    distances$distance = vapply(1:nrow(distances), function(x){Data_post$distance_matrix[[distances$vehicle_type[x]]][[distances$from_loc_id[x]]][[distances$to_loc_id[x]]]$distance}, FUN.VALUE = 0)
    
    routes = merge(routes, distances, all.x = T, all.y = F, by = c('vehicle_type', 'from_loc_id', 'to_loc_id'))
    
    ################################## calculate costs
    routes$cost = routes$cost_per_hour * routes$time + routes$cost_per_km * routes$distance
    routes$date = paste0('2017-', month, '-', day)
    all_routes = rbind(all_routes, routes)  # cost_per_km is not accurate

    
  }

}

cost_per_day = aggregate(cbind(cost, time, distance) ~ date, all_routes, FUN = sum) # they may only need distance 
vehicles_per_day = aggregate(courier_id ~ date, all_routes, FUN = function(x)length(unique(x)))

## is this applicable for both base and bundle case?


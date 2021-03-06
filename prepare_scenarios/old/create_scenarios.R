rm(list = ls())

library(plyr)
library(dplyr)
library(xlsx)
library(mvtnorm)
library(reshape2)

setwd("Z:/project/connected_freight/R_code/prepare_scenarios")
source('./agg_module.R')
source('./do_pairing.R')

pre_data_folder = 'Y:/Public Folder/Tim/connected_freight/prepared_data'
raw_data_folder = 'Y:/Public Folder/Tim/connected_freight/input_data'

# retail site
supplier_data = read.xlsx(file.path(pre_data_folder, 'supplier_data.xlsx'), sheetIndex = 1) ########## list of suppliers to be considered
shell_sales_data = read.csv(file.path(pre_data_folder, 'consolidated_sales_data_v2.csv')) ############ internal shell sales data (link to central database, customer sales data, per transanc data)
additional_shell_data  = read.csv(file.path(pre_data_folder, 'additional_sales.csv')) ################ additional sales data ?? what is the difference from internal sales (SKU100, smaller site, not link to central database, supplier deliver info, but we do not now what is actually saled)
product_dimensions = read.csv(file.path(pre_data_folder, 'product_category_dimensions.csv')) ############## product dimensions and weights (tim made up)
site_name_mapping = read.csv(file.path(raw_data_folder, 'Shell - Centro Asia SIte Name Mapping.csv')) ################ mapping of Shell site names between Centro and Shell convention
# two names for shell petro station(shell and centro.asia) + Lat, Long
centro_sales_data = read.csv(file.path(pre_data_folder, 'centro_sept.csv'))   ############# Centro Asia Sales Data (supplier, non-shell retail site sales data)
# pairing shell site with non-shell site based on sales distance and amt speding on each catergory 

retailer_addresses = read.xlsx(file.path(raw_data_folder, 'Suppliers Addresses_CA Non-Shell Retailers.xlsx'), sheetName = 'Centro Asia Non-Shell Retailers') ########### retailer addresses
# non-shell site


#################################### fill in blank locations for suppliers
supplier_data = supplier_data %>% filter(is.na(Lat) | is.na(Lng) | (Lat > 14 & Lat < 15 & Lng > 120.8 & Lng < 121.2))  ## restrict to manila areas, outside box will be excluded

idx = is.na(supplier_data$Lat) | is.na(supplier_data$Lng)

if(sum(idx > 0)){
  
  idx_fill = sample((1:nrow(supplier_data))[!idx], sum(idx), replace = T)
  supplier_data[idx, c('Lat', 'Lng')] = supplier_data[idx_fill, c('Lat', 'Lng')] + rmvnorm(sum(idx), mean  = c(0, 0), sigma = diag(c(4e-5, 4e-5)))
  
}

###################################### merge with Shell Sales Data
# stacking
shell_sales_data = bind_rows(shell_sales_data, additional_shell_data)

shell_sales_data = merge(shell_sales_data, site_name_mapping[, c('Site.Name..Shell.Naming.Convention.', 'Site.Address...Shell', 'lat', 'long')], 
                         all.x = F, all.y = F, by.x = 'Site.Name', by.y = 'Site.Name..Shell.Naming.Convention.')

######################## merge with supplier data
Y = shell_sales_data
shell_sales_data = merge(supplier_data, shell_sales_data, by = 'Supplier', all = F)

Keep_Cols = c('Supplier', 'Lat', 'Lng', 'Days.Supplier.Doesn.t.Deliver', 'Likelihood.of.Non.Shell.Deliveries', 'deliveries.per.month',
              'Date', 'Product.Name', 'Category.Name', 'Quantity', 'Site.Name', 'Site.Address...Shell', 'lat', 'long')
shell_sales_data = shell_sales_data %>% select(one_of(Keep_Cols)) %>% 
                          dplyr::rename(pickup_name = Supplier, pickup_lat = Lat, pickup_long = Lng, package_quantity = Quantity, 
                                        delivery_name = Site.Name, delivery_address = Site.Address...Shell, delivery_lat = lat, delivery_long = long)


###################################### aggregate to deliveries and add in dimensions
# agg based on supplier dlivery freq
deliveries = agg_module(shell_sales_data) %>% filter(Date == '2017-08-16')  ##

deliveries$Category.Name = toupper(deliveries$Category.Name)

deliveries = merge(deliveries, product_dimensions, all.x = T, all.y = F, by = 'Category.Name')

idx = is.na(deliveries$Weight_kg)
deliveries[idx, c("Length_cm", "Width_cm", "Height_cm", "Weight_kg")] = product_dimensions[product_dimensions$Category.Name == 'Other / Missing', c("Length_cm", "Width_cm", "Height_cm", "Weight_kg")]

deliveries = deliveries %>% dplyr::rename(package_length = Length_cm, package_width = Width_cm, package_height = Height_cm, package_weight = Weight_kg)

############################################# Centro Asia data and Site Pairing
## pairing based on distance
## aggregate sales vol by site and see which are similar for shell site and non-shell site, then we paring them

centro_sales_data_shell = centro_sales_data %>% filter(grepl('SHELL', toupper(Customer)))
centro_sales_data = centro_sales_data %>% filter(!grepl('SHELL', toupper(Customer)))

centro_sales_data_shell = merge(centro_sales_data_shell, site_name_mapping, by.x = 'Customer', by.y = 'Site.Name...Centro.Asia.Naming.Convention', all = F) %>%
                              select(one_of(c(names(centro_sales_data), 'Site.Name..Shell.Naming.Convention.')))
centro_sales_data = bind_rows(centro_sales_data, centro_sales_data_shell)

# summarize sales per catergory per site                              
# each row is site, col is each catergory of sales 
centro_by_category = centro_sales_data %>% dcast(Customer + Site.Name..Shell.Naming.Convention. ~ Category, fun.aggregate = sum, value.var = 'Amount') %>% select(-matches('Var.2'))
                                        
pairing = do_pairing(centro_by_category)

pairing = merge(unique(centro_by_category[,  c("Site.Name..Shell.Naming.Convention.", "Customer")]), pairing, by.x = 'Customer', by.y = 'Shell', all = F) %>%
                select(-matches('Customer')) %>% dplyr::rename(Shell = Site.Name..Shell.Naming.Convention.)
                                        


################################################## merge in retailer locations for Non Shell and simulate loactions
# fill in retailer missing
retailer_addresses = retailer_addresses %>% filter(is.na(Lat) | is.na(Long) | (Lat > 14 & Lat < 15 & Long > 120.8 & Long < 121.2))

pairing = merge(pairing, retailer_addresses, by.x = 'Competitor', by.y = 'ORIGINAL.SITE.NAME', all.x = T, all.y = F) %>% 
              select(one_of(c('Shell', 'Competitor', 'ADDRESS', 'Lat', 'Long'))) %>%
              dplyr::rename(delivery_name_new = Competitor, delivery_address_new = ADDRESS, delivery_lat_new = Lat, delivery_long_new = Long)

idx = is.na(pairing$delivery_lat_new) | is.na(pairing$delivery_long_new)

if(sum(idx > 0)){
  
  idx_fill = sample((1:nrow(pairing))[!idx], sum(idx), replace = T)
  pairing[idx, c('delivery_lat_new', 'delivery_long_new')] = pairing[idx_fill, c('delivery_lat_new', 'delivery_long_new')] + rmvnorm(sum(idx), mean  = c(0, 0), sigma = diag(c(4e-5, 4e-5)))
  
}

################################################ join in transactions for paired 9non-shell sites
## copy paired shell delivery schedue to the non-shell site.
non_shell_deliveries = merge(deliveries, pairing, by.x = 'delivery_name', by.y = 'Shell', all = F) %>%
                        select(-one_of(c('delivery_name', 'delivery_address', 'delivery_lat', 'delivery_long'))) %>%
                        dplyr::rename(delivery_name = delivery_name_new, delivery_address = delivery_address_new, delivery_lat = delivery_lat_new, delivery_long = delivery_long_new)

# clean trasaction! 
sample = unique(non_shell_deliveries[, c('Likelihood.of.Non.Shell.Deliveries', 'pickup_name', 'delivery_name')])  # in terms supplier monopoly

sample$prob = ifelse(sample$Likelihood.of.Non.Shell.Deliveries == 'High', 0.9,
                     ifelse(sample$Likelihood.of.Non.Shell.Deliveries == 'Medium', 0.5,
                            ifelse(sample$Likelihood.of.Non.Shell.Deliveries == 'Low', 0.1, 0.5)))  # remove the transaction if it is low prob, not replace the supplier.

sample$include = rbinom(nrow(sample), 1, sample$prob)

sample = sample[sample$include == 1, c('Likelihood.of.Non.Shell.Deliveries', 'pickup_name', 'delivery_name')]

non_shell_deliveries = merge(non_shell_deliveries, sample, all.x = F, all.y = F)

################################################ finalise

deliveries = bind_rows(deliveries, non_shell_deliveries)

deliveries$pickup_country_code = 'ph'
deliveries$delivery_country_code = 'ph'
deliveries$pickup_after = paste0(deliveries$Date, ' 00:00:00')  # based on supplier working schedule
deliveries$delivery_after = paste0(deliveries$Date, ' 09:00:00')
deliveries$delivery_before = paste0(deliveries$Date, ' 21:00:00')

deliveries = deliveries %>% select(-one_of("Days.Supplier.Doesn.t.Deliver", "Likelihood.of.Non.Shell.Deliveries", 
                                           "deliveries.per.month", "Product.Name", "Category.Name", "Comment", 'Datepa'))


## transanction data
## opt twice
# Sci1: one car can pick up from different suppliers (low cost in mileage)
# Sci2: one car can pick up only from one suppliers  (high cost in mileage)




                                        
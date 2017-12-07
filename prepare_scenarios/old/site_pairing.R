## is this being used???
rm(list = ls())

library(plyr)
library(dplyr)
library(reshape2)


input_path = 'R:/Public Folder/Tim/connected_freight/input_data/'
prep_path = 'R:/Public Folder/Tim/connected_freight/prepared_data/'

site_name_mapping = read.csv(file.path(input_path, 'Shell - Centro Asia SIte Name Mapping.csv'), stringsAsFactors = F) %>% select(matches('Site.Name...Centro.Asia.Naming.Convention'))
centro_data = read.csv(file.path(prep_path, 'centro_sept.csv'), stringsAsFactors = F)

centro_shell = centro_data %>% filter(grepl('SHELL', Customer))
centro_non_shell = centro_data %>% filter(!grepl('SHELL', Customer))

Data = merge(centro_shell, site_name_mapping, all = F, by.x = 'Customer', by.y = 'Site.Name...Centro.Asia.Naming.Convention')

Data = Data %>% rbind(centro_non_shell) %>% dcast(Customer ~ Category, fun.aggregate = sum, value.var = 'Amount') %>% select(-matches('Var.2'))

distance = Data %>% select(-matches('Customer')) %>% dist(method = 'canberra') %>% as.matrix() %>% data.frame()

row.names(distance) = names(distance) = Data$Customer

distance = distance[grepl('SHELL', names(distance)), !grepl('SHELL', names(distance))]

distance[is.na(distance)] = 1e10
distance[distance == 0] = 1e-10

pairing = data.frame(Competitor = names(distance))
pairing$Shell = row.names(distance)[apply(distance, 2, function(x)sample(1:length(x), 1, prob = 1/x))]

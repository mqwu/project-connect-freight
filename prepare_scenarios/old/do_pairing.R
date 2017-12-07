

do_pairing = function(Data){

# cal dist based on revenue
distance = as.data.frame(as.matrix(dist(Data[, !names(Data) %in% c('Customer')], upper = T, diag = T, 
                                        method = 'euclidean')))

row.names(distance) = names(distance) = Data$Customer

# dist between shell and non-shell
distance = distance[grepl('SHELL', names(distance)), !grepl('SHELL', names(distance))]

distance[is.na(distance)] = 1e10
distance[distance == 0] = 1e-10

## paring based on inversed dist prob.
pairing = data.frame(Competitor = names(distance))
pairing$Shell = row.names(distance)[apply(distance, 2, function(x)sample(1:length(x), 1, prob = 1/x))]

pairing

}



# input: sales data
# output: delivery date for the bundlled transaction/product
agg_module = function(Data){
library(dplyr)
library(plyr)
library(parallel)

###################### Replace NULLs
idx = is.na(Data$Days.Supplier.Doesn.t.Deliver)
if(sum(!idx) > 0){
  
  Data$Days.Supplier.Doesn.t.Deliver[idx] = sample(x = Data$Days.Supplier.Doesn.t.Deliver[!idx], replace = T, size = sum(idx))
  
} else{
  
  Data$Days.Supplier.Doesn.t.Deliver[idx] = ''
  
}


idx = is.na(Data$deliveries.per.month)
if(sum(!idx) > 0){
  
  Data$deliveries.per.month[idx] = sample(x = Data$deliveries.per.month[!idx], replace = T, size = sum(idx))
  
} else{
  
  Data$deliveries.per.month[idx] = sample(c(1, 4, 8, 25), replace = T, size = sum(idx))
  
}


idx = is.na(Data$Likelihood.of.Non.Shell.Deliveries)
if(sum(!idx) > 0){
  
  Data$Likelihood.of.Non.Shell.Deliveries[idx] = sample(x = Data$Likelihood.of.Non.Shell.Deliveries[!idx], replace = T, size = sum(idx))
  
} else{
  
  Data$Likelihood.of.Non.Shell.Deliveries[idx] = sample(c('High', 'Medium', 'Low'), replace = T, size = sum(idx))   ##?? likelihood non-shell deliver means?
  
}



agg_deliveries = function(Data){
  
  Data$Date = as.Date(Data$Date)
  
  date_num = data.frame(Date = seq.Date(min(Data$Date), max(Data$Date), by = 'days'), label = NA)  # number of dates for all sales date
  
  delivery_spacing = round(365 / (12 * unique(Data$deliveries.per.month)[1]))  # how many days between each delivery
  
  num_deliv = ceiling(nrow(date_num) / delivery_spacing) + 1  ## within the range of data, how many delievery we need
  label = rep(1:num_deliv, each = delivery_spacing)  # label delivery
  overlap = length(label) - nrow(date_num)  # trim tail and head of the lable
  offset = sample(1:overlap, 1)  # random offset delivery date for diff supplier
  
  date_num$label = label[offset : (length(label) - overlap + offset - 1)]  # trim down vec lable, put into a df 
  delivery_date = aggregate(Date ~ label, data = date_num, FUN = min)  # what delivery date is for each actually sales.
  
  non_days = unique(Data$Days.Supplier.Doesn.t.Deliver)  # dates supplier does not delivery
  idx = 1
  while(sum(idx) > 0){ # if supplier do not deliver, you shift 1 day
    idx = weekdays(delivery_date$Date) %in% non_days
    delivery_date$Date[idx] = delivery_date$Date[idx] + 1
  }
  
  Data = merge(Data, date_num, by = 'Date', all.x = T, all.y = F) %>% select(-matches('Date')) %>%
    merge(delivery_date, by = 'label', all.x = T, all.y = F) %>% select(-matches('label'))  # actually transaction date replaced by delivery date
  Data
}

cl = makeCluster(detectCores() - 1)

Data_list = split(Data, list(pickup_name = Data$pickup_name, delivery_name = Data$delivery_name))
out = by(cl, Data_list, agg_deliveries)
stopCluster(cl)
out = do.call(rbind.data.frame, out)

out

}

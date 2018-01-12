clean_merge_EIA_860_923_data <- function(F860_data, F932_data, F860M_data){
  
  #read in EIA F860 and F923 data sets (hav3 already had some columns deleted)
  f860 <- read.csv(F860_data)
  f923 <- read.csv(F932_data)
  
  ######## clean up F860 data  ######## 
  
  #reduce F860 data down to just the rows we care to aggregate on (nameplace capacity)
  f860_r1 <- f860[c('Utility.ID', 'Plant.Code', 'Technology', 'Prime.Mover', 'Energy.Source.1', 'Nameplate.Capacity..MW.')]
  
  #aggregate F860 data in order to get nameplate capacity based on utility ID, plantc code, technology, and prime mover
  ex2 <- as.data.frame(aggregate(x = f860_r1$Nameplate.Capacity..MW., by = list(f860_r1$Utility.ID, f860_r1$Plant.Code, f860_r1$Technology, f860_r1$Prime.Mover, f860_r1$Energy.Source.1), FUN = sum))
  
  #rename the columns after aggregate in order to make easier to read
  names(ex2) <- c('Utility.ID', 'Plant.Code', 'Technology', 'Prime.Mover', 'Energy.Source.1', 'Nameplate.Capacity')
  
  #reduce F860 data down to just the rows we care to aggregate on (operating year)
  f860_r2 <- f860[c('Utility.ID', 'Plant.Code', 'Technology', 'Prime.Mover', 'Energy.Source.1', 'Operating.Year')]
  
  #aggregate F860 data in order to get average, min, and max operating year based on utility ID, plantc code, technology, and prime mover
  ex3 <- as.data.frame(aggregate(x = f860_r2$Operating.Year, by = list(f860_r2$Utility.ID, f860_r2$Plant.Code, f860_r2$Technology, f860_r2$Prime.Mover, f860_r2$Energy.Source.1), FUN = function(x) c(avg = mean(x), max = max(x), min = min(x))))
  
  #convert that last vector, x, that is holding three columns into three distince columns
  ex4 <- as.data.frame(cbind(ex3[c('Group.1', 'Group.2', 'Group.3', 'Group.4', 'Group.5')], as.data.frame(as.matrix(ex3$x))))
  
  #rename the columns after aggregate in order to make easier to read
  names(ex4) <- c('Utility.ID', 'Plant.Code', 'Technology', 'Prime.Mover', 'Energy.Source.1', 'Operating.Year.avg', 'Operating.Year.max', 'Operating.Year.min')
  
  #merge the two different F860 datasets back together
  m1 <- merge(x = ex2, y = ex4, by = c('Utility.ID', 'Plant.Code', 'Technology', 'Prime.Mover', 'Energy.Source.1'))
  
  #check to make sure that an unaccpetable amoutn of data are not lost
  # print(dim(ex2))
  # print(dim(ex4))
  # print(dim(m1))
  
  
  ######## clean up F923 data  ######## 
  
  #aggregate F923 data in order to get total MWh generated based on plant ID, operator ID, prime mover, and fuel type
  a1 <- aggregate(x = cbind(f923$Net.Generation..Megawatthours., f923$Total.Fuel.Consumption.MMBtu), by = list(f923$Plant.Id, f923$Operator.Id, f923$Reported.Prime.Mover, f923$Reported.Fuel.Type.Code), FUN = sum)
  
  #rename the columns after aggregate in order to make easier to read
  names(a1) <- c('Plant.Id', 'Operator.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Net.Generation..Megawatthours.', 'Total.Fuel.Consumption.MMBtu')
  
  ######## merge F860 anf F923 data together  ########   
  
  #merge the F860 and F923 data together
  m3 <- merge(x = m1, y = a1, by.x = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1'), by.y = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
  
  #capcluate the total amount of capacity and energy in the final dataset as compared to what we started with
  cap <- sum(m3$Nameplate.Capacity)/sum(f860$Nameplate.Capacity..MW.)
  energy <- sum(m3$Net.Generation..Megawatthours.)/sum(f923$Net.Generation..Megawatthours.)
  
  #print these results to the console
  print(paste('Capacity in final dataset: ', round(cap*100, 2), '%', sep = ''))
  print(paste('Energy in final dataset: ', round(energy*100, 2), '%', sep = ''))
  
  #calcualte capacity factor and heat rate 
  m3$cap_fac <- m3$Net.Generation..Megawatthours./(m3$Nameplate.Capacity*8760)
  m3$heat_rate <- m3$Total.Fuel.Consumption.MMBtu*1000/m3$Net.Generation..Megawatthours.
  
  write.csv(x = m3, file = 'merged_2016_f860_f923_data.csv', row.names = F)
  
  ######## add 860M data w/ locational data  ########
  
  #read in F860 data (post removal columns/rows we don't need)
  f860m <- read.csv(F860M_data)
  
  #reduce to the columns wanted
  f860m2 <- f860m[c("Entity.ID", "Plant.ID", "Prime.Mover.Code", "Plant.Name", "Plant.State", "Latitude", "Longitude", "County", "Balancing.Authority.Code")]
  
  #remove duplicated rows in reduced down data
  f860m3 <- f860m2[!duplicated(f860m2),]
  
  #remove the "Prime.Mover.Code" column in order to get less duplication
  f860m4 <- subset(f860m3, select=-c(Prime.Mover.Code))
  
  #remove duplicates from the above
  f860m5 <- f860m4[!duplicated(f860m4),]
  
  #merge the data on utility and plant code
  m4 <- merge(x = m3, y = f860m5, by.x = c('Utility.ID', 'Plant.Code'), by.y = c('Entity.ID', 'Plant.ID'), all.x = F, all.y = F)
  
  #remove any plants thavt have NA values for any columns
  data <- m4[complete.cases(m4),]
  
  #add year back to data
  data$year <- 2016
  
  #write up datasets
  write.csv(data, 'complete_eia_data_2016.csv', row.names = F)
  

  return(data)  
  
}
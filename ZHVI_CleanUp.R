## exploring Zillow's monthly ZHVI Housing Values Index

zhvi <- read.csv("County_Zhvi_AllHomes.csv",header=TRUE,sep=",")

## Data is pretty clean
## Adding FIPS-standardized county names, based on fipstate and fipscty codes

counties <- as.data.frame(read.delim("counties.txt",header = TRUE,","),row.names=NULL)

# re-name fip metrics to make Merging easier
colnames(zhvi)[5] <- "fipstate"
colnames(zhvi)[6] <- "fipscty"

zhvi <- merge(zhvi, counties, by = c("fipstate","fipscty"),all.X=TRUE)

# re-order to bring county name left
zhvi <- zhvi[,c(1,2,246,3:245)]
colnames(zhvi)[3] <- "County Name, State"

write.csv(x=zhvi,file="ZHVI_by_county_by_month.csv",row.names = FALSE)

# create some time-series plots for ZHVI movement

# look at April 1996 to January 2016 for Philadelphia ~ RegionID 3175
require(graphics)

#philly
philly <- unlist(subset(zhvi,subset = zhvi$RegionID == 3175)[9:246])
phillyTS <- ts(philly, start=c(1996,4),end=c(2016,1),frequency = 12)
plot(phillyTS, 
     xlab="Year/Month",
     ylab="Monthly ZHIV ($)",
     main = "Philadelphia, PA ~ ZHVI")

#buffalo
buffalo <- unlist(subset(zhvi,subset=zhvi$RegionID == 1946)[9:246])
buffaloTS <- ts(buffalo, start = c(1996,4),end=c(2016,1),frequency = 12)
plot(buffaloTS, 
     xlab="Year/Month",
     ylab="Monthly ZHIV ($)",
     main = "Buffalo, NY ~ ZHVI")

#utica
utica <- unlist(subset(zhvi,subset=zhvi$RegionID == 1283)[9:246])
uticaTS <- ts(utica, start = c(1996,4),end=c(2016,1),frequency = 12)
plot(uticaTS, 
     xlab="Year/Month",
     ylab="Monthly ZHIV ($)",
     main = "Utica, NY ~ ZHVI")

#detroit
detroit <- unlist(subset(zhvi,subset=zhvi$RegionID == 791)[9:246])
detroitTS <- ts(detroit, start = c(1996,4),end=c(2016,1),frequency = 12)
plot(detroitTS, 
     xlab="Year/Month",
     ylab="Monthly ZHIV ($)",
     main = "Detroit, MI ~ ZHVI")

#waco
waco <- unlist(subset(zhvi,subset=zhvi$RegionID == 2429)[9:246])
wacoTS <- ts(waco, start = c(1996,4),end=c(2016,1),frequency = 12)
plot(wacoTS, 
     xlab="Year/Month",
     ylab="Monthly ZHIV ($)",
     main = "Waco, TX ~ ZHVI")



# work with maps
library(maps)
library(leaflet)
library(ggmap)
library(maptools)
library(htmlwidgets)
library(rgeos)

area = readShapePoly("cb_2014_us_county_500k.shp")
area$NAME = as.character(area$NAME)
area$NAME = gsub("\\.", "", area$NAME)

rates = as.data.frame(read.csv("cleaned_income.csv"))
rates$Name = gsub(" County","", rates$Name)
rates$Name = gsub("\\.","", rates$Name)

# Combine
numeric = c("01","02","04","05","06","08","09","11","12","13",
            "15","16","17","18","19","20","21","22","23","24",
            "25","26","27","28","29","30","31","33","34","35",
            "36","37","38","39","40","41","42","45","46","47",
            "48","49","50","51","53","54","55","56","60","69",
            "72","78","10","32","44","66")
name =     c("AL","AK","AZ","AR","CA","CO","CT","DC","FL","GA",
             "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
             "MA","MI","MN","MS","MO","MT","NE","NH","NJ","NM",
             "NY","NC","ND","OH","OK","OR","PA","SC","SD","TN",
             "TX","UT","VT","VA","WA","WV","WI","WY","AS","MP",
             "PR","VI","DE","NV","RI","GU")

nn = cbind(numeric,name)

for (i in 1:length(area@data$NAME)){
  area@data$OBJECT[i] <-nn[which(nn[,1]==area@data$STATEFP[i]),2]
}

area@data$OBJECTID <- paste0(area@data$OBJECT,', ',area@data$NAME)

sortedNN = data.frame(rep(0,length(unique(area@data$OBJECTID))),
                      rep(0,length(unique(area@data$OBJECTID))))
rownames(sortedNN) = unique(area@data$OBJECTID)
             
for (oid in area@data$OBJECTID ){
  if (length(rates[which(rates$Name == oid),9])==0){
    sortedNN[oid,1] = 0
  }
  else{
    sortedNN[oid,1] = rates[which(rates$Name == oid),9]
  }
  sortedNN[oid,2] = oid
}

area.map = fortify(area, region = "OBJECTID")
area.map$Percent <- rep(0,nrow(area.map))

area@data$Percent <- 1:nrow(area@data)

# Replicate the percentage of signed up
for (aid in unique(area.map$id)){
  if (length(sortedNN[which(sortedNN$rep.0..length.unique.area.data.OBJECTID....1 == aid),1])==0){
    area.map$Percent[which(area.map$id == aid)] = 0
    area@data$Percent[which(area$OBJECTID == aid)] = 0   
  }
  else {
  area.map$Percent[which(area.map$id == aid)] = sortedNN[which(sortedNN$rep.0..length.unique.area.data.OBJECTID....1 == aid),1]
  area@data$Percent[which(area$OBJECTID == aid)] = sortedNN[which(sortedNN$rep.0..length.unique.area.data.OBJECTID....1 == aid),1]
  }
}

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = area.map$Percent
)

mapcounties = map('state', fill = TRUE, plot = FALSE)
mc = leaflet() %>% addTiles() %>%
  addPolygons(data=area,  fillColor = ~pal(area@data$Percent), stroke = FALSE) %>%
  addLegend(pal = pal,
    values = area.map$Percent, 
    position = "bottomright", 
    title = "Income")
    #labFormat = labelFormat(suffix = "%")) 

mc
saveWidget(mc, file="mapcounties.html")


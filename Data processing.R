

library(dplyr)
library(lubridate)

trip <- read.csv("mydata.csv")
station <- read.csv("station.csv")
weather <- read.csv("weather-data-for-san-francisco.csv", stringsAsFactors = FALSE)
colnames(weather)[1] <- "Date"

# which stations are in San Francisco
station.san.fran <- filter(station, station$city == "San Francisco")
head(station.san.fran)

# trips that are end in San Fran
trip.san.fran <- inner_join(trip, station.san.fran, by = c("end_station_id" = "id"))


# some of the rents return imediately therefore we need to remove 
# (ex. trips under 4 minutes and start and end are the same)
trip.san.fran <- filter(trip.san.fran, trip.san.fran$start_station_id != trip.san.fran$end_station_id 
                        | trip.san.fran$duration >= 240)

trip.san.fran.group <- group_by(trip.san.fran, end_station_id, year,month, day) %>% summarise(nTrip = n())
trip.san.fran.group <- left_join(trip.san.fran.group, station, by = c("end_station_id" = "id"))
colnames(trip.san.fran.group)[6] <- "station.name"
head(trip.san.fran.group)
trip.san.fran.group <- as.data.frame(trip.san.fran.group)

# cleaning weather data

a <- weather$Date
a <- strsplit(a, split = "/")
yyyy <- NA
mm <- NA
dd <- NA
for (i in 1:length(weather$Date)) {
  yyyy[i] <- as.numeric(a[[i]][1])
  mm[i] <- as.numeric(a[[i]][2])
  dd[i] <- as.numeric(a[[i]][3])
}

weather$year <- yyyy
weather$month <- mm
weather$day <- dd

# append weather data onto trip.san.fran.group
trip.san.fran.group <- left_join(trip.san.fran.group, weather, by = c("year" = "year", "month" = "month", "day" = "day"))


station_new <- read.csv("station_new.csv", stringsAsFactors = FALSE)
#trip.san.fran.group[1] <- NULL

# append some demgraphic features related to each station form station_new file 
fulldata <- left_join(trip.san.fran.group, station_new, by = c("end_station_id" = "id"))

# remove some redundant features and meaningless features
fulldata[,c("dock_count.x", "city.x", "installation_date", "lat.x", "long.x", "lat.y", "long.y")] <- NULL
colnames(fulldata)[16] <- "dock_count"
colnames(fulldata)[17] <- "city"

# some of the value are 0 in actual but show in blank value (NA) when scratching the data 
fulldata[is.na(fulldata)] <- 0




fulldata$Date <- ymd(fulldata$Date)
q <- mutate(fulldata, DayofWeek = factor(wday(Date,label = TRUE),ordered = FALSE))
c <- data.frame(Sat = ifelse(q$DayofWeek == "¶g¤»",1,0), Sun = ifelse(q$DayofWeek == "¶g¤é",1,0))
fulldata$WeekendOrNot <- ifelse(c$Sat+c$Sun == 1, TRUE, FALSE)
rm(c)






write.csv(fulldata, file = "full data.csv")

















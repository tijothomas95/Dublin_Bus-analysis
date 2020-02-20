                                        #DublinBus.R
#Libraries involved                              
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

#Importing the zip file and unzipping the data :
unzip('DublinBus.zip', exdir = './DublinBus')
list.files('DublinBus')
DUblinBus_data <- list.files('DublinBus/googletransitdublinbusp20130315-1546')
length(DUblinBus_data)


# Reading the files from the DublinBus folder :
agency_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/agency.txt')
calender_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/calendar.txt')
calenderDates_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/calendar_dates.txt')
routes_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/routes.txt')
shapes_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/shapes.txt')
stopTimes_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/stop_times.txt')
stops_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/stops.txt')
transfers_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/transfers.txt')
trips_Data <- read_csv('DublinBus/googletransitdublinbusp20130315-1546/trips.txt')

#Shaping the data using join :
agency_Route <- select( inner_join(agency_Data,routes_Data, by='agency_id'), c(agency_name,route_id,route_type,route_short_name,route_long_name))
trips_Route <- select( inner_join(trips_Data,agency_Route, by='route_id'), c(agency_name, trip_id,route_type,route_short_name,route_long_name, direction_id))
stop_Times <- select( inner_join(stops_Data,stopTimes_Data, by='stop_id'), c(stop_name,trip_id,arrival_time,departure_time,stop_sequence,stop_headsign, shape_dist_traveled))
tripStop_Data <- select( inner_join(trips_Route,stop_Times, by='trip_id'), c(agency_name, trip_id,route_short_name,route_long_name, direction_id,stop_name,arrival_time,departure_time,stop_sequence,stop_headsign,shape_dist_traveled))
names(tripStop_Data)[5] <- 'Travel_Direction'
tripStop_Data$Travel_Direction <- factor(tripStop_Data$Travel_Direction, levels = c(0:1), labels = c('Outward', 'Inward'))

                                    #Analysis of various Route data:


#Analysis of various Routes information
Routes_Info <- group_by(tripStop_Data, route_short_name, route_long_name)
BusRoutes_Info <- summarise(Routes_Info, no_of_stops = max(stop_sequence), total_distance = max(shape_dist_traveled))
View(BusRoutes_Info)

#Total number of Routes
TOtal_number_of_routes <- length(BusRoutes_Info$route_short_name)
TOtal_number_of_routes

#Checking the traffic in the stops
busy_Stops <- group_by(tripStop_Data, stop_name)
busyStops_Info <- summarise(busy_Stops, count =n())
View(busyStops_Info)
length(busyStops_Info$stop_name)

## Most Busy Stop
most_busy_stop <- max(busyStops_Info$count)
most_busy_stop_Info <- filter(busyStops_Info, count == most_busy_stop)
View(most_busy_stop_Info)

##Least Busy Stop
least_busy_stop <- min(busyStops_Info$count)
least_busy_stop_Info<- busyStops_Info[busyStops_Info$count == least_busy_stop,]
View(least_busy_stop_Info)

#Shortest travelling bus based on number of stops
shortest_stops <- min(BusRoutes_Info$no_of_stops)
shortest_Bus_travel <-BusRoutes_Info[BusRoutes_Info$no_of_stops == shortest_stops, ]
names(shortest_Bus_travel)<- c('BusNo','Route Name', 'Total No.of stops', 'Total distance covered')
shortest_Bus_travel

#Longest travelling bus based on number of stops
highest_stops <- max(BusRoutes_Info$no_of_stops)
Longest_Bus_travel <-BusRoutes_Info[BusRoutes_Info$no_of_stops == highest_stops, ]
names(Longest_Bus_travel)<- c('BusNo','Route Name', 'Total No.of stops', 'Total distance covered')
Longest_Bus_travel

#Most Distance travelling bus based on distance covered
most_distance_covered <- max(BusRoutes_Info$total_distance)
Most_distancTravelling_Bus <-BusRoutes_Info[BusRoutes_Info$total_distance == most_distance_covered, ]
names(Most_distancTravelling_Bus)<- c('BusNo','Route Name', 'Total No.of stops', 'Total distance covered')
Most_distancTravelling_Bus

#Least Distance travelling bus based on distance covered
least_distance_covered <- min(BusRoutes_Info$total_distance)
least_distancTravelling_Bus <-BusRoutes_Info[BusRoutes_Info$total_distance == least_distance_covered, ]
names(least_distancTravelling_Bus)<- c('BusNo','Route Name', 'Total No.of stops', 'Total distance covered')
least_distancTravelling_Bus

# Grouping the bus routes information by Travel direction 
busRoutes_DirectionBased <- group_by(tripStop_Data, route_short_name, route_long_name, Travel_Direction)
busRoutes_DirectionBased_Info <- summarise(busRoutes_DirectionBased, no_of_stops = max(stop_sequence), total_distance = max(shape_dist_traveled))
View(busRoutes_DirectionBased_Info)

# Group Outward/Inward Bus routes by stop name and stop sequence
busRoutes_Direction_Stops <- group_by(tripStop_Data, route_short_name, route_long_name, Travel_Direction, stop_name,stop_sequence)
busRoutes_Direction_Stops_Info <- summarise(busRoutes_Direction_Stops, total_distance = max(shape_dist_traveled)
                                       , First_Bus = min(departure_time,na.rm = TRUE), Last_Bus = max(arrival_time,na.rm = TRUE))
View(busRoutes_Direction_Stops_Info)


# Bus route with earliest departure
earliest_departure_bustime <- min(busRoutes_Direction_Stops_Info$First_Bus)
seconds_to_period(earliest_departure_bustime)
earliest_departure_bus <- filter(busRoutes_Direction_Stops_Info, First_Bus == earliest_departure_bustime)
View(earliest_departure_bus)

# Bus route with latest arrival
latest_arrival_bustime <- max(busRoutes_Direction_Stops_Info$Last_Bus)
seconds_to_period(latest_arrival_bustime)
latest_arrival_bus <- filter(busRoutes_Direction_Stops_Info, Last_Bus == latest_arrival_bustime)
View(latest_arrival_bus)

#Checking the total Number of Stopsss
busy_Stops <- group_by(tripStop_Data, stop_name)
busyStops_Info <- summarise(busy_Stops, count =n())
View(busyStops_Info)
length(busyStops_Info$stop_name)

                                        #Task 2

                   ##Inward trip from Adamstown to bachelors Walk

#Checking the number of buses available for Mr.Thomas to reach Bachelors Walk for his work before 11:00 everyday from Adamstown.
#He prefer 25B route for his Inward journey from Adamstown to Bachelors Walk every day so that he can catch that bus from the Adamstown stop which is 100m opposite to his house.
BusNo_25B <- filter(tripStop_Data, route_short_name == '25B')
BusNo_25B_InRoute <- arrange(filter(tripStop_Data, route_short_name == '25B', Travel_Direction=='Inward',stop_name=="Bachelors Walk", arrival_time < period_to_seconds(hms("11:0:0"))),desc(arrival_time))
total_Bus_InRoute <- nrow(BusNo_25B_InRoute)
total_Bus_InRoute

#Scheduled Bus timing for his Inward trip from Adamstown to bachelors Walk
Bus_schedule <- arrange(filter(BusNo_25B, Travel_Direction=='Inward',str_detect(stop_name, 'Adamstown'), arrival_time <= period_to_seconds(hms("11:0:0"))),(arrival_time))
View(Bus_schedule)

# First bus for his Inward trip from Adamstown to Bachelors Walk: 
firstBus_Arrival_In <- BusNo_25B_InRoute[total_Bus_InRoute,]
View(firstBus_Arrival_In)
firstBus_dep_In <- filter(BusNo_25B, trip_id == firstBus_Arrival_In$trip_id, str_detect(stop_name, 'Adamstown'))
firstBus_dep_In1<- firstBus_dep_In[1,]
View(firstBus_dep_In1)


# Last bus for his Inward trip from Adamstown to Bachelors Walk
last_Bus_Arrival <- BusNo_25B_InRoute[1,]
View(last_Bus_Arrival)
Last_Bus_Departure <- filter(BusNo_25B, trip_id == last_Bus_Arrival$trip_id, str_detect(stop_name, 'Adamstown'))
Last_Bus_Departure1_In<-Last_Bus_Departure[1,]
View(Last_Bus_Departure1_In)


# Number of stops he got to pass inorder to reach Bachelors Walk
no_stops <- firstBus_Arrival_In$stop_sequence-firstBus_dep_In1$stop_sequence
no_stops

# Distance he needs to travel
Distance_forWork <- firstBus_Arrival_In$shape_dist_traveled-firstBus_dep_In1$shape_dist_traveled
round(Distance_forWork)

#Total time taken for his Inward journey from Adamstown to bachelors walk
Timetaken_In<- firstBus_Arrival_In$arrival_time - firstBus_dep_In1$departure_time 
Timetaken_In_Min <- seconds_to_period(Timetaken_In)
Timetaken_In_Min

bus_map<- data.frame(firstBus_Arrival_In$route_short_name,no_stops,Distance_forWork,Timetaken_In_Min,firstBus_dep_In1$stop_name,firstBus_Arrival_In$stop_name)
names(bus_map)<-c("Bus_No","Number of Stops","Total distance (m)","Total Time taken (min)","Departure","Arrival")
View(bus_map)


                #Outward trip back to home from Wellington Quay to Adamstown

#Checking the number of buses available for Mr.Thomas to reach back home at Adamstown before 20:00 everyday from Wellington Quay.
BusNo_25B <- filter(tripStop_Data, route_short_name == '25B')
BusNo_25B_OutRoute <- arrange(filter(tripStop_Data, route_short_name == '25B', Travel_Direction=='Outward',stop_name=="Adamstown", arrival_time < period_to_seconds(hms("20:0:0"))),desc(arrival_time))
total_Bus_OutRoute <- nrow(BusNo_25B_OutRoute)
total_Bus_OutRoute

#Scheduled Bus timing for his Outward trip from Wellington Quay to Adamstown
Bus_schedule <- arrange(filter(BusNo_25B, Travel_Direction=='Outward',str_detect(stop_name, 'Wellington Quay'), arrival_time <= period_to_seconds(hms("11:0:0"))),(arrival_time))
View(Bus_schedule)

# First bus for his Outward trip from Wellington Quay to Adamstown 
firstBus_Arr_Out <- BusNo_25B_OutRoute[total_Bus_OutRoute,]
View(firstBus_Arr_Out)
firstBus_Dep_Out <- filter(BusNo_25B, trip_id == firstBus_Arr_Out$trip_id, str_detect(stop_name, 'Wellington Quay'))
firstBus_Dep_Out1<- firstBus_Dep_Out[1,]
View(firstBus_Dep_Out1)


# Last bus for his Outward trip from Wellington Quay to Adamstown
last_Bus_Arrival <- BusNo_25B_OutRoute[1,]
View(last_Bus_Arrival)
Last_Bus_Departure_Out <- filter(BusNo_25B, trip_id == last_Bus_Arrival$trip_id, str_detect(stop_name, 'Wellington Quay'))
Last_Bus_Departure1_Out<-Last_Bus_Departure_Out[1,]
View(Last_Bus_Departure1_Out)


# Number of stops he got to pass inorder to reach Adamstown
no_stops <- firstBus_Arr_Out$stop_sequence-firstBus_Dep_Out1$stop_sequence
no_stops

# Distance he needs to travel
Distance_forWork <- firstBus_Arr_Out$shape_dist_traveled-firstBus_Dep_Out1$shape_dist_traveled
Distance_forWork

#Total time taken for his Outward journey from Wellington Quay to Adamstown
Timetaken_In<- firstBus_Arr_Out$arrival_time - firstBus_Dep_Out1$departure_time 
Timetaken_In_Min <- seconds_to_period(Timetaken_In)
Timetaken_In_Min

bus_map<- data.frame(firstBus_Arr_Out$route_short_name,no_stops,Distance_forWork,Timetaken_In_Min,firstBus_Dep_Out1$stop_name,firstBus_Arr_Out$stop_name)
names(bus_map)<-c("Bus_No","Number of Stops","Total distance (m)","Total Time taken (min)","Departure","Arrival")
View(bus_map)


#Plot Visualisation
tripID <- arrange(filter(BusNo_25B, (Travel_Direction == 'Inward'&trip_id == Last_Bus_Departure1_In$trip_id)), arrival_time)[1,]$trip_id
qplot(data=filter(BusNo_25B, (trip_id == Last_Bus_Departure1_Out$trip_id | trip_id == tripID)), arrival_time, shape_dist_traveled, color=Travel_Direction, geom = 'boxplot', main = 'Route 25 Bus: Plot for Time-Distance Comparison', ylab = 'Distance Covered', xlab = 'Time')



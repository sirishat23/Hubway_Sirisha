install.packages("readxl")
getwd()
library(readxl)
hubway_trips <- read_excel("downloads/hubway_trips.xlsx")

# 1. This file is quite large. (And we've already removed move than 75% of the data!) How many rows and columns does it have?

length(unique(hubway_trips$seq_id))
length(colnames(hubway_trips))

#(or)

#For row count
nrow(hubway_trips)  350615

#For column count
ncol(hubway_trips)  13

#2. How many unique user zip codes are in this dataset?
length(unique(hubway_trips$zip_code))  375

#3. How many unique bicycles are in this dataset?
length(unique(hubway_trips$bike_nr))  882

#4. Calculate the count of rides for each unique bicycle. 

rides <- table(hubway_trips$bike_nr)
View(rides)

#5. Which bicycle is ridden most frequently?

which.max(rides)   B00401 400 


#6. Which bicycle is ridden least frequently?

which.min(rides)  T01093 676
 

#7. Calculate the total duration of all rides for each bicycle. Hint: tapply() or aggregate()

#totdur <- tapply(hubway_trips$bike_nr, hubway_trips$duration, FUN = sum)

dur_tot <- aggregate(duration~bike_nr, hubway_trips, sum)
View(dur_tot)

#8. Which bicycle has been ridden for the longest total duration in this dataset? Shortest total duration?

dur_tot[which.max(dur_tot$duration),]  B00585

bike_nr duration
582  B00585  8704314

dur_tot[which.min(dur_tot$duration),] T01380 

bike_nr duration
846  T01380     1331

#9. Consider only trips on the bicycle that has been ridden for the longest duration, which station 
#is its most frequent end station? Which station is its most frequent start station? Return the station ids.


maxbike <- dur_tot[which.max(dur_tot$duration),]
maxbike

maxbike_data <- subset(hubway_trips, bike_nr == "B00585")

station_freq <- data.frame(table(maxbike_data$strt_statn))
station_freq[which.max(station_freq$Freq),]   22

Var1 Freq
8   22    5

station_freq1 <- data.frame(table(maxbike_data$end_statn))
station_freq1[which.max(station_freq1$Freq),]   22

Var1 Freq
6   22    5

#10. Look up the name of the above station ids in the stations data frame. What are the names of the most 
# frequent start and end stations for this bicycle?
  
#Most frequent start station
Mostfreq_strtstn <- subset(hubway, id == 22)

South Station - 700 Atlantic Ave.

#Most frequent end station
Mostfreq_endstn <- subset(hubway, id == 22)

South Station - 700 Atlantic Ave.	

#R – Hubway Lab – Part II

hubway <- read_excel("downloads/hubway_stations.xlsx")

# 1. Use merge (twice) to append the appropriate station names to the trips data frame. 
# Add a column for the starting station and the ending station. See ?merge.

#A. Merge the trip and station data

mergedData <- merge(hubway_trips, hubway,
                    by.x="strt_statn", by.y="id")

mergedData1 <- merge(mergedData, hubway, by.x =  "end_statn", by.y = "id")


#2. Use names() or colnames() to clean up the column names in the merged data frame.

# delete multiple columns in r 
# delete column in R by mapping Null value to them

#colnames(mergedData1)<- NULL

# 3. Which station is most frequently used as a starting station?

statn_freq <- data.frame(table(mergedData1$strt_statn))
statn_freq[which.max(statn_freq$Freq),]   22

Var1  Freq
20   22 20768
  
# 4. Which station is most frequently used as an ending station?

statn_freq1 <- data.frame(table(mergedData1$end_statn))
statn_freq1[which.max(statn_freq1$Freq),]  22

Var1  Freq
20   22 21022


#5. What is the name of the starting station with the longest average trip duration? 
  #Hint: tapply or aggregate

long_tot <- aggregate(duration~station.x, mergedData1, sum)
View(long_tot)

long_tot[which.max(long_tot$duration),]

station.x duration
81 South Station - 700 Atlantic Ave. 19578019


# 6. What is the name of the starting station with the shortest average trip duration? 
 
long_tot[which.min(long_tot$duration),]

station.x duration
54 JFK / UMASS Station    32867

  
# 7. What is the name of the ending station with the longest and shortest average trip duration?

long_tot1 <- aggregate(duration~station.y, mergedData1, sum)
long_tot1[which.max(long_tot1$duration),]

station.y duration
84 TD Garden - Legends Way 17145645

long_tot1[which.min(long_tot1$duration),]

station.y duration
54 JFK / UMASS Station    36357

# 8. Which user zipcode is associated most frequently with trips ending at the station with the longest average trip duration?

zip_dur <- subset(mergedData1, station.y == "TD Garden - Legends Way" )

zip_freq <- data.frame(table(zip_dur$zip_code))
zip_freq[which.max(zip_freq$Freq),]  01915

Var1 Freq
66 01915 1097

#  9. Create a table showing the number of trips by gender. Plot this table using barplot. The argument 
# for barplot is the table itself. What percent of total trips were completed by males?

trips_gen <- data.frame(table(mergedData1$gender))

# The most basic barplot you can do:
#barplot(height=data$value, names=data$name)

barplot (height=trips_gen$Freq, names.arg=c('Female','Male'), 
         xlab = "Gender", ylab ="Number of Trips", main = "HUBWAY",
         space=c(1,1), width = c(2,2), col=c("darkblue","lightcyan"))

# What percent of total trips were completed by males?

#margin.table(mytable, 1) # A frequencies (summed over B)
#margin.table(mytable, 2) # B frequencies (summed over A)

#prop.table(mytable) # cell percentages
#prop.table(mytable, 1) # row percentages
#prop.table(mytable, 2) # column percentages

round(100*prop.table(trips_gen$Freq),digits=2)   Males = 75.18

24.82 75.18

# 10. Use hist with the vector for trip duration as the only argument to plot the histogram of trip 
#durations. What do you notice?

trip_duration <- (mergedData1$duration)
hist(trip_duration)

#We see only a single bar, as the data is diverse ranging from 0 to 8000,000. The X axis has values like 0e+00, 4e+06, 8e+06.

# 11. Use hist with the vector for trip duration, considering only trips less than an hour to plot the 
# histogram of trip durations under an hour.

trips_hrdur <- subset(mergedData1, duration <3600)
hist(trips_hrdur$duration, main = "Histogram for Trip Duration less than an hour",
     xlab ="Duration", border = "blue", col = "yellow")
















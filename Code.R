# Installing required packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")

# Uploading packages
library(tidyverse)
library(ggplot2)
library(lubridate)

# Checking column names
colnames(jan)
colnames(feb)
colnames(march)
colnames(april)
colnames(may)
colnames(june)
colnames(july)
colnames(aug)
colnames(sep)
colnames(oct)
colnames(nov)
colnames(dec)

# Joining all the datasets as one single dataset for analysis process
all_rides <- rbind(jan, feb, march, april, may, june, july, aug, sep,
                   oct, nov, dec)

# Inspecting newly created data set
head(all_rides)
summary(all_rides)
str(all_rides)
nrow(all_rides)
tail(all_rides)

# Adding columns to the data
all_rides$started_at <- ymd_hms(all_rides$started_at)
all_rides$month <- format(as.Date(all_rides$started_at), "%m")
all_rides$day <- format(as.Date(all_rides$started_at), "%d")
all_rides$year <- format(as.Date(all_rides$started_at), "%y")
all_rides$start_time <- format(all_rides$started_at, format = "%H")
all_rides$week_day <- wday(all_rides$started_at, label = TRUE)

# Adding a column of trip duration using difftime and convert it to numeric type
all_rides$ended_at <- ymd_hms(all_rides$ended_at)
all_rides$trip_duration <- difftime(all_rides$ended_at, all_rides$started_at)
all_rides$trip_duration <- as.numeric(all_rides$trip_duration)

# Checking if we got any negative or zero values in trip duration
sum(all_rides$trip_duration <= 0) # 531 negative or zero values found

# Removing all negative and zero values 
all_rides <- all_rides[!all_rides$trip_duration <= 0, ]

# Inspecting if there are still any negative or zero values available
sum(all_rides$trip_duration <= 0 ) # all negative and zero values is removed 

# As we are comparing between member riders and casual riders let us inspect column "member_casual"
# Checking for unique names in the column
unique(all_rides$member_casual) # only "member" and "casual" 

# Descriptive analysis of the data
# A summary of trip_duration for the types of riders
summary(all_rides$trip_duration) # min : 1 secs, 1stQu. : 349secs, median : 617secs, mean : 1167secs, 3rd Qu. : 1108secs, max : 2483235 secs

# Comparing rides between both types of riders
aggregate(all_rides$trip_duration ~ all_rides$member_casual, FUN = mean) #   1      casual               1748.9000
                                                                         #   2      member                762.8915

aggregate(all_rides$trip_duration ~ all_rides$member_casual, FUN = median) # 1      casual                     780
                                                                           # 2      member                     530

aggregate(all_rides$trip_duration ~ all_rides$member_casual, FUN = max) # 1         casual                 2483235
                                                                        # 2         member                   93594

aggregate(all_rides$trip_duration ~ all_rides$member_casual, FUN = min) # 1         casual                       1
                                                                        # 2         member                       1

# Average ride duration on week day
aggregate(all_rides$trip_duration ~ all_rides$member_casual + all_rides$week_day, FUN = mean)

# Inspecting how many types of ride able_bikes are available
unique(all_rides$rideable_type) # electric_bike, classic_bike and docked_bike

# Arranging the data frame by time_duration
arrange(all_rides, trip_duration)

# Descriptive analysis
#grouped by member_casual and week_day
all_rides %>% 
  group_by(member_casual, week_day) %>% 
  summarize(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, week_day)

# grouped by member_casual and month
all_rides %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month)

# grouped by type of bikes used for rides
all_rides %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, rideable_type)

# average ride length by user type and week day
all_rides %>% 
  group_by(member_casual, week_day) %>% 
  summarize(average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, week_day)

### GRAPHS
# Visualization - Monthly rides by user type
all_rides %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = number_of_rides,fill = member_casual)) +
  geom_col(position = "dodge") +
  xlab("Month") +
  ylab("Rides") +
  ggtitle("Monthly Rides by Month") 
 
# Visualization of riders by user type and week day
all_rides %>% 
  group_by(member_casual, week_day) %>% 
  summarize(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, week_day) %>% 
  ggplot(aes(x = week_day, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")+
  xlab("Week Day")+
  ylab("Number of Rides")+
  ggtitle("Number of Rides by User and Weekday")

# Visualization of user and ride able type
all_rides %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x= member_casual, y = number_of_rides, fill = rideable_type))+
  geom_col(position = "dodge")+
  xlab("Rideable Type")+
  ylab("Number of Rides")+
  ggtitle("Types of bikes Users prefer")

# Visualization of average duration of bikes used
all_rides %>% 
  group_by(rideable_type, week_day, member_casual) %>% 
  summarize(average_duration = mean(trip_duration)) %>% 
  arrange(rideable_type, week_day, member_casual) %>% 
  ggplot(aes(x = week_day, y = average_duration, fill = rideable_type))+
  geom_col(position = "dodge")+
  xlab("Week Day")+
  ylab("Average Ride Duration")+
  ggtitle("Average duration of Types of bikes used")

# Visualization Average ride length by user type and weekday
all_rides %>% 
  group_by(member_casual, week_day) %>% 
  summarize(average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, week_day) %>% 
  ggplot(aes(x = week_day, y = average_duration, fill = member_casual))+
  geom_col(position = "dodge")+
  xlab("Week Day")+
  ylab("Average trip Duration")+
  ggtitle("Avearge Ride Length By User and Weekday")



# Additional descriptive analysis
# difference between ride lenght of member riders and casual riders
all_rides %>% 
  group_by(member_casual) %>% 
  summarize(count = n(), total_pop = nrow(all_rides), share = count/total_pop*100,
            mean(trip_duration))

# Visualization during working days 
all_rides %>% 
  filter(week_day %in% c("Mon","Tue", "Wed", "Thu", "Fri")) %>% 
  group_by(member_casual, start_time) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, start_time) %>% 
  ggplot(aes(x = start_time, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge") +
  xlab("Hour")+
  ylab("Rides")+ 
  ggtitle("Hourly Rides")

# Hourly Average Rides on working days
all_rides %>% 
  filter(week_day %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  group_by(member_casual, start_time) %>% 
  summarize(avg_ride_length = mean(trip_duration)) %>% 
  arrange(member_casual, start_time) %>%
  ggplot(aes(x = start_time, y = avg_ride_length, fill = member_casual))+
  geom_col(position = "dodge") + xlab("Hour") + ylab("Average Ride Lenght") +
  ggtitle(" Hourly Average Ride Length on Working Days")

# Rides during Week ends
all_rides %>% 
  filter(week_day %in% c("Sat", "Sun")) %>% 
  group_by(member_casual, start_time) %>% 
  summarize(num_of_rides = n()) %>% 
  arrange(member_casual, start_time) %>% 
  ggplot(aes(x = start_time, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + xlab("Hour") + ylab("Rides") +
  ggtitle("Number of Rides on Weekends")

# Average hourly ride length on weekends
all_rides %>% 
  filter(week_day %in% c("Sat", "Sun")) %>% 
  group_by(member_casual, start_time) %>% 
  summarize(avg_ride_length = mean(trip_duration)) %>% 
  arrange(member_casual, start_time) %>% 
  ggplot(aes(x = start_time, y = avg_ride_length, fill = member_casual))+
  geom_col(position = "dodge")+ xlab("Hour") + ylab("Time in Secs")+
  ggtitle("Average hourly Ride length on Weekends")

# Rides based on starting point
df_location_start <- all_rides %>% 
  group_by(member_casual, start_lat, start_lng, .groups = "keep") %>% 
  summarize(total_count = n()) %>% 
  ungroup()
df_location_start <- df_location_start %>% 
  select(-.groups)

# Scatter plot
df_location_start %>% 
  drop_na() %>% 
  ggplot()+
  geom_point(mapping = aes(x = start_lng, y= start_lat, group = member_casual, color = member_casual,
                           size = total_count, alpha = .2))+
  xlim(-87.9, -87.5)+
  ylim(41.64, 42.1)+
  facet_wrap(~member_casual)+
  labs(title = "Cyclistic Trip Destiny", subtitle = "Chicago Start Station", x = "Longitude",
       y ="Latitude")



























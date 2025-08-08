# installing relevant packages
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("readr")
library(readr)
install.packages("janitor")
library(janitor)
install.packages("ggmap")
library(ggmap)
install.packages("geosphere")
library(geosphere)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)


# importing data
data_Q1 <- read_csv("original_dataset/Divvy_Trips_2019_Q1.csv")
data_Q2 <- read_csv("original_dataset/Divvy_Trips_2019_Q2.csv")
data_Q3 <- read_csv("original_dataset/Divvy_Trips_2019_Q3.csv")
data_Q4 <- read_csv("original_dataset/Divvy_Trips_2019_Q4.csv")

#checking consistent column names and structures
colnames(data_Q1)
colnames(data_Q2)
colnames(data_Q3)
colnames(data_Q4)

str(data_Q1)
str(data_Q2)
str(data_Q3)
str(data_Q4)

# changing Q2 column names
colnames(data_Q2) <- c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear")

# merging datasets
trip_data <- bind_rows(data_Q1, data_Q2, data_Q3, data_Q4)

# viewing and confirming merged datasets
View(trip_data)
colnames(trip_data)
str(trip_data)
summary(trip_data)

# saving merged data
my_documents_path <- file.path(Sys.getenv("Howard"), "Desktop", "Data Analytics Case Study", "trip_data.csv")
write_csv(trip_data, "trip_data.csv")

# filtering out trips that are negative and more than 24 hours
trip_data_clean <- trip_data[!(trip_data$tripduration <= 0),]
View(trip_data_clean)
trip_data_cleaned_2 <- trip_data_clean[!(trip_data_clean$tripduration >= 86400),]
View(trip_data_cleaned_2)

#filtering out rows that have na values
trip_data_cleaned_2[rowSums(is.na(trip_data_cleaned_2)) != ncol(trip_data_cleaned_2),]


trip_data_cleaned_2 <- trip_data_cleaned_2 %>% 
  distinct() %>% # removing duplicates
  mutate(tripduration = tripduration/60) %>% # converting trip duration from seconds to minutes
  mutate(year = format(as.Date(start_time), "%Y")) %>% # adding year column
  mutate(month = format(as.Date(start_time), "%B")) %>% # adding month column
  mutate(date = format(as.Date(start_time), "%d")) %>% # adding date column
  mutate(day_of_week = format(as.Date(start_time), "%A")) %>% # adding day of week column
  mutate(time = strftime(start_time, "%H")) %>% # adding time column
  mutate(age=2019 - birthyear) %>% 
  mutate(
    month_num = month(start_time),  # extract numeric month
    season = case_when( # assigning numeric month to season according to Northern Hemisphere
      month_num %in% c(12, 1, 2)  ~ "Winter",
      month_num %in% c(3, 4, 5)   ~ "Spring",
      month_num %in% c(6, 7, 8)   ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Autumn"
    )
  ) %>% 
  select(-month_num) # removing numeric month column

# checking that all Null, "", 0 rows have been remove. If so, it should return a some of 0

sum(apply(trip_data_cleaned_2, 1, function(row) all(is.na(row) | row == "" | row == 0)))
str(trip_data_cleaned_2)

is_empty_row <- apply(trip_data_cleaned_2, 1, function(row) {
  row_char <- as.character(row)
  all(is.na(row) | row_char == "" | row_char == "0" | tolower(row_char) %in% c("na", "null"))
})

sum(is_empty_row)

# removing outliers from data. All data outside of 3 standard deviation are removed. 
no_outliers <- trip_data_cleaned_2[sapply(trip_data_cleaned_2, is.numeric)] # identifies columns that are numeric to use z score
z_scores <- scale(trip_data_cleaned_2$tripduration) # normalising the data to standard normal
no_outliers_data <- abs(z_scores) <= 3 # creating dataset that is less than 3 standard deviations
trip_data_cleaned_3 <- trip_data_cleaned_2[no_outliers_data, ] # new table without outliers
View(trip_data_cleaned_3)

# calculating percentage of new table without outliers to original dataset to see what proportion of data we will be working with
processed_data <- count(trip_data_cleaned_3)
unprocessed_data <- count(trip_data)
processed_data_percentage <- (processed_data/unprocessed_data) * 100
processed_data_percentage

#calculating descriptive statistics for the data
trip_data_cleaned_3 %>% 
  summarise(average_tripduration=mean(tripduration), median_tripduration=median(tripduration),
            max_tripduration=max(tripduration), min_tripduration=min(tripduration))

ggplot(data=trip_data_cleaned_3) + geom_histogram(mapping = aes(x=tripduration),
          fill = "blue",color="black", bins = 50) + labs(title = "Distribution of tripduration", x="tripduration (minutes)",
                                                         y="Frequency")

# counting the number of trips for each usertype
usertype_count <- trip_data_cleaned_3 %>% 
  count(usertype)

# calculating and labeling the percentage of trips taken by each member type for pie chart
usertype_count <- usertype_count %>% 
  mutate(percent = round(n / sum(n) * 100,1),
         label = paste0(usertype, "\n", percent, "%"), 
         ypos = cumsum(n) - 0.5 * n)

# creating pie chart 
ggplot(usertype_count, aes(x = "", y = n, fill = usertype)) + # initially creating bar chart
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") + #convert bar chart to pie chart
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  labs(title = "User Type Distribution") +
  scale_fill_brewer(palette = "Set1")

write_csv(trip_data_cleaned_3, "trip_data_3.csv") # saving data without outliers

# calculating descriptive statistics for each usertype
trip_data_cleaned_3 %>% 
  group_by(usertype) %>% 
  summarise(average_tripduration=mean(tripduration), median_tripduration=median(tripduration))

# calculating total rides on a monthly basis

trip_data_cleaned_3$month <- factor(trip_data_cleaned_3$month, levels = c(
  "January", "February", "March", "April", "May", "June", 
  "July", "August", "September", "October", "November", "December"
))

# Prepare the data with percentages
rides_summary <- trip_data_cleaned_3 %>%
  group_by(usertype, month) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  mutate(total_rides = sum(number_of_rides),
         percent_of_total = number_of_rides / total_rides * 100)  # percentage within each usertype



# Plot with percentage labels
ggplot(rides_summary, aes(x = month, y = number_of_rides, fill = usertype)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = paste0(round(percent_of_total, 1), "%")),
            position = position_dodge(width = 0.5),
            vjust = -0.5, size = 2.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Total Rides by Subscribers and Customers vs. Month",
       x = "Month",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# calculating total rides on a seasonal basis

# Prepare the data with percentages
rides_summary <- trip_data_cleaned_3 %>%
  group_by(usertype, season) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  mutate(total_rides = sum(number_of_rides),
         percent_of_total = number_of_rides / total_rides * 100)  # percentage within each usertype

# Plot with percentage labels
ggplot(rides_summary, aes(x = season, y = number_of_rides, fill = usertype)) +
  geom_col(width = 0.8, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(percent_of_total, 1), "%")),
            position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Total Rides by Subscribers and Customers vs. Season",
       x = "Season",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# calculating total rides on a daily basis


rides_summary$day_of_week <- factor(rides_summary$day_of_week,
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday"))

# Prepare the data with percentages
rides_summary <- trip_data_cleaned_3 %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  mutate(total_rides = sum(number_of_rides),
         percent_of_total = number_of_rides / total_rides * 100)  # percentage within each usertype



# Plot with percentage labels
ggplot(rides_summary, aes(x = day_of_week, y = number_of_rides, fill = usertype)) +
  geom_col(width = 0.8, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(percent_of_total, 1), "%")),
            position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Total Rides by Subscribers and Customers vs. Day of Week",
       x = "Day of Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# calculating total rides on a time basis
# Prepare the data with percentages
rides_summary <- trip_data_cleaned_3 %>%
  group_by(usertype, time) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  mutate(total_rides = sum(number_of_rides),
         percent_of_total = number_of_rides / total_rides * 100)  # percentage within each usertype


# Plot with percentage labels
ggplot(rides_summary, aes(x = time, y = number_of_rides, fill = usertype)) +
  geom_col(width = 0.8, position = position_dodge(width = 0.8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Total Rides by Subscribers and Customers vs. Time",
       x = "Time",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# calculating total rides on a time basis by day of week
trip_data_cleaned_3$day_of_week <- factor(trip_data_cleaned_3$day_of_week,
                                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                     "Friday", "Saturday", "Sunday"),
                                          ordered = TRUE)
trip_data_cleaned_3 %>%
  ggplot(aes(time, fill=usertype)) +
  geom_bar() +
  labs(x="Hour of the day", y="Number of Rides",title="Total rides per hour by day of the week") +
  facet_wrap(~ day_of_week) + theme_minimal() + theme(axis.text.x = element_blank())





# calculating median monthly trip duration 

# Ensure months are in order
trip_data_cleaned_3$month <- factor(trip_data_cleaned_3$month, levels = c(
  "January", "February", "March", "April", "May", "June", 
  "July", "August", "September", "October", "November", "December"
))


trip_data_cleaned_3 %>%  
  group_by(usertype, month) %>% 
  summarise(median_tripduration = median(tripduration), .groups="drop") %>%
  ggplot(aes(x = month, y = median_tripduration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Median Trip Duration by Subscribers and Customers  Vs. Month (minutes)", x="Month", y="Median Trip Duration") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,hjust = 1))

# calculating median seasonal trip duration 

trip_data_cleaned_3 %>%  
  group_by(usertype, season) %>% 
  summarise(median_tripduration = median(tripduration), .groups="drop") %>%
  ggplot(aes(x = season, y = median_tripduration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Median Trip Duration by Subscribers and Customers  Vs. Season (minutes)", x="Season", y="Median Trip Duration") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,hjust = 1))


# calculating median daily trip duration 

trip_data_cleaned_3 %>%  
  group_by(usertype, day_of_week) %>% 
  summarise(median_tripduration = median(tripduration), .groups="drop") %>%
  ggplot(aes(x = day_of_week, y = median_tripduration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Median Trip Duration by Subscribers and Customers  Vs. Day of Week (minutes)", x="Day of Week", y="Median Trip Duration") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,hjust = 1))


# calculating median trip duration by time 

trip_data_cleaned_3 %>%  
  group_by(usertype, time) %>% 
  summarise(median_tripduration = median(tripduration), .groups="drop") %>%
  ggplot(aes(x = time, y = median_tripduration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Median Trip Duration by Subscribers and Customers  Vs. Time (minutes)", x="Time", y="Median Trip Duration") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,hjust = 1))


# calculating median trip duration by time and day of week 

trip_data_cleaned_3$day_of_week <- factor(trip_data_cleaned_3$day_of_week,
                                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                     "Friday", "Saturday", "Sunday"),
                                          ordered = TRUE)

median_duration <- trip_data_cleaned_3 %>%
  group_by(usertype, day_of_week, time,) %>%
  summarise(median_duration = median(tripduration, na.rm = TRUE), .groups = "drop")


ggplot(data = median_duration, aes(x = time, y = median_duration, fill = usertype)) +
  geom_col(position = "stack") +
  facet_wrap(~ day_of_week) +
  labs(
    title = "Median Trip Duration per Hour by Day of the Week (minutes)",
    x = "Hour of the Day",
    y = "Median Trip Duration",
    fill = "User Type"
  ) +
  theme_minimal() + theme(axis.text.x = element_blank())

# Age analysis

#age distribution
trip_data_cleaned_3 <- trip_data_cleaned_3 %>%
  filter(!is.na(age), age > 10, age < 100)

summary(trip_data_cleaned_3$age)

ggplot(trip_data_cleaned_3, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Riders",
       x = "Age",
       y = "Count") +
  theme_minimal()

# age distribution by usertype

# filter young and old ages
trip_data_cleaned_3 <- trip_data_cleaned_3 %>%
  filter(!is.na(age), age > 10, age < 100)

summary(trip_data_cleaned_3$age)

ggplot(trip_data_cleaned_3, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Riders",
       x = "Age",
       y = "Count") +
  theme_minimal()

# age distribution by usertype

# create age ranges
trip_data_cleaned_3 <- trip_data_cleaned_3 %>%
  mutate(age_group = cut(
    age,
    breaks = c(15, 25, 35, 45, 60, 100),
    labels = c("15–25", "26–35", "36–45", "46–60", "61+"),
    right = TRUE
  ))

age_group_summary <- trip_data_cleaned_3 %>%
  group_by(age_group) %>%
  summarise(trips = n(), .groups = "drop")

print(age_group_summary)

# age distribution by usertype
ggplot(trip_data_cleaned_3, aes(x = age, fill = usertype)) +
  geom_histogram(binwidth = 5, position = "stack", color = "black") +
  labs(title = "Age Distribution by User Type",
       x = "Age",
       y = "Count") +
  theme_minimal()

# age distribution by usertype and age group
age_usertype_summary <- trip_data_cleaned_3 %>%
  group_by(age_group, usertype) %>%
  summarise(trips = n(), .groups = "drop")

ggplot(age_usertype_summary, aes(x = age_group, y = trips, fill = usertype)) +
  geom_col(position = "dodge") +
  labs(title = "Trips by Age Group and User Type",
       x = "Age Group",
       y = "Number of Trips",
       fill = "User Type") +
  theme_minimal()



# Identifying top 10 stations by usertype

top_stations_by_usertype <- trip_data_cleaned_3 %>%
  group_by(usertype, from_station_name) %>% 
  summarise(number_of_rides = n(), .groups = "drop_last") %>%
  slice_max(order_by = number_of_rides, n = 10) %>%
  arrange(usertype,desc(number_of_rides)) %>% 
  ungroup()

View(top_stations_by_usertype)
# Show top stations for customers

top_stations_by_usertype_customer <- top_stations_by_usertype %>% 
  filter(usertype == "Customer")

# plot customer top 10 stations

ggplot(top_stations_by_usertype_customer, aes(
  x = fct_reorder(from_station_name, number_of_rides),  # orders by ride count
  y = number_of_rides,
  fill = number_of_rides
)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  # makes the bars horizontal
  facet_wrap(~ usertype, scales = "free_y") +  # separate chart per usertype
  labs(
    title = "Top 10 Stations by Number of Rides per User Type",
    x = "Station Name",
    y = "Number of Rides"
  ) + scale_fill_gradient(low = "beige", high = "blue") +  # Orange to red gradient
  theme_minimal()


# Show top 10 station for subscribers

top_stations_by_usertype_subscriberr <- top_stations_by_usertype %>% 
  filter(usertype == "Subscriber")

# plot customer top 10 stations

ggplot(top_stations_by_usertype_subscriberr, aes(
  x = fct_reorder(from_station_name, number_of_rides),  # orders by ride count
  y = number_of_rides,
  fill = number_of_rides
)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  # makes the bars horizontal
  facet_wrap(~ usertype, scales = "free_y") +  # separate chart per usertype
  labs(
    title = "Top 10 Stations by Number of Rides per User Type",
    x = "Station Name",
    y = "Number of Rides"
  ) + scale_fill_gradient(low = "beige", high = "blue") +  # Orange to red gradient
  theme_minimal()


top_customer_routes <- trip_data_cleaned_3 %>%
  filter(usertype == "Customer") %>%
  group_by(from_station_name, to_station_name) %>%
  summarise(number_of_trips = n(), .groups = "drop") %>%
  arrange(desc(number_of_trips)) %>%
  slice_head(n = 10)

View(top_customer_routes)

top_subscriber_routes <- trip_data_cleaned_3 %>%
  filter(usertype == "Subscriber") %>%
  group_by(from_station_name, to_station_name) %>%
  summarise(number_of_trips = n(), .groups = "drop") %>%
  arrange(desc(number_of_trips)) %>%
  slice_head(n = 10)

View(top_subscriber_routes)

# mapping top route for each subscriber 

register_google("AIzaSyBE-XP366EE7-ij9kMohsY-rE-UeGKzzqE")

top_route_by_usertype <- trip_data_cleaned_3 %>%
  group_by(usertype, from_station_name, to_station_name) %>%
  summarise(number_of_trips = n(), .groups = "drop") %>%
  arrange(usertype, desc(number_of_trips)) %>%
  group_by(usertype) %>%
  slice_head(n = 1) %>%
  ungroup()

# top route by age group and usertype 

# filter old and young age and create age groups
trip_data_cleaned_3 <- trip_data_cleaned_3 %>%
  mutate(age = 2019 - birthyear) %>%
  filter(!is.na(age), age >= 10, age <= 100) %>%
  mutate(age_group = cut(
    age,
    breaks = c(15, 25, 35, 45, 60, 100),
    labels = c("15–25", "26–35", "36–45", "46–60", "61+"),
    right = TRUE
  ))

# identify top route per usertype
top_routes <- trip_data_cleaned_3 %>%
  group_by(usertype, from_station_name, to_station_name) %>%
  summarise(number_of_trips = n(), .groups = "drop") %>%
  arrange(usertype, desc(number_of_trips)) %>%
  group_by(usertype) %>%
  slice_head(n = 1) %>%
  ungroup()

# join top route per usertype and age group
top_routes_by_age <- trip_data_cleaned_3 %>%
  inner_join(top_routes, by = c("usertype", "from_station_name", "to_station_name")) %>%
  group_by(usertype, age_group) %>%
  summarise(route_trips = n(), .groups = "drop") %>%
  arrange(usertype, desc(route_trips))

# plotting top route per usertype and age group
ggplot(top_routes_by_age, aes(x = age_group, y = route_trips, fill = usertype)) +
  geom_col(position = "dodge") + facet_wrap(~ usertype)
labs(
  title = "Top Route Usage by Age Group and Usertype",
  x = "Age Group",
  y = "Number of Trips"
) +
  theme_minimal()


# Manually create a tibble of cleaned locations
stations <- tibble::tibble(
  station_name = c(
    "Streeter Dr & Grand Ave",
    "Lake Shore Dr & Monroe St",
    "Canal St & Adams St",
    "Michigan Ave & Washington St"
  ),
  address = c(
    "Streeter Dr and Grand Ave, Chicago, IL",
    "Lake Shore Dr and Monroe St, Chicago, IL",
    "Canal St and Adams St, Chicago, IL",
    "Michigan Ave and Washington St, Chicago, IL"
  )
)

# Geocode
geocoded_stations <- stations %>%
  mutate_geocode(address, output = "latlon", source = "google")

routes <- tibble(
  usertype = c("Customer", "Subscriber"),
  from = c("Lake Shore Dr & Monroe St", "Canal St & Adams St"),
  to = c("Streeter Dr & Grand Ave", "Michigan Ave & Washington St")
)

routes <- routes %>%
  left_join(geocoded_stations, by = c("from" = "station_name")) %>%
  rename(from_lat = lat, from_lon = lon) %>%
  left_join(geocoded_stations, by = c("to" = "station_name")) %>%
  rename(to_lat = lat, to_lon = lon)


# Get a basemap centered on Chicago
chicago_map <- get_map(location = "Chicago, IL", zoom = 13, source = "google", maptype = "roadmap")

# Plot routes
ggmap(chicago_map) +
  geom_segment(data = routes,
               aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat, color = usertype),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
  geom_point(data = geocoded_stations, aes(x = lon, y = lat), size = 2, color = "black") +
  geom_text(data = geocoded_stations, aes(x = lon, y = lat, label = station_name), hjust = -0.1, size = 3) +
  labs(title = "Top Routes by Usertype in Chicago",
       subtitle = "Customer vs Subscriber",
       x = "", y = "") +
  theme_minimal() + theme(axis.text = element_blank())

                                            
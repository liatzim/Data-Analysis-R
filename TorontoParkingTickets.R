

# Import data sets and bind together

data1 <- read.csv('Parking_Tags_Data_2015_1.csv', stringsAsFactors = F)
data2 <- read.csv('Parking_Tags_Data_2015_2.csv', stringsAsFactors = F)
data3 <- read.csv('Parking_Tags_Data_2015_3.csv', stringsAsFactors = F)

tickets=rbind(data1, data2, data3)
options("scipen"=100, "digits"=4) 

# tURNED THE PARKING DATE COLUMN INTO PROPER FORM
# Made sure the date range is only in 2016
# The range of the tickets goes as high as $450.

tickets$date_of_infraction <- as.Date(as.character(tickets$date_of_infraction), "%Y%m%d")
summary(tickets$date_of_infraction)
summary(tickets$set_fine_amount)

# Change time format
# sprintf returns a character vector containing a formatted combination of text and variable values

tickets$time_of_infraction <- sprintf("%04d", tickets$time_of_infraction)
tickets$time_of_infraction <- format(strptime(tickets$time_of_infraction, 
                                              format="%H%M"), 
                                     format = "%H:%M")

# Remove missing values
tickets=tickets[complete.cases(tickets[,-1]),]

# Time distribution of parking tickets is very equally distributed
# Only at the end of the year there is a slight decline - probably due to the holidays
# 
library(ggplot2)
ggplot(aes(x = date_of_infraction), data = tickets) +
  geom_histogram(bins = 60, color = 'black', fill = 'lightgreen') +
  ggtitle('Dates Distribution')

# First extracted only the hour from the time column
# Histagram shows that around 5-6am the best time not to get a ticket, or around midnight
# whereas around noon is the worst time 
tickets$time_of_infraction <- as.POSIXlt(tickets$time_of_infraction, format="%H:%M")$hour
ggplot(aes(x = time_of_infraction), data = tickets) + 
  geom_histogram(bins = 30, color = 'yellow', fill = 'blue') +
  theme_tufte()+
  ggtitle('Tickets Distribution By Hour of the Day')

# Fine amount distribution

ggplot(aes(x = set_fine_amount), data = tickets) + 
  geom_histogram(bins = 100, color = 'grey', fill = 'red2') +
  theme_solarized()+
  ggtitle('Fine Distribution')


# Day distribution

library(dplyr)
# Adding a column with the day of the week
tickets$day=weekdays(as.Date(tickets$date_of_infraction))
# Creating a vector for days
weekday_group=group_by(tickets,day)
parking_df_day=summarise(weekday_group, count=n(),
                         total_day=sum(set_fine_amount))

parking_df_day$day <- ordered(parking_df_day$day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
ggplot(aes(x = day, y = count), data = parking_df_day) +
  geom_bar(stat = 'identity',color='grey',fill='pink') +
  theme_tufte()+
  ylab('Number of tickets') +
  ggtitle('Day Distribution')

# weekends are weaker in terms of infractions 


# Top 10 frequent types of tickets
infraction_group=group_by(tickets, infraction_description, infraction_code)
parking_df_infr=summarise(infraction_group,count=n())

parking_df_infr=head(parking_df_infr[order(parking_df_infr$count, decreasing= TRUE),],n=10)
parking_df_infr

library(ggthemes)
ggplot(aes(x = reorder(infraction_description, count), y = count), data = parking_df_infr) +
  geom_bar(stat = 'identity') +
  theme_tufte() +
  theme(axis.text = element_text(size = 20, face = 'italic')) +
  coord_flip() +
  xlab('') +
  ylab('Total Number of Infractions') +
  ggtitle("Top 10 Infractions") +
  theme_fivethirtyeight()


# Top 10 frequent types of locations that are prone to be ticketed

location_group <- group_by(tickets, location2)
parking_df_lo <- summarise(location_group, total = sum(set_fine_amount),
                           count = n())
parking_df_lo <- head(parking_df_lo[order(parking_df_lo$count, decreasing = TRUE), ], n=10)
ggplot(aes(x = reorder(location2, count), y = count), data = parking_df_lo) +
  geom_bar(stat = 'identity') +
  theme_tufte() +
  theme(axis.text = element_text(size = 10, face = 'bold')) +
  coord_flip() +
  xlab('') +
  ylab('Total Number of Infractions') +
  ggtitle("Top 10 Locations") +
  theme_fivethirtyeight()



week_time_group <- group_by(tickets, day, time_of_infraction)
parking_df_week_time <- dplyr::summarise(week_time_group, time_sum =
                                           sum(set_fine_amount),
                                         count = n())

parking_df_week_time$day_of_week <- ordered(parking_df_week_time$day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
ggplot(aes(x = time_of_infraction, y = count, color = day_of_week), data = parking_df_week_time) +
  geom_line(size = 2.5, alpha = 0.7) +
  geom_point(size = 0.5) + 
  xlab('Hour(24 hour clock)') +
  ylab('Number of Infractions') +
  ggtitle('Infractions Time of the Day') +
  theme_economist()




# Change in distribution throughout the day of the top 10 infractions 
top_10 <- c('PARK-SIGNED HWY-PROHIBIT DY/TM', 'PARK ON PRIVATE PROPERTY', 'PARK PROHIBITED TIME NO PERMIT', 'PARK FAIL TO DISPLAY RECEIPT', 'PARK MACHINE-REQD FEE NOT PAID', 'PARK - LONGER THAN 3 HOURS ', 'STOP-SIGNED HWY-PROHIBIT TM/DY', 'STAND VEH.-PROHIBIT TIME/DAY', 'STOP-SIGNED HIGHWAY-RUSH HOUR', 'PARK-VEH. W/O VALID ONT PLATE')
parking_df_top_10 <- parking_df %>%
  filter(infraction_description %in% top_10)
top_10_groups <- group_by(parking_df_top_10, infraction_description, day, time_of_infraction)
parking_df_top_10 <- dplyr::summarise(top_10_groups, total =
                                        sum(set_fine_amount),
                                      count = n())

parking_df_top_10$day_of_week <- ordered(parking_df_top_10$day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
ggplot(aes(x = time_of_infraction, y = count, color = day_of_week), data = parking_df_top_10) +
  geom_line(size = 1.5) +
  geom_point(size = 0.5) +
  xlab('Hour(24 hour clock)') +
  ylab('Number of Infractions') +
  ggtitle('(log_10)Infractions Time of the Day') +
  scale_y_log10() +
  facet_wrap(~infraction_description)

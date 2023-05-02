#Name: Kenen Corea
#Purpose: Google Data Analytics Capstone Project: Cyclistic Case Study

#Load in libraries
library(tidyverse)
library(magrittr)
library(janitor)


#Set working directory
setwd("~/Desktop/Data/Cyclistic_Case_Study/Cyclistic_Raw_CSVs")


#Load in each month's dataset for the past twelve months (April 2022 - March 2023)
apr_2022 <- read_csv("2022_04-divvy-tripdata.csv")
may_2022 <- read_csv("2022_05-divvy-tripdata.csv")
jun_2022 <- read_csv("2022_06-divvy-tripdata.csv")
jul_2022 <- read_csv("2022_07-divvy-tripdata.csv")
aug_2022 <- read_csv("2022_08-divvy-tripdata.csv")
sep_2022 <- read_csv("2022_09-divvy-publictripdata.csv")
oct_2022 <- read_csv("2022_10-divvy-tripdata.csv")
nov_2022 <- read_csv("2022_11-divvy-tripdata.csv")
dec_2022 <- read_csv("2022_12-divvy-tripdata.csv")
jan_2023 <- read_csv("2023_01-divvy-tripdata.csv")
feb_2023 <- read_csv("2023_02-divvy-tripdata.csv")
mar_2023 <- read_csv("2023_03-divvy-tripdata.csv")

 
#Since all of our tables have the same exact variables/columns, we can vertically
#merge each table onto one another using the "rbind()" function, creating a new dataframe
#named "total_year_merged"
total_year_merged <- rbind(apr_2022, may_2022, jun_2022, jul_2022, aug_2022, sep_2022, 
              oct_2022, nov_2022, dec_2022, jan_2023, feb_2023, mar_2023)


################################################################################################################
########################## CLEANING & TRANSFORMATION ###########################################################
################################################################################################################


#Our dataset already appears to be clean at first glance, but we will run the "clean_names()"
#function from the janitor package for good measure
total_year_merged %<>% clean_names()


#Running a simple algorithm to confirm that there are no duplicated "ride_id"s/instances
#We are looking for zero results to show when this line is run ("A tibble: 0 x 2")
total_year_merged %>% count(ride_id) %>% filter(n>1)
#Test passed! We now know that there are no duplicate ride IDs


#Running some tests to make sure some key variables/columns all have the same name/spelling
#convention for every observation
total_year_merged %>% distinct(member_casual)
total_year_merged %>% distinct(rideable_type)
#Passed!


#Running a test to see if some key variables/columns contain N/A values
#In this case, we want every observation to be "FALSE", any "TRUE"
#values signify N/A instances
total_year_merged %>% count(is.na(ride_id))
total_year_merged %>% count(is.na(member_casual))
#Passed!


#Creating a new column named "ride_length" that calculates the difference
#between start and end times, converting the time in seconds to HH:MM:SS format,
#and creating another new column named "ride_length_minutes" which does the same thing
#but only displays the minutes (easier for graphing and analysis later on) 
total_year_merged %<>% mutate(ride_length = (ended_at - started_at)) %>%
                       mutate(ride_length = hms::as_hms(ride_length)) %>%
                       mutate(ride_length_minutes = minute(ride_length))


#Creating a new column named "day_of_week" that calculates the day of the week of 
#each starting ride (Sunday = 1, Saturday = 7)
total_year_merged %<>% mutate(day_of_week = wday(started_at))


#Editing the "member_casual" column so that the outputs start with a capitalized letter for nicer graph labeling
#(purely aesthetic)
total_year_merged %<>% mutate(member_casual = ifelse(member_casual == "casual", "Casual", "Member"))


################################################################################################################
############################# ANALYSIS #########################################################################
################################################################################################################


########## GRAPH 1 ##########

#Creating a new table named "avg_ride_time_by_group" that summarizes the average ride time per group (casual or member)
avg_ride_time_by_group <- total_year_merged %>% group_by(member_casual) %>% 
                                                summarize(mean_ride_length = mean(ride_length_minutes, na.rm = T))


#Creating bar chart plotting the average ride time per group
avg_ride_time_by_group %>% ggplot(aes(x = member_casual, y = mean_ride_length, fill = member_casual)) +
                           geom_bar(stat = "identity", width = 0.5) +
                           ylim(0,17) +
                           scale_fill_manual(values = c("#212121", "#0071bc")) +
                           labs(title = "Average Ride Time by Customer Type",
                                subtitle = "Casual Customers vs Members",
                                x = "Customer Type",
                                y = "Average Ride Length (Minutes)",
                                fill = "") +
                           theme_minimal() +
                           geom_text(aes(label = round(mean_ride_length, digits = 2)), vjust = -1, size = 4.5)


########## GRAPH 2 ##########

#Creating a new column that inputs the numerical day of week and outputs the corresponding abbreviated
#day of the week for easier graph labeling (purely aesthetic)
total_year_merged %<>% mutate(day_of_week_abr = case_when(day_of_week == 1 ~ "Sun",
                                                          day_of_week == 2 ~ "Mon",
                                                          day_of_week == 3 ~ "Tue",
                                                          day_of_week == 4 ~ "Wed",
                                                          day_of_week == 5 ~ "Thu",
                                                          day_of_week == 6 ~ "Fri",
                                                          day_of_week == 7 ~ "Sat"))


#Converting the new abbreviated day of week column into a factor and specifying the levels argument in order to
#manually control the order of the bars (First bar = Monday, Last bar = Sunday)
total_year_merged$day_of_week_abr <- factor(total_year_merged$day_of_week_abr,
                                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


#Creating the bar chart plotting the amount of rides per weekday by group
total_year_merged %>% ggplot(aes(x = day_of_week_abr, fill = member_casual)) +
                                    geom_bar() +
                                    scale_y_continuous(labels = scales::comma) +
                                    scale_fill_manual(values = c("#212121", "#0071bc")) +
                                    labs(title = "Total Rides by Weekday",
                                         subtitle = "Casual Customers vs Members",
                                         x = "Day of Week",
                                         y = "Rides",
                                         fill = "") +
                                    facet_wrap(~member_casual) + 
                                    theme_minimal()


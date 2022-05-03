# Cyclist Visualizations

# Load packages

library(dplyr)
library(tidyverse)
library(readr)
library(skimr)
library(hablar)
library(lubridate)
library(ggplot2)
library(scales)

# Read csv file with the 25 milion entries

#
full_y_2015_2021 <- read_csv("full_y_2015_2021.csv")

skim_without_charts(full_y_2015_2021)

## Users behavior  (1-4) ----
# Trips made (num obs, mean, duration, etc)

#
gg_year_obs <-  full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  select(new_usertype, tripduration, year_ot) %>% 
  group_by(year_ot) %>% 
  summarize(num_obs = n(),
            mean_trip = mean(tripduration),
            max_trip = max(tripduration),
            min_trip = min(tripduration)) %>% 
  mutate(percent_of_users = 100*num_obs / sum(num_obs))

gg_year_obs %>% 
  select(percent_of_users) %>%
  summarize(sum(percent_of_users))

gg_year_obs

# 1 - Number of trips made from 2015 to 2021
write.csv(gg_year_obs, "1gg_year_obs.csv", row.names = FALSE)

# 
# 2 - Mean trip duration per year

# 2-2 Mean trip duration per user type
gg_year_obs_2 <-  full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  select(new_usertype, tripduration, year_ot) %>% 
  group_by(year_ot, new_usertype) %>% 
  summarize(num_obs = n(),
            mean_trip = mean(tripduration),
            max_trip = max(tripduration),
            min_trip = min(tripduration)) %>% 
  mutate(percent_of_users = 100*num_obs / sum(num_obs))

gg_year_obs_2
write.csv(gg_year_obs_2, "2_2gg_year_obs.csv", row.names = FALSE)

## Distribution of user type from 2015 to 2021 

gg_usertype_f <- full_y_2015_2021 %>% 
  group_by(new_usertype) %>% 
  summarize(num_obs = n(),
            mean_trip = mean(tripduration),
            max_trip = max(tripduration),
            min_trip = min(tripduration)) %>% 
  mutate(percent_of_users = num_obs / sum(num_obs))

gg_usertype_f

# 3 - Distribution of user type from 2015 to 2021
write.csv(gg_usertype_f, "3gg_usertype_f.csv", row.names = FALSE)

## Distribution of user type per year

gg_usertype_y <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  select(new_usertype, tripduration, year_ot) %>% 
  group_by(year_ot, new_usertype) %>% 
  summarize(num_obs = n(),
            mean_trip = mean(tripduration)) %>% 
  mutate(percent_7years = 100*num_obs / 25475734) %>% 
  mutate(percent_per_year = 100*num_obs/sum(num_obs))

gg_usertype_y

# 4 - Distribution of user type per year
write.csv(gg_usertype_y, "4gg_usertype_y.csv", row.names = FALSE)

### Stations
## Top 10 Start stations from 2015 to 2021 (5-6) ----

gg_start_fu <- full_y_2015_2021 %>% 
  group_by(from_station_id, from_station_name) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 25475734) %>% 
  arrange(desc(num_obs)) %>% 
  head(10)

gg_start_fu

# 5 - Top 10 Start stations from 2015 to 2021
write.csv(gg_start_fu, "5gg_start_fu.csv", row.names = FALSE)

## Top 5 Start stations from 2015 to 2021 per user type

gg_start_st <- full_y_2015_2021 %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 18076209), 
                                 (100*num_obs / 7399525))) %>% 
  filter(num_obs == 246341 | num_obs == 244693 | num_obs == 228301 | 
           num_obs == 177236 | num_obs == 166425 | num_obs == 390487 |
           num_obs == 240512 | num_obs == 164433 | num_obs == 157003 |
           num_obs == 150449 ) %>% 
  arrange(desc(num_obs))

gg_start_st

# 
# 6 - Top 5 Start stations from 2015 to 2021 per user type
write.csv(gg_start_st, "6gg_start_st.csv", row.names = FALSE)

## Start Stations (7) ----
# Top start stations
gg_start_st_f <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 25475734) %>% 
  arrange(desc(num_obs)) %>% 
  head(15)

gg_start_st_f

#2015
t15 <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2015) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3183439) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2016
t16 <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2016) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3595383) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2017
t17 <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2017) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3525382) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2018
t18 <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2018) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3601571) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2019
t19 <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2019) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3816156) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2020
t20 <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2020) %>% 
  group_by(from_station_id, from_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3329741) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

# Unite start stations table 15-21

t15_21 <- rbind(t15, t16, t17, t18, t19, t20, t21)

t15_21

# 
## 7 - Top 5 Start stations per year
write.csv(t15_21, "7t15_21.csv", row.names = FALSE)

## Top 5 Start stations per user type (8-14) ----

## 2015 

t15_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2015) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2253413), 
                                 (100*num_obs / 930026))) %>% 
  filter(num_obs == 54260 | num_obs == 39539 | num_obs == 26850 | 
           num_obs == 26359 | num_obs == 25688 | num_obs == 37113 |
           num_obs == 34149 | num_obs == 29615 | num_obs == 24747 |
           num_obs == 21872 ) %>% 
  arrange(desc(num_obs))

t15_u

# 8 - Top 5 Start stations per user type in the year 2015
write.csv(t15_u, "8t15_u.csv", row.names = FALSE)

## 2016 

t16_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2016) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2736869), 
                                 (100*num_obs / 858514))) %>% 
  filter(num_obs == 70921 | num_obs == 40467 | num_obs == 27711 | 
           num_obs == 26190 | num_obs == 25763 | num_obs == 45907 |
           num_obs == 39402 | num_obs == 33801 | num_obs == 31799 |
           num_obs == 28930 ) %>% 
  arrange(desc(num_obs))

t16_u

# 9 - Top 5 Start stations per user type in the year 2016
write.csv(t16_u, "9t16_u.csv", row.names = FALSE)

## 2017 

t17_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2017) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2726342), 
                                 (100*num_obs / 799040))) %>% 
  filter(num_obs == 73848 | num_obs == 40039 | num_obs == 27248 | 
           num_obs == 25293 | num_obs == 23715 | num_obs == 45729 |
           num_obs == 45353 | num_obs == 39231 | num_obs == 29688 |
           num_obs == 29436 ) %>% 
  arrange(desc(num_obs))

t17_u

# 10 - Top 5 Start stations per user type in the year 2017
write.csv(t17_u, "10t17_u.csv", row.names = FALSE)

## 2018 

t18_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2018) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2925333), 
                                 (100*num_obs / 676238))) %>% 
  filter(num_obs == 49798 | num_obs == 27456 | num_obs == 19989 | 
           num_obs == 17918 | num_obs == 16662 | num_obs == 59537 |
           num_obs == 48861 | num_obs == 46497 | num_obs == 32324 |
           num_obs == 31784 ) %>% 
  arrange(desc(num_obs))

t18_u

# 11 - Top 5 Start stations per user type in the year 2018
write.csv(t18_u, "11t18_u.csv", row.names = FALSE)

## 2019 

t19_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2019) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2936866), 
                                 (100*num_obs / 879290))) %>% 
  filter(num_obs == 50569 | num_obs == 45986 | num_obs == 45375 | 
           num_obs == 31369 | num_obs == 30828 | num_obs == 53071 |
           num_obs == 39209 | num_obs == 21728 | num_obs == 21370 |
           num_obs == 20609 ) %>% 
  arrange(desc(num_obs))

t19_u

# 12 - Top 5 Start stations per user type in the year 2019
write.csv(t19_u, "12t19_u.csv", row.names = FALSE)

## 2020 

t20_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2020) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>%
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2051340), 
                                 (100*num_obs / 1278401))) %>% 
  filter(num_obs == 19885 | num_obs == 16261 | num_obs == 15559 | 
           num_obs == 15453 | num_obs == 15371 | num_obs == 25585 |
           num_obs == 19711 | num_obs == 18186 | num_obs == 14367 |
           num_obs == 13064 ) %>% 
  arrange(desc(num_obs))

t20_u

# 13 - Top 5 Start stations per user type in the year 2020
write.csv(t20_u, "13t20_u.csv", row.names = FALSE)

## 2021 

t21_u <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2021) %>% 
  group_by(from_station_id, from_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>%
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2446046), 
                                 (100*num_obs / 1978016))) %>% 
  filter(num_obs == 23291 | num_obs == 22303 | num_obs == 21893 | 
           num_obs == 19806 | num_obs == 18172 | num_obs == 63004 |
           num_obs == 34091 | num_obs == 31531 | num_obs == 28073 |
           num_obs == 28053 ) %>% 
  arrange(desc(num_obs))

t21_u

# 14 - Top 5 Start stations per user type in the year 2021
write.csv(t21_u, "14t21_u.csv", row.names = FALSE)

# For see top 5 of each year

full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2015) %>% 
  group_by(new_usertype) %>% 
  summarize(num_obs = n())

## Top end stations from 2015 to 2021, (15-16) ----

gg_end_fu <- full_y_2015_2021 %>% 
  group_by(to_station_id, to_station_name) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 25475734) %>% 
  arrange(desc(num_obs)) %>% 
  head(10)

gg_end_fu

# 15 - Top 10 End stations from 2015 to 2021
write.csv(gg_end_fu, "15gg_end_fu.csv", row.names = FALSE)

# Top 5 End stations from 2015 to 2021 per user type

gg_end_st <- full_y_2015_2021 %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 18076209), 
                                 (100*num_obs / 7399525))) %>% 
  filter(num_obs == 450267 | num_obs == 241880 | num_obs == 234903 | 
           num_obs == 221397 | num_obs == 216929 | num_obs == 183589 |
           num_obs == 179257 | num_obs == 177884 | num_obs == 166065 |
           num_obs == 164578 ) %>% 
  arrange(desc(num_obs))

gg_end_st

# 16 - Top 5 End stations from 2015 to 2021 per user type
write.csv(gg_end_st, "16gg_end_st.csv", row.names = FALSE)

# to obtain the sum of the %
gg_end_st %>% 
  filter(new_usertype == "casual riders") %>% 
  group_by(percent_7years) %>% 
  summarize(sum(percent_7years)) %>% 
  summarize(sum(`sum(percent_7years)`) )

## End Stations (17) ----
# Top end stations per year (17)

#2015
t15_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2015) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3183439) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2016
t16_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2016) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3595383) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2017
t17_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2017) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3525382) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2018
t18_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2018) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3601571) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2019
t19_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2019) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3816156) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2020
t20_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2020) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3329741) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

#2021
t21_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2021) %>% 
  group_by(to_station_id, to_station_name, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 4424062) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

# Top end stations table 15-21

t15_21_e <- rbind(t15_e, t16_e, t17_e, t18_e, t19_e, t20_e, t21_e)

t15_21_e

# 
## 17 - Top 5 End stations per year
write.csv(t15_21_e, "17t15_21_e.csv", row.names = FALSE)

## Distribution user type - end stations  (18-24) ----

## 2015 - end 

t15_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2015) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2253413), 
                                 (100*num_obs / 930026))) %>% 
  filter(num_obs == 37032 | num_obs == 34343 | num_obs == 32894 | 
           num_obs == 21605 | num_obs == 21464 | num_obs == 65913 |
           num_obs == 37917 | num_obs == 30562 | num_obs == 28914 |
           num_obs == 28668 ) %>% 
  arrange(desc(num_obs))

t15_u_e

# 18 - Top 5 End stations per user type in the year 2015
write.csv(t15_u_e, "18t15_u_e.csv", row.names = FALSE)

## 2016  - end

t16_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2016) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2736869), 
                                 (100*num_obs / 858514))) %>% 
  filter(num_obs == 79703 | num_obs == 38723 | num_obs == 31202 | 
           num_obs == 29831 | num_obs == 28020 | num_obs == 43054 |
           num_obs == 37319 | num_obs == 33743 | num_obs == 29084 |
           num_obs == 29044 ) %>% 
  arrange(desc(num_obs))

t16_u_e

# 19 - Top 5 End stations per user type in the year 2016
write.csv(t16_u_e, "19t16_u_e.csv", row.names = FALSE)

## 2017 - end

t17_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2017) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2726342), 
                                 (100*num_obs / 799040))) %>% 
  filter(num_obs == 43565 | num_obs == 42890 | num_obs == 40617 | 
           num_obs == 29690 | num_obs == 27085 | num_obs == 81593 |
           num_obs == 37236 | num_obs == 29627 | num_obs == 28375 |
           num_obs == 25745 ) %>% 
  arrange(desc(num_obs))

t17_u_e

# 20 - Top 5 End stations per user type in the year 2017
write.csv(t17_u_e, "20t17_u_e.csv", row.names = FALSE)

## 2018 - end

t18_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2018) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2925333), 
                                 (100*num_obs / 676238))) %>% 
  filter(num_obs == 61584 | num_obs == 23672 | num_obs == 21466 | 
           num_obs == 18155 | num_obs == 17918 | num_obs == 56532 |
           num_obs == 48062 | num_obs == 45794 | num_obs == 32769 |
           num_obs == 32579 ) %>% 
  arrange(desc(num_obs))

t18_u_e

# 21 - Top 5 End stations per user type in the year 2018
write.csv(t18_u_e, "21t18_u_e.csv", row.names = FALSE)

## 2019 - end

t19_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2019) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2936866), 
                                 (100*num_obs / 879290))) %>% 
  filter(num_obs == 48192 | num_obs == 47328 | num_obs == 44304 | 
           num_obs == 30629 | num_obs == 30209 | num_obs == 67572 |
           num_obs == 30668 | num_obs == 25205 | num_obs == 23684 |
           num_obs == 23271 ) %>% 
  arrange(desc(num_obs))

t19_u_e

# 22 - Top 5 End stations per user type in the year 2019
write.csv(t19_u_e, "22t19_u_e.csv", row.names = FALSE)

## 2020 - end

t20_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2020) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>%
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2051340), 
                                 (100*num_obs / 1278401))) %>% 
  filter(num_obs == 28208 | num_obs == 19267 | num_obs == 19094 | 
           num_obs == 16274 | num_obs == 13701 | num_obs == 20564 |
           num_obs == 17406 | num_obs == 16455 | num_obs == 16033 |
           num_obs == 15784 ) %>% 
  arrange(desc(num_obs))

t20_u_e

# 23 - Top 5 End stations per user type in the year 2020
write.csv(t20_u_e, "23t20_u_e.csv", row.names = FALSE)

## 2021 - end

t21_u_e <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2021) %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>%
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2446046), 
                                 (100*num_obs / 1978016))) %>% 
  filter(num_obs == 23309 | num_obs == 22968 | num_obs == 22197 | 
           num_obs == 20390 | num_obs == 18833 | num_obs == 65694 |
           num_obs == 33017 | num_obs == 32976 | num_obs == 31825 |
           num_obs == 29847 ) %>% 
  arrange(desc(num_obs))

t21_u_e

# 24 - Top 5 End stations per user type in the year 2021
write.csv(t21_u_e, "24t21_u_e.csv", row.names = FALSE)

# to obtain the sum of the %
t21_u_e %>% 
  filter(new_usertype == "annual members") %>% 
  group_by(percent_7years) %>% 
  summarize(sum(percent_7years)) %>% 
  summarize(sum(`sum(percent_7years)`) )

# To obtain the first 5
full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2021) %>% 
  filter(new_usertype == "casual riders") %>% 
  group_by(to_station_id, to_station_name, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

## Top trips routes (25-26) ----

#
trips_rutes_15_21 <- full_y_2015_2021 %>% 
  unite("trip_rute", from_station_name, to_station_name, sep = " to ",
        remove = FALSE) %>% 
  select(starttime, new_usertype, trip_rute)

trips_rutes_15_21 

#
gg_trip_fu <- trips_rutes_15_21  %>% 
  group_by(trip_rute) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 25475734) %>% 
  arrange(desc(num_obs)) %>% 
  head(10)

gg_trip_fu

# 25 - Top 10 Trip routes from 2015 to 2021
write.csv(gg_trip_fu, "25gg_trip_fu.csv", row.names = FALSE)

## Top 5 Trip routes from 2015 to 2021 per user type

gg_trip_st <- trips_rutes_15_21 %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 18076209), 
                                 (100*num_obs / 7399525))) %>% 
  filter(num_obs == 53753 | num_obs == 53082 | num_obs == 41567 | 
           num_obs == 28385 | num_obs == 26445 | num_obs == 12840 |
           num_obs == 12362 | num_obs == 11618 | num_obs == 11223 |
           num_obs == 11141 ) %>% 
  arrange(desc(num_obs))

gg_trip_st

# 26 - Top 5 Trip routes from 2015 to 2021 per user type
write.csv(gg_trip_st, "26gg_trip_st.csv", row.names = FALSE)

# To obtain the sum of the %
gg_trip_st %>% 
  filter(new_usertype == "annual members") %>% 
  group_by(percent_7years) %>% 
  summarize(sum(percent_7years)) %>% 
  summarize(sum(`sum(percent_7years)`) )

## Top Trip routes per year (27) ----

#
trips_rutes_15_21 <- full_y_2015_2021 %>% 
  unite("trip_rute", from_station_name, to_station_name, sep = " to ",
        remove = FALSE) %>% 
  select(starttime, new_usertype, trip_rute)

trips_rutes_15_21 

#2015
t_rs_15 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2015) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3183439) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_15

#2016
t_rs_16 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2016) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3595383) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_16

#2017
t_rs_17 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2017) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3525382) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_17

#2018
t_rs_18 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2018) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3601571) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_18

#2019
t_rs_19 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2019) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3816156) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_19

#2020
t_rs_20 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2020) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 3329741) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_20

#2021
t_rs_21 <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>% 
  filter(year_ot== 2021) %>% 
  group_by(trip_rute, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = 100*num_obs / 4424062) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

t_rs_21

gg_year_obs

# top trips routes per year table 15-21

t_rs_15_21 <- rbind(t_rs_15, t_rs_16, t_rs_17, t_rs_18, t_rs_19,
                    t_rs_20, t_rs_21) 

t_rs_15_21

# 
## 27 - Top 5 Trip routes per year
write.csv(t_rs_15_21, "27t_rs_15_21.csv", row.names = FALSE)

## Trips routes distribution per user typepor (28-34) ----

## 2015 - trips

t_rs15_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2015) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2253413), 
                                 (100*num_obs / 930026))) %>% 
  filter(num_obs == 8833 | num_obs == 5263 | num_obs == 5156 | 
           num_obs == 4523 | num_obs == 4509 | num_obs == 2007 |
           num_obs == 1976 | num_obs == 1940 | num_obs == 1829 |
           num_obs == 1799 ) %>% 
  arrange(desc(num_obs))

t_rs15_u

# 28 - Top 5 Trip routes per user type in the year 2015
write.csv(t_rs15_u, "28t_rs15_u.csv", row.names = FALSE)

## 2016 - trips

t_rs16_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2016) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2736869), 
                                 (100*num_obs / 858514))) %>% 
  filter(num_obs == 2376 | num_obs == 2222 | num_obs == 2125 | 
           num_obs == 2111 | num_obs == 2103 | num_obs == 10063 |
           num_obs == 7286 | num_obs == 5999 | num_obs == 5862 |
           num_obs == 5625 ) %>% 
  arrange(desc(num_obs))

t_rs16_u

# 29 - Top 5 Trip routes per user type in the year 2016
write.csv(t_rs16_u, "29t_rs16_u.csv", row.names = FALSE)

## 2017 - trips

t_rs17_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2017) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2726342), 
                                 (100*num_obs / 799040))) %>% 
  filter(num_obs == 10350 | num_obs == 8758 | num_obs == 6430 | 
           num_obs == 6007 | num_obs == 5840 | num_obs == 2916 |
           num_obs == 2579 | num_obs == 2458 | num_obs == 2409 |
           num_obs == 2378 ) %>% 
  arrange(desc(num_obs))

t_rs17_u

# 30 - Top 5 Trip routes per user type in the year 2017
write.csv(t_rs17_u, "30t_rs17_u.csv", row.names = FALSE)

## 2018 - trips

t_rs18_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2018) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2925333), 
                                 (100*num_obs / 676238))) %>% 
  filter(num_obs == 4037 | num_obs == 3909 | num_obs == 3314 | 
           num_obs == 3029 | num_obs == 2935 | num_obs == 8052 |
           num_obs == 7043 | num_obs == 5431 | num_obs == 3700 |
           num_obs == 3319 ) %>% 
  arrange(desc(num_obs))

t_rs18_u

# 31 - Top 5 Trip routes per user type in the year 2018
write.csv(t_rs18_u, "31t_rs18_u.csv", row.names = FALSE)

## 2019 - trips

t_rs19_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2019) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2936866), 
                                 (100*num_obs / 879290))) %>% 
  filter(num_obs == 9348 | num_obs == 8631 | num_obs == 8049 | 
           num_obs == 4989 | num_obs == 3527 | num_obs == 3538 |
           num_obs == 2777 | num_obs == 2640 | num_obs == 2537 |
           num_obs == 2328 ) %>% 
  arrange(desc(num_obs))

t_rs19_u

# 32 - Top 5 Trip routes per user type in the year 2019
write.csv(t_rs19_u, "32t_rs19_u.csv", row.names = FALSE)

## 2020 - trips

t_rs20_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2020) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>%
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2051340), 
                                 (100*num_obs / 1278401))) %>% 
  filter(num_obs == 1188 | num_obs == 1025 | num_obs == 891 | 
           num_obs == 880 | num_obs == 855 | num_obs == 5564 |
           num_obs == 5518 | num_obs == 4592 | num_obs == 4513 |
           num_obs == 3568 ) %>% 
  arrange(desc(num_obs))

t_rs20_u

# 33 - Top 5 Trip routes per user type in the year 2020
write.csv(t_rs20_u, "33t_rs20_u.csv", row.names = FALSE)

## 2021 - trips

t_rs21_u <- trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2021) %>% 
  group_by(trip_rute, new_usertype) %>% 
  summarize(num_obs = n()) %>%
  mutate(percent_7years = ifelse(new_usertype == 'annual members', 
                                 (100*num_obs / 2446046), 
                                 (100*num_obs / 1978016))) %>% 
  filter(num_obs == 10953 | num_obs == 7412 | num_obs == 5766 | 
           num_obs == 5588 | num_obs == 5544 | num_obs == 4082 |
           num_obs == 3652 | num_obs == 3109 | num_obs == 3010 |
           num_obs == 1989 ) %>% 
  arrange(desc(num_obs))

t_rs21_u

# 34 - Top 5 Trip routes per user type in the year 2021
write.csv(t_rs21_u, "34t_rs21_u.csv", row.names = FALSE)

# To obtain the sum of the %
t_rs21_u %>% 
  filter(new_usertype == "casual riders") %>% 
  group_by(percent_7years) %>% 
  summarize(sum(percent_7years)) %>% 
  summarize(sum(`sum(percent_7years)`) )

# to obtain the first 5
trips_rutes_15_21 %>% 
  mutate(year_ot = year(starttime)) %>%
  filter(year_ot== 2021) %>% 
  filter(new_usertype == "annual members") %>% 
  group_by(trip_rute, year_ot, new_usertype) %>% 
  summarize(num_obs = n()) %>% 
  arrange(desc(num_obs)) %>% 
  head(5)

## Time analysis (35-37) ----

# Trips per month according to the type of user from 2015 to 2021

gg_t_m <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  mutate(month_ot = month(starttime, label = TRUE)) %>% 
  select(new_usertype, year_ot, month_ot) %>% 
  group_by(new_usertype, year_ot, month_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015 & new_usertype == "annual members", 
            (100*num_obs/2253413), 
            (ifelse(year_ot == 2016 & new_usertype == "annual members", 
             (100*num_obs/2736869), 
             (ifelse(year_ot == 2017 & new_usertype == "annual members", 
              (100*num_obs/2726342), 
              (ifelse(year_ot == 2018 & new_usertype == "annual members", 
               (100*num_obs/2925333),
               (ifelse(year_ot == 2019 & new_usertype == "annual members", 
                (100*num_obs/2936866), 
                (ifelse(year_ot == 2020 & new_usertype == "annual members", 
                 (100*num_obs/2051340), 
                 (ifelse(year_ot == 2021 & new_usertype == "annual members", 
                  (100*num_obs/2446046), 
                  (ifelse(year_ot == 2015 & new_usertype == "casual riders", 
                   (100*num_obs/930026), 
                   (ifelse(year_ot == 2016 & new_usertype == "casual riders", 
                   (100*num_obs/858514), 
                   (ifelse(year_ot == 2017 & new_usertype == "casual riders", 
                   (100*num_obs/799040), 
                   (ifelse(year_ot == 2018 & new_usertype == "casual riders", 
                   (100*num_obs/676238), 
                   (ifelse(year_ot == 2019 & new_usertype == "casual riders", 
                   (100*num_obs/879290), 
                   (ifelse(year_ot == 2020 & new_usertype == "casual riders", 
                   (100*num_obs/1278401), 
                   (100*num_obs/1978016)
                                                                                                                  
                      )) ))) ))) ))) ))) ))) )))) )) )) )

gg_t_m

# 35 - Trips per month according to the type of user from 2015 to 2021
write.csv(gg_t_m, "35gg_t_m.csv", row.names = FALSE)

# Casual riders from Jun, Jul, Aug

gg_t_m %>% 
  filter(new_usertype == "casual riders") %>% 
  filter(month_ot == "Jun" | month_ot == "Jul" | month_ot == "Aug") %>% 
  filter(year_ot == 2021) %>% 
  summarize(sum(percent_of_per_year))

# Trips per day of the week according to the type of user

gg_t_wday <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  mutate(wday_ot =wday(starttime, label = TRUE)) %>% 
  select(new_usertype, year_ot, wday_ot) %>% 
  group_by(new_usertype, year_ot, wday_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015 & new_usertype == "annual members", 
            (100*num_obs/2253413), 
           (ifelse(year_ot == 2016 & new_usertype == "annual members", 
           (100*num_obs/2736869), 
            (ifelse(year_ot == 2017 & new_usertype == "annual members", 
            (100*num_obs/2726342), 
              (ifelse(year_ot == 2018 & new_usertype == "annual members", 
             (100*num_obs/2925333),
              (ifelse(year_ot == 2019 & new_usertype == "annual members", 
             (100*num_obs/2936866), 
             (ifelse(year_ot == 2020 & new_usertype == "annual members", 
              (100*num_obs/2051340), 
              (ifelse(year_ot == 2021 & new_usertype == "annual members", 
             (100*num_obs/2446046), 
              (ifelse(year_ot == 2015 & new_usertype == "casual riders", 
              (100*num_obs/930026), 
              (ifelse(year_ot == 2016 & new_usertype == "casual riders", 
              (100*num_obs/858514), 
              (ifelse(year_ot == 2017 & new_usertype == "casual riders", 
                (100*num_obs/799040), 
                (ifelse(year_ot == 2018 & new_usertype == "casual riders", 
              (100*num_obs/676238), 
               (ifelse(year_ot == 2019 & new_usertype == "casual riders", 
                (100*num_obs/879290), 
               (ifelse(year_ot == 2020 & new_usertype == "casual riders", 
                           (100*num_obs/1278401), 
                            (100*num_obs/1978016)
                                                                                                                  
                          )) ))) ))) ))) ))) ))) )))) )) )) )

gg_t_wday

# 36 - Trips per day of the week according to the type of user
write.csv(gg_t_wday, "36gg_t_wday.csv", row.names = FALSE)

## Casual riders from Sat & Sun

gg_t_wday %>% 
  filter(new_usertype == "casual riders") %>% 
  filter(wday_ot == "Sat" | wday_ot == "Sun") %>% 
  filter(year_ot == 2021) %>% 
  summarize(sum(percent_of_per_year))

## Trips per hour of the day according to the type of user

gg_t_hour <- full_y_2015_2021 %>% 
  mutate(year_ot = year(starttime)) %>% 
  mutate(hour_ot = hour(starttime)) %>% 
  select(new_usertype, year_ot, hour_ot) %>% 
  group_by(new_usertype, year_ot, hour_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015 & new_usertype == "annual members", 
          (100*num_obs/2253413), 
        (ifelse(year_ot == 2016 & new_usertype == "annual members", 
        (100*num_obs/2736869), 
       (ifelse(year_ot == 2017 & new_usertype == "annual members", 
        (100*num_obs/2726342), 
          (ifelse(year_ot == 2018 & new_usertype == "annual members", 
          (100*num_obs/2925333),
          (ifelse(year_ot == 2019 & new_usertype == "annual members", 
        (100*num_obs/2936866), 
          (ifelse(year_ot == 2020 & new_usertype == "annual members", 
        (100*num_obs/2051340), 
         (ifelse(year_ot == 2021 & new_usertype == "annual members", 
        (100*num_obs/2446046), 
         (ifelse(year_ot == 2015 & new_usertype == "casual riders", 
          (100*num_obs/930026), 
         (ifelse(year_ot == 2016 & new_usertype == "casual riders", 
          (100*num_obs/858514), 
         (ifelse(year_ot == 2017 & new_usertype == "casual riders", 
          (100*num_obs/799040), 
          (ifelse(year_ot == 2018 & new_usertype == "casual riders", 
           (100*num_obs/676238), 
           (ifelse(year_ot == 2019 & new_usertype == "casual riders", 
           (100*num_obs/879290), 
           (ifelse(year_ot == 2020 & new_usertype == "casual riders", 
                (100*num_obs/1278401), 
                  (100*num_obs/1978016)
                                                                                                                  
              )) ))) ))) ))) ))) ))) )))) )) )) )

gg_t_hour

# 37 - Trips per hour of the day according to the type of user
write.csv(gg_t_hour, "37gg_t_hour.csv", row.names = FALSE)

## Annual members 8hrs and 17hrs

gg_t_hour %>% 
  filter(new_usertype == "annual members")%>% 
  filter(hour_ot == 8 | hour_ot == 17) %>% 
  filter(year_ot == 2021) %>% 
  summarize(sum(percent_of_per_year))

## Day of the week per month (38-44) ----

gg_t_dwm <- full_y_2015_2021 %>% 
  mutate(year_ot  = year(starttime)) %>% 
  mutate(month_ot = month(starttime, label = TRUE)) %>% 
  mutate(wday_ot = wday(starttime, label = TRUE)) %>% 
  group_by(new_usertype, year_ot, month_ot, wday_ot) %>% 
  summarise(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015 & new_usertype == "annual members", 
           (100*num_obs/2253413), 
            (ifelse(year_ot == 2016 & new_usertype == "annual members", 
            (100*num_obs/2736869), 
            (ifelse(year_ot == 2017 & new_usertype == "annual members", 
             (100*num_obs/2726342), 
             (ifelse(year_ot == 2018 & new_usertype == "annual members", 
              (100*num_obs/2925333),
              (ifelse(year_ot == 2019 & new_usertype == "annual members", 
               (100*num_obs/2936866), 
              (ifelse(year_ot == 2020 & new_usertype == "annual members", 
               (100*num_obs/2051340), 
                (ifelse(year_ot == 2021 & new_usertype == "annual members", 
                (100*num_obs/2446046), 
                (ifelse(year_ot == 2015 & new_usertype == "casual riders", 
                (100*num_obs/930026), 
               (ifelse(year_ot == 2016 & new_usertype == "casual riders", 
              (100*num_obs/858514), 
               (ifelse(year_ot == 2017 & new_usertype == "casual riders", 
               (100*num_obs/799040), 
                (ifelse(year_ot == 2018 & new_usertype == "casual riders", 
                (100*num_obs/676238), 
                (ifelse(year_ot == 2019 & new_usertype == "casual riders", 
                 (100*num_obs/879290), 
                  (ifelse(year_ot == 2020 & new_usertype == "casual riders", 
                        (100*num_obs/1278401), 
                         (100*num_obs/1978016)
                                                                                                                  
              )) ))) ))) ))) ))) ))) )))) )) )) )

gg_t_dwm

# 2015
gg_t_dwm_15 <- gg_t_dwm %>% 
  filter(year_ot == 2015)

# 38 - Trips per day of the week per month in the year 2015
write.csv(gg_t_dwm_15, "38gg_t_dwm_15.csv", row.names = FALSE)

# 2016
gg_t_dwm_16 <- gg_t_dwm %>% 
  filter(year_ot == 2016)

# 39 - Trips per day of the week per month in the year 2016
write.csv(gg_t_dwm_16, "39gg_t_dwm_16.csv", row.names = FALSE)

# 2017
gg_t_dwm_17 <- gg_t_dwm %>% 
  filter(year_ot == 2017)

# 40 - Trips per day of the week per month in the year 2017
write.csv(gg_t_dwm_17, "40gg_t_dwm_17.csv", row.names = FALSE)

# 2018
gg_t_dwm_18 <- gg_t_dwm %>% 
  filter(year_ot == 2018)

# 41 - Trips per day of the week per month in the year 2018
write.csv(gg_t_dwm_18, "41gg_t_dwm_18.csv", row.names = FALSE)

# 2019
gg_t_dwm_19 <- gg_t_dwm %>% 
  filter(year_ot == 2019)

# 42 - Trips per day of the week per month in the year 2019
write.csv(gg_t_dwm_19, "42gg_t_dwm_19.csv", row.names = FALSE)

# 2020
gg_t_dwm_20 <- gg_t_dwm %>% 
  filter(year_ot == 2020)

# 43 - Trips per day of the week per month in the year 2020
write.csv(gg_t_dwm_20, "43gg_t_dwm_20.csv", row.names = FALSE)

# 2021
gg_t_dwm_21 <- gg_t_dwm %>% 
  filter(year_ot == 2021)

# 44 - Trips per day of the week per month in the year 2021
write.csv(gg_t_dwm_21, "44gg_t_dwm_21.csv", row.names = FALSE)

## Hour of the day of per month (45-51) ----

gg_t_hdm <- full_y_2015_2021 %>% 
  mutate(year_ot  = year(starttime)) %>% 
  mutate(month_ot = month(starttime, label = TRUE)) %>% 
  mutate(hour_ot = hour(starttime)) %>% 
  group_by(new_usertype, year_ot, month_ot, hour_ot) %>% 
  summarise(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015 & new_usertype == "annual members", 
             (100*num_obs/2253413), 
             (ifelse(year_ot == 2016 & new_usertype == "annual members", 
              (100*num_obs/2736869), 
             (ifelse(year_ot == 2017 & new_usertype == "annual members", 
              (100*num_obs/2726342), 
              (ifelse(year_ot == 2018 & new_usertype == "annual members", 
              (100*num_obs/2925333),
               (ifelse(year_ot == 2019 & new_usertype == "annual members", 
               (100*num_obs/2936866), 
             (ifelse(year_ot == 2020 & new_usertype == "annual members", 
                (100*num_obs/2051340), 
               (ifelse(year_ot == 2021 & new_usertype == "annual members", 
             (100*num_obs/2446046), 
               (ifelse(year_ot == 2015 & new_usertype == "casual riders", 
             (100*num_obs/930026), 
              (ifelse(year_ot == 2016 & new_usertype == "casual riders", 
             (100*num_obs/858514), 
             (ifelse(year_ot == 2017 & new_usertype == "casual riders", 
               (100*num_obs/799040), 
               (ifelse(year_ot == 2018 & new_usertype == "casual riders", 
                 (100*num_obs/676238), 
                (ifelse(year_ot == 2019 & new_usertype == "casual riders", 
               (100*num_obs/879290), 
               (ifelse(year_ot == 2020 & new_usertype == "casual riders", 
                        (100*num_obs/1278401), 
                            (100*num_obs/1978016)
                                                                                                                  
                 )) ))) ))) ))) ))) ))) )))) )) )) )

gg_t_hdm

# 2015
gg_t_hdm_15 <- gg_t_hdm %>% 
  filter(year_ot == 2015)

# 45 - Trips per hour of the day per month in the year 2015
write.csv(gg_t_hdm_15, "45gg_t_hdm_15.csv", row.names = FALSE)

# 2016
gg_t_hdm_16 <- gg_t_hdm %>% 
  filter(year_ot == 2016)

# 46 - Trips per hour of the day per month in the year 2016
write.csv(gg_t_hdm_16, "46gg_t_hdm_16.csv", row.names = FALSE)

# 2017
gg_t_hdm_17 <- gg_t_hdm %>% 
  filter(year_ot == 2017)

# 47 - Trips per hour of the day per month in the year 2017
write.csv(gg_t_hdm_17, "47gg_t_hdm_17.csv", row.names = FALSE)

# 2018
gg_t_hdm_18 <- gg_t_hdm %>% 
  filter(year_ot == 2018)

# 48 - Trips per hour of the day per month in the year 2018
write.csv(gg_t_hdm_18, "48gg_t_hdm_18.csv", row.names = FALSE)

# 2019
gg_t_hdm_19 <- gg_t_hdm %>% 
  filter(year_ot == 2019)

# 49 - Trips per hour of the day per month in the year 2019
write.csv(gg_t_hdm_19, "49gg_t_hdm_19.csv", row.names = FALSE)

# 2020
gg_t_hdm_20 <- gg_t_hdm %>% 
  filter(year_ot == 2020)

# 50 - Trips per hour of the day per month in the year 2020
write.csv(gg_t_hdm_20, "50gg_t_hdm_20.csv", row.names = FALSE)

# 2021
gg_t_hdm_21 <- gg_t_hdm %>% 
  filter(year_ot == 2021)

# 51 - Trips per hour of the day per month in the year 2021
write.csv(gg_t_hdm_21, "51gg_t_hdm_21.csv", row.names = FALSE)

## Every day of the year (52) ----

gg_t_yday <- full_y_2015_2021 %>% 
  mutate(year_ot  = year(starttime)) %>% 
  mutate(yday_ot = yday(starttime)) %>% 
  select(new_usertype, year_ot, yday_ot) %>% 
  group_by(new_usertype, year_ot, yday_ot) %>% 
  summarise(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015 & new_usertype == "annual members", 
             (100*num_obs/2253413), 
            (ifelse(year_ot == 2016 & new_usertype == "annual members", 
              (100*num_obs/2736869), 
             (ifelse(year_ot == 2017 & new_usertype == "annual members", 
              (100*num_obs/2726342), 
               (ifelse(year_ot == 2018 & new_usertype == "annual members", 
               (100*num_obs/2925333),
              (ifelse(year_ot == 2019 & new_usertype == "annual members", 
             (100*num_obs/2936866), 
               (ifelse(year_ot == 2020 & new_usertype == "annual members", 
              (100*num_obs/2051340), 
               (ifelse(year_ot == 2021 & new_usertype == "annual members", 
             (100*num_obs/2446046), 
             (ifelse(year_ot == 2015 & new_usertype == "casual riders", 
              (100*num_obs/930026), 
               (ifelse(year_ot == 2016 & new_usertype == "casual riders", 
              (100*num_obs/858514), 
              (ifelse(year_ot == 2017 & new_usertype == "casual riders", 
              (100*num_obs/799040), 
               (ifelse(year_ot == 2018 & new_usertype == "casual riders", 
               (100*num_obs/676238), 
                (ifelse(year_ot == 2019 & new_usertype == "casual riders", 
                (100*num_obs/879290), 
                (ifelse(year_ot == 2020 & new_usertype == "casual riders", 
                           (100*num_obs/1278401), 
                             (100*num_obs/1978016)
                                                                                                                  
                 )) ))) ))) ))) ))) ))) )))) )) )) )

gg_t_yday

# 52 - Number of trips for day of the year
write.csv(gg_t_yday, "52gg_t_yday.csv", row.names = FALSE)

## Types of bikes used (53) ----

# Types of bikes used in 2020 and 2021

gg_by_u <- full_y_2015_2021 %>% 
  filter(!is.na(rideable_type)) %>% 
  mutate(year_ot = year(starttime)) %>% 
  group_by(rideable_type, new_usertype, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_per_year = 
           ifelse(year_ot == 2020 & new_usertype == "annual members",
               100*num_obs/2051340,
               ifelse(year_ot == 2020 & new_usertype == "casual riders",
                  100*num_obs/1278401,
                    ifelse(year_ot == 2021 & new_usertype == "annual members",
                                100*num_obs/2446046,
                                100*num_obs/1978016)  ) )) %>% 
  arrange(year_ot, rideable_type)

gg_by_u

# 53 - Types of bikes used in 2020 and 2021
write.csv(gg_by_u, "53gg_by_u.csv", row.names = FALSE)

# Types of bikes used in 2020 and 2021 (year and month)

full_y_2015_2021 %>% 
  filter(!is.na(rideable_type)) %>% 
  mutate(year_ot = year(starttime)) %>% 
  mutate(month_ot = month(starttime, label = TRUE)) %>% 
  group_by(rideable_type, new_usertype, year_ot, month_ot) %>% 
  summarize(num_obs = n()) %>% 
  arrange(year_ot, rideable_type)

## Age (54) ----

# Age of annual members from 2015 to 2019

gg_age_15_19 <- full_y_2015_2021 %>% 
  filter(!is.na(age)) %>% 
  mutate(year_ot = year(starttime)) %>% 
  group_by(age, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015, 
                  (100*num_obs/2253413), 
                  (ifelse(year_ot == 2016, 
                          (100*num_obs/2736869), 
                          (ifelse(year_ot == 2017, 
                                  (100*num_obs/2726342), 
                                  (ifelse(year_ot == 2018, 
                                          (100*num_obs/2925333),
                                          (100*num_obs/2936866
                                          )) )) )) ))) %>% 
  arrange(desc(year_ot), desc(num_obs))

gg_age_15_19

# To obtain the sum of the %
gg_age_15_19 %>% 
  filter(year_ot == 2019) %>% 
  group_by(percent_of_per_year) %>% 
  summarize(sum(percent_of_per_year)) %>% 
  summarize(sum(`sum(percent_of_per_year)`) )

# 54 - Age of annual members from 2015 to 2019
write.csv(gg_age_15_19, "54gg_age_15_19.csv", row.names = FALSE)

# Between 25 and 35 years old            
gg_age_15_19 %>% 
  filter(year_ot == 2019) %>% 
  filter(age >= 25 & age <=35) %>% 
  group_by(percent_of_per_year) %>% 
  summarize(sum(percent_of_per_year)) %>% 
  summarize(sum(`sum(percent_of_per_year)`) )

# 2015 53.5%
# 2016 52.8%
# 2017 53.1%
# 2018 52.7%
# 2019 53.6%

## Gender (55) ----

# Distribution of gender of annual members from 2015 to 2019

gg_gender <- full_y_2015_2021 %>% 
  filter(!is.na(gender)) %>% 
  mutate(year_ot = year(starttime)) %>% 
  group_by(gender, year_ot) %>% 
  summarize(num_obs = n()) %>% 
  mutate(percent_of_per_year = 
           ifelse(year_ot == 2015, 
                  (100*num_obs/(1685968+567339)), 
                  (ifelse(year_ot == 2016, 
                          (100*num_obs/(2046798+689763)), 
                          (ifelse(year_ot == 2017, 
                                  (100*num_obs/(2037097+688637)), 
                                  (ifelse(year_ot == 2018, 
                                          (100*num_obs/(2206765+709632)),
                                          (100*num_obs/(2187719+726399)
                                          )) )) )) ))) %>% 
  arrange(year_ot)

gg_gender

# 55 - Distribution of gender of annual members from 2015 to 2019
write.csv(gg_gender, "55gg_gender.csv", row.names = FALSE)








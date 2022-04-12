Cyclistic Case Study 2015 to 2021
================
Juan Cordova
4/7/2022

Case Study project

This project was carried out as a data analysis exercise with a real
dataset, The data and idea of the project were part of the **Google Data
Analysis course** , which is used to obtain the **Data Analysis
Certificate** from Google. The analysis, visualizations, conclusions and
recommendations were done by me.

The data is provided by Bikeshare (Divvy), a company who operates the
City of Chicago’s bike-sharing service, (you can see the license to use
the data ***here***).

For this exercise, the name Cyclistic will be used as the fictitious
company that provides the data.

## Case Study: Cyclistic Bike-Share from 2015 to 2021

### Scenario

Being a junior data analyst working in the marketing team at Cyclistic,
a share-bike company in the city of Chicago, you now have to develop the
following project. The director of marketing believes that the future of
the company depends on maximizing the number of annual members.
Therefore, it is necessary to know how casual riders and annual members
use the Cyclistic service, so that their differences are known in order
to carry out a marketing strategy campaign that seeks to convert casual
riders into annual members.

The team leader asks you to answer the following question:

-   How do annual members and casual riders use Cyclistic bikes
    differently?

### Business Task

**Problem:** *How do annual members and casual riders use Cyclistic
bikes differently?*

What is intended?

-   Learn how casual cyclists and annual subscribers use the bike-share
    service

How would it be achieved?

-   By Analyzing the data provided by Cyclistics

What is the analysis looking for?

-   Find discoveries between the differences between the two types of
    users of the system, in order to make data-driven decisions

What will be generated?

-   A summary of the analysis

-   Supporting visualizations and key findings

-   Main recommendations based on the analysis done that will respond to
    the question posed.

### Data sources used

All the files the company Cyclistics provided were downloaded, sorting
the data by year, creating a file containing all the data for each year,
from 2015 to 2021

All these data were organized in a folder in chronological order. First
Using the **rbin()** function, all the tables of each year were joined,
obtaining seven files referring to the years from 2015 to 2021. These
files then went through a cleaning process in order to be united into a
single file that encompasses the seven years

### Documentation of data cleaning

**Process:** *Check each column, that what it should be is fulfilled,
checking the data type and if there are errors correct them*

The files of each year will be structured in the same way, having in
them the same number of columns, with the same type of data, even when
there are discrepancies in some years where there are columns with
information that in others do not, that column will simply be added to
the year that does not have that information and it will be filled with
blank values (NA).

**Documentation**

The files will be arranged in 13 columns:

-   6 columns with a character data type: *trip_id, rideable_type,
    from_station_name, to_station_name, new_usertype, gender*

-   5 columns with a numeric data type: *bikeid, from_station_id,
    to_station_id, tripduration, age*

-   2 columns with POSIXct data type (date format): *starttime,
    stoptime*

**Cleaning** Data cleaning was carried out so that the columns meet the
following specifications:

***trip_id:*** id number (2015 to 2019) or id code (2020 to 2021) of the
trip

***rideable_type:*** type of bike used (only applies for 2020 and 2021)

***from_station_name:*** name of the station where the trip starts

***to_station_name:*** name of the station where the trip ends

***bikeid:*** id of the bike used (only applies for 2020 and 2021)

***from_station_id:*** id of the station where the trip starts

***to_station_id:*** id of the station where the trip ends

***tripduration:*** duration of the trip in minutes, only trips lasting
between 1 minute an 1440 minutes (this is equivalen to 1 day) were taken
into account.

***new_usertype:*** The usertype was changed to only two; *“Customer”*
and *“Dependent”* to **“Casual Riders”** and *“Subscriber”* to **“Annual
Member”**

***age:*** This information was only provided by annual riders from
years 2015 to 2019, and only ages between 13 and 100 years old where
taken into account

***gender:*** This information was only provided by annual riders from
years 2015 to 2019.

***starttime:*** star date of the trip showing the year, month, day,
hour and second.

***stoptime:*** end date of the trip showing the year, month, day, hour
and second.

-   **Stations of the bike share system**

From the stations that were taken into account, 688 diferent stations
were found, which were cleaned and organized so that they are
homogeneous in all years. From year to year the name of the stations
changed, as different names has the same id station, most of it where
small changes, so only one name was taken into account for each station
id, which resulted in 668 stations with unique id and names.

### Summary of the data analysis with visualizations

Analysis of the Cyclistic bikes-share’s dataset from 2015 to 2021

#### 1 Load of packages and file upload

-   Packages used

``` r
#load paquetes
library(dplyr)
library(tidyverse)
library(readr)
library(skimr)
library(hablar)
library(lubridate)
library(ggplot2)
library(scales)
```

#### 2 Data preview

-   Preview of the dataset used from 2015 to 2021

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/skim_image.png)<!-- -->

-   Show first 5 rows

<!-- -->

    ##   trip_id bikeid rideable_type           starttime            stoptime
    ## 1 4413171    528            NA 2015-01-01 00:07:00 2015-01-01 00:26:00
    ## 2 4413170   2147            NA 2015-01-01 00:08:00 2015-01-01 00:10:00
    ## 3 4413169    713            NA 2015-01-01 00:08:00 2015-01-01 00:13:00
    ## 4 4413172   1586            NA 2015-01-01 00:10:00 2015-01-01 00:16:00
    ## 5 4413175   2705            NA 2015-01-01 00:13:00 2015-01-01 00:29:00
    ##                from_station_name from_station_id            to_station_name
    ## 1      LaSalle St & Jackson Blvd             283  Blue Island Ave & 18th St
    ## 2     Clarendon Ave & Gordon Ter             312 Clarendon Ave & Junior Ter
    ## 3     Clarendon Ave & Gordon Ter             312 Sheridan Rd & Montrose Ave
    ## 4 Lake Shore Dr & Wellington Ave             157     Broadway & Belmont Ave
    ## 5     Lincoln Ave & Waveland Ave             257   Lincoln Ave & Leavitt St
    ##   to_station_id tripduration   new_usertype age gender
    ## 1           129    19.000000 annual members  28   Male
    ## 2           245     1.850000 annual members  46   Male
    ## 3           231     5.066667 annual members  41   Male
    ## 4           296     6.333333 annual members  26   Male
    ## 5           243    15.766667  casual riders  NA   <NA>

-   Show last 5 rows

<!-- -->

    ##            trip_id bikeid rideable_type           starttime            stoptime
    ## 1 297D1BD291D07BF3     NA electric_bike 2021-12-31 23:59:48 2022-01-01 00:14:42
    ## 2 B0466FF51982DE4B     NA electric_bike 2021-12-31 23:59:39 2022-01-01 00:21:08
    ## 3 CE1BE016BCE85CCB     NA electric_bike 2021-12-31 23:59:27 2022-01-01 00:32:34
    ## 4 B5AEBEF3B5F41C77     NA electric_bike 2021-12-31 23:58:45 2022-01-01 00:47:07
    ## 5 7B3D0E8AB0842D5E     NA  classic_bike 2021-12-31 23:58:21 2022-01-01 00:46:56
    ##          from_station_name from_station_id          to_station_name
    ## 1  Dearborn St & Monroe St              49       Clark St & Lake St
    ## 2          Millennium Park              90   Michigan Ave & 14th St
    ## 3 Clark St & Congress Pkwy              50 Clark St & Congress Pkwy
    ## 4   Michigan Ave & Lake St              52   Clinton St & Tilden St
    ## 5   Michigan Ave & Lake St              52   Clinton St & Tilden St
    ##   to_station_id tripduration   new_usertype age gender
    ## 1            38     14.90000  casual riders  NA     NA
    ## 2           168     21.48333 annual members  NA     NA
    ## 3            50     33.11667  casual riders  NA     NA
    ## 4            68     48.36667  casual riders  NA     NA
    ## 5            68     48.58333  casual riders  NA     NA

#### 3 Distribution of users and trip duration

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### 4 Analysis of the use of the stations

##### Start stations

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

-   Start stations distribution per year and user type

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

##### End stations

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

-   End stations distribution per year and user type

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

##### Full trip routes

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

-   Trip routes distribution per year and user type

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

#### 5 Analysis by time

##### Trips per month from 2015 to 2021

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
# Percentage of casual riders from Jun, Jul, Aug
# Jun - Jul - Aug
# 2015 - 57.7%
# 2016 - 51.0%
# 2017 - 57.3%
# 2018 - 57.2%
# 2019 - 56.0%
# 2020 - 54.5%
# 2021 - 49.6%
```

##### Trips per day of the week according to the type of user

-   General from 2015 to 2021

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
# Percentage of casual riders from Sat - Sun
# 2015 - 47.9%
# 2016 - 50.4%
# 2017 - 51.5%
# 2018 - 45.3%
# 2019 - 43.0%
# 2020 - 42.2%
# 2021 - 42.6%
```

-   Day of the week per month each year

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

##### Hour of the day in each year

-   General from 2015 to 2021

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
# Percentage of Annual member's peak hours - 8hrs and 17hrs
# 2015 - 21.9%
# 2016 - 21.9%
# 2017 - 22.5%
# 2018 - 22.5%
# 2019 - 23.0%
# 2020 - 17.1%
# 2021 - 16.7%
```

-   Hour of the day of per month each year

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

##### Number of trips for day of the year

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

#### 6 Bikes used in 2020 and 2021

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

#### 7 Aged from 2015 to 2019 (annual members)

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
# Percentage between 25 and 35 years old 
# 2015 - 53.5%
# 2016 - 52.8%
# 2017 - 53.1%
# 2018 - 52.7%
# 2019 - 53.6%
```

#### 8 Gender from 2015 to 2019 (annual members)

![](Cyclistic_CS_2015_2021_g_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

### Findings pre-pandemic from 2015 to 2019

-   The behavior all the users is very similar in all the pre-pandemic
    years

-   Casual riders double in average travel time and in some years they
    triple it, but in average they are only a quarter of all trips made.

-   Casual riders concentrate between 18% an 23% of their trips on 5
    departure stations (the top 5 start stations), compared to the
    annual riders who only concentrate between 6% an 7.5%. Also, in 5
    arrival stations (the top 5 end stations), the casual riders
    concentrate between 20% and 25% of the trips compared to the annual
    riders who only concentrate betweem 6 an 7.5%. This confirms that
    casual riders concentrate on certain stations while annuals use the
    system more widely, without concentrating on specific stations.

-   Casual riders concentrate between 3% an 4.6% of their trips on 5
    rutes (complete trips), compared to the annual members who only
    concentrate between 0.4 an 0.6%. This shows that casual riders tend
    to have specific routes

-   Annual members use the system at work start and end times, they have
    peak hours, they have peak hours, 8hr and 17hr specifically,
    concentrating between 21% and 23% of their trips in that hours,
    while casual riders mainly use the system in the afternoon between
    14hrs an 17hrs with and average of 38% of their trips.

-   Users who provide age (annual members) have a range that oscillates
    mainly between 25 and 35 years (with an average concentration of
    53.5%)

### Findings with the pandemic from 2020 to 2021

-   There is a clear change in the behavior of the user when the pandeic
    arrives

-   The mean trip duration of annual members is the same as in previous
    years, but it shows a drop in the mean duration on casual riders,
    which indicates that in 2021 the behavior of the user types tend to
    be the same.

-   The distribution of user types changes, the percentage of casual
    riders grows compared to the previous years, to the point where they
    reach a 45% of user in 2021.

-   There is drop in the top 5 Casual riders departure estations
    concentrating now between 7% an 9.3% of their trips, as well as the
    annual riders who now only concentrate between 4% an 4.3%. This also
    happens in the top 5 arrival stations, the casual riders concentrate
    between 7.5% and 9.8% of the trips compared to the annual riders who
    now concentrate between 4.2% an 4.4%. This confirms that a changed
    was made on pandemic years and now the users don’t concentrate on
    specific stations.

-   Also a drop is shown in the Casual riders top 5 trip routes
    concentrating around the 1.8% of their routes made, as well as the
    annual members who now only concentrate between 0.2% an 0.6%.
    Showing the same tendency, as now the user don’t have specifics
    routes.

-   The way the users annual members and casual riders use the system in
    the hours of the day changed notably, annual members still have
    peaks at 8hrs and 17hrs but nos they reduced from and average of 22%
    to and average of 16.8%, and now the behavior of the casual riders
    tend to be the same as the annual members.

### Overall findings from all the years (from 2015 to 2021)

-   Casual riders clearly use the system in the summer, with a rate
    between 49% and 57% of their trips made between August and
    September. A small change is noticeable in the pandemic years but
    the trend does not change at all.

-   Casual riders use the system mainly on weekends, concentrating
    between 42% to 52% of their trips between Saturday and Sunday,
    maintaining the same trend in all years (2015 to 2021), while an
    opposite behavior is shown in the pre-pandemic years (2015 to 2019)
    in the annual members who use the system during the week, having
    80.2% of their trips between Monday to Friday with a distribution of
    16% per day in that interval, and the years of pandemic the annual
    members changes their trend and now they made their trips more even
    in all the days of the week.

### Insights

To answer the question that was raised: ***How do annual members and
casual riders use Cyclistic bikes differently?*** This question has two
answers, which depend on whether the pre-pandemic years or the pandemic
years are analyzed.

**Pre-pandemic years (2015 to 2019)**

-   There is a clear greater use of the service in the summer months,
    but this is extremely noticeable in casual members who make more
    than half of their trips in the months of June to August.

-   The casual riders are concentrated in specific stations, either
    departure or arrival, concentrating almost 30% in 10 departure
    stations and 30% in 10 arrival stations, A comparison of the annual
    members that have an almost homogeneous distribution throughout the
    service stations both in arrival and departure. This same phenomenon
    happens in routes (complete trips).

-   It is clearly marked that casual riders use the service mainly on
    weekends, Saturday and Sunday, contrary to annual members who use
    the system on weekdays.In the same way, there is a clear difference
    in the hours of the day in which the service is used, the annual
    members use the service mainly in the hours of departure and arrival
    of work, while the casual riders use them in the afternoon between
    12 and 6 p.m.

-   This shows the clearest difference, the annual riders use the system
    as a transportation system, mainly to get to and from work, while
    the casual riders use the system as a leisure activity, using the
    service on weekends and in summer

**Pandemic years**

-   The type of user **casual riders** has a noticeable rise in trips
    made, and specifically in the year 2021, which is the year with the
    highest number of trips made. This may indicate two things. One:
    ***many annual members changed to casual riders due to the
    circumstances of the pandemic, which explains why now the casual
    riders behave like annual members from years past***. Two: ***casual
    riders grew in 2021 because people who did not use the system in
    previous years (or not as much), now use it as a transportation
    alternative due to the pandemic but do they not want to be
    subscribers or paid to be annual members***

### Recommendations

For the marketing campaign that seeks to convert casual riders to annual
members, you should focus on users who use the system as a recreational
activity, looking for an offer in this regard.

In addition, it can clearly be indicated that the marketing campaign
should focus on a schedule from 12:00 p.m. to 6:00 p.m., on Saturdays
and Sundays, but above all, have a strong presence in the summer months
of June, July, and August.

There has to be a campaign for people to return from casual riders to
annual members, offering discounts based on trends annual riders had in
the pre-pandemic years, so that people who use the system as casual
riders but behave in this way see the benefit of making a change and
dare to do so.

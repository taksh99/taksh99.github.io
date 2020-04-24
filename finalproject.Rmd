---
title: "Final Project"
author: "Taksh Anand and Sadanidan Balamurugan"
date: "3/27/20"
output: 
    html_notebook:
    fig_height: 6
    fig_width: 10
---

1.Guiding Question

What types of crimes are prominent in certain areas of the city and how likely are they to occur in different times of the day?

2.

This particular data set was found out in kaggle which contains numerous data sets.
It was made by Ankur jain a student in north eastern university.
It is based on the crimes commited of the year 2015-2018. It was collected to find out what crimes where commited between the years of 2015 and 2018 and when they were commited.
It has 2,60,760 number of rows or cases and each case represents a crime at a particular location at a particular time of a day.
Some of the many variables that we plan on using are Incident_number, year,occured_on_date,street,etc.



### Setup



```{r}

rm(list = ls())



library(lubridate)

library(tidyverse)

library(mosaic)

library(rvest)

library(tidyr)



library(ggplot2)


library(DataComputing)
#view(ZipGeography)

```

### Loading Dataset 

```{r}

# Loading Data table: Crimes in Boston



library(readr)
#loading primary data set
link="T:\\Spring20\\stat184\\boston-crime-data\\crime.csv"
getDataset<- function(link){
  c<- read_csv(link)

  
}
crime<-getDataset(link)
crime
#inspecting primary data set

View(c)

top_ten<-crime%>%head(10)
top_ten
#loading secondary data set
 data("ZipGeography")
 



```



### Data Wrangling u



``` {r}



# Creating a new table with necessry variables 



crime_2018 <- 

  crime %>%

  filter(YEAR == "2018") %>%

  select(INCIDENT_NUMBER, OFFENSE_CODE, OFFENSE_CODE_GROUP, OFFENSE_DESCRIPTION, MONTH, UCR_PART)



view(crime_2018)





crime_2015 <- 

  crime %>%

  filter(YEAR == "2015") %>%

  select(INCIDENT_NUMBER, OFFENSE_CODE, OFFENSE_CODE_GROUP, OFFENSE_DESCRIPTION, MONTH, UCR_PART)



view(crime_2015)



```

  

  

``` {r}



# Number of motor vehicle accidents in 2015



crime_2015 %>%

  filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>%

  summarise(count = n())



```



``` {r}

# Number of motor vehicle accidents in 2018



crime_2018 %>%

  filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>%

  summarise(count = n())

  

```



``` {r}

# Highest crimes in each month arranged in descending order



crime_2015 %>%

  group_by(MONTH) %>%

  summarise(count = n()) %>%

  arrange(desc(count))



crime_2018 %>%

  group_by(MONTH) %>%

  summarise(count = n()) %>%

  arrange(desc(count))



```



``` {r}

# Grouping the crimes by UCR part



crime_2015 %>%

  group_by(UCR_PART) %>%

  summarise(count = n())



crime_2018 %>%

  group_by(UCR_PART) %>%

  summarise(count = n())



```





``` {r}

# Most popular crime in 2015



sort(table(crime_2015$OFFENSE_CODE_GROUP), decreasing = TRUE)



```



``` {r}

# Most popular crime in 2018



sort(table(crime_2018$OFFENSE_CODE_GROUP), decreasing = TRUE)



```



``` {r}

# Crimes grouped by the area



sort(table(crime$DISTRICT), decreasing = TRUE)



```



B2 district has experienced the most crimes from 2015-18 





### Joining two tables





``` {r}
newjoin <- 

  crime_2015 %>%

  left_join(crime_2018) %>%

  group_by(OFFENSE_CODE_GROUP, MONTH)
```




### Amalyzing similar data using regex


``` {r}
pattern <- "ST$"
streetsonly <-
  crime %>% 
  filter(  grepl(pattern, STREET),  #Filtering addresses with the given street     
  )
streetsonly
```

``` {r}
pattern <- "RD$"
extract_addr<- function(pattern){
  roadsonly <-
  crime %>% 
  filter(  grepl(pattern, STREET),  #Filtering addresses with the given road    
  )
  roadsonly
}


extract_addr(pattern)
```






### Plotting graph



``` {r}

# Creating a new table for with fewer variables to make graph 



cr15 <- 

  crime %>%

  filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response")



view(cr15)



```



``` {r}



crime %>%

  ggplot(aes(x = MONTH)) +

  geom_density(color = "black", fill = "gray", alpha = 1) +

  xlab("Month") +

  xlim(1,12)

  

  

  

```



This density graph shows that the crime in general, is high between June and September







``` {r}

crime %>%

  ggplot(aes(x=MONTH, colour = DISTRICT)) +

  geom_freqpoly(binwidth=1)





```

This graph confirms B2 has the highest crime rate and months 5-9 has an increase in crime rate.





``` {r}



crime %>%

  ggplot(aes(x = MONTH, fill = UCR_PART)) +

  geom_bar()





```

 The graph shows that Part 3 almost consists of 50% of the crimes.





``` {r}



crime %>%

  ggplot(aes(x = YEAR)) +

  geom_bar()





```



The bar graph shows that the crimes increase in the year 2016 and gradually decreasing in the year 2018





``` {r}



crime %>%

  ggplot(aes(x = HOUR)) +

  geom_bar() 

  

```



The graph shows that more crimes occur when it gets dark outside. The hours start from 0 because it is in 24 hour format.



``` {r}

crime %>%

  ggplot(aes(x = MONTH, fill = UCR_PART)) +

  geom_bar() +

  xlim(1,12) +

  facet_grid(~ YEAR)



```

  

The above graph shows UCR parts based on the months each year (2015 - 2018)



## Conclusion



After a performing a few queries on the dataset, district B2 had the most crimes occured and Motor Vehicle Accidents was the most reported crime according to the data. The findings from this project can help make people decisions on which community is safer and where to live.


## Other Data Sources

ZipGeography dataset can be used to pinpoint the location of a crime in a given zipcode

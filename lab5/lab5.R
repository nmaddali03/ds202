library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate) # Time
library(scales)
library(readxl)

acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)
person <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)

#Question 1. Create a data frame containing the persons who are fatally hurt in the accidents 
#(see FARS manual and look up variable INJ_SEV
fatal <- person %>% filter(INJ_SEV == 4)

#Question 2. Create a data frame containing the most dangerous vehicle make in each state. 
#The number of persons fatally hit in the vehicle make is used to assess the (non-)safety of a 
#make. Make sure to handle the missing values appropriately. (look up variable MAKE)
danger <- fatal %>% group_by(STATE, MAKE) %>%
  summarise(numfatal = n())
danger <- danger %>% na.omit() %>%
  group_by(STATE) %>%
  filter(numfatal == max(numfatal))

#Question 3. Create a map, and label each state with the most dangerous vehicle. Discuss the 
#definition of the most dangerous vehicle, and what you find from the map. (Hint: Read the 
#description for the STATE and COUNTY columns in the FARS manual. The state & county codes are 
#Geographic Locator Codes (GLCs) from the General Services Administration's (GSA) publication. 
#Use readxl::read_xlsx to read in the GLCs.)
GLCs <- readxl::read_xlsx("FRPP_GLC_-_United_StatesNov42021.xlsx")
states <- map_data('state')
GLCs$'State Name' <- tolower(GLCs$'State Name')
GLCs$'State Code' <- as.numeric(GLCs$'State Code')
labels <- states %>%
  group_by(region) %>%
  summarise(long= mean(long, na.rm = TRUE), lat= mean(lat, na.rm = TRUE))
labels <- left_join(labels, GLCs, by = c("region" = "State Name"))
labels <- left_join(labels, danger, by = c("State Code" = "STATE"))
labels$MAKE <- factor(labels$MAKE, levels = c(12, 20, 49, 37),
                      labels = c("Ford", "Chevrolet", "Toyota", "Honda"))
states %>% ggplot() +
  geom_polygon( aes(x=long, y=lat, group=group),
                color="grey", fill="blue" ) + 
  geom_text(data=labels, aes(label = MAKE, x=long, y=lat), color="black", size=3)

#Question 4. Join the accident and person table (work out which variable(s) to use)
joinedTables <- inner_join(person, acc, by="ST_CASE")

#Question 5. Tally the number of accidents by day of the week (DAY_WEEK), hour of the day 
#(HOUR) and gender (SEX). Visualize the results and explain what you find.
tallied <- joinedTables %>%
  filter(SEX == 1 || SEX == 2) %>%
  group_by(ST_CASE) %>%
  summarise(avgSex = mean(SEX))
tallied$sexStr <- ifelse(tallied$avgSex == 1, 'Men', ifelse(tallied$avgSex == 2, 'Women', 'Both'))
accident <- tallied %>%
  select(ST_CASE, sexStr) %>%
  inner_join(acc, by='ST_CASE')
grouped <- accident %>%
  filter(HOUR <= 24) %>%
  filter(DAY_WEEK != 9) %>%
  group_by(sexStr, HOUR, DAY_WEEK) %>%
  summarise(numAccidents = n()) %>%
  arrange(desc(numAccidents))
ggplot(grouped,aes(x=HOUR, y=numAccidents)) +
  geom_bar(stat='identity') +
  facet_grid(sexStr~DAY_WEEK) +
  xlab("Hours of the day") +
  ylab("Total number of accidents")

#Question 6. Now plot a choropleth map of the number of deaths on a county level. Also explain 
#what you find.
df <- readxl::read_xlsx("FRPP_GLC_-_United_StatesNov42021.xlsx")
states <- map_data('state')
counties <- map_data('county')

var <- c("STATE", "COUNTY", "FATALS")
fatals <- acc[var]
df$'State Code' <- as.numeric(df$`State Code`)
df$'City Code' <- as.numeric(df$`City Code`)
df$'County Code' <- as.numeric(df$`County Code`)
fatals <- fatals %>% group_by(STATE, COUNTY) %>%
  summarize(FATALS = sum(FATALS))
fatals <- fatals %>% left_join(df, by=c('COUNTY' = 'County Code', 'STATE' =
                                          'State Code'))
var2 <- c("State Name", "County Name", "FATALS")
fatals <- fatals[var2]
fatals <- unique(fatals)
fatals$`State Name` <- tolower(fatals$`State Name`)
fatals$`County Name` <- tolower(fatals$`County Name`)
fatals <- fatals %>% right_join(counties, by=c('County Name' = 'subregion',
                                               'State Name' = 'region'))
ggplot(fatals, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) +
  geom_polygon(aes(group=group, fill=FATALS))

deaths <- accident %>%
  inner_join(fatal %>%
               select(INJ_SEV, ST_CASE),
             by=c("ST_CASE")) %>%
  summarize(INJ_SEV, ST_CASE, LATITUDE, LONGITUD, STATE)

ggplot(states, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) + geom_point(aes(x=LONGITUD, y=LATITUDE),
                                              data=deaths %>% filter(INJ_SEV == 4),
                                              color = 'lightgreen',alpha=0.2,size=0.02)+
  xlim(-130,-60)+ylim(20,50)+coord_map()+labs(x="Longitude",y="Latitude",title="Number of Deaths in a County")

#Question 7. (Optional) Is summer or winter more dangerous? Does this depend on states? Explore 
#and explain.
Winter <- acc %>%
  filter(MONTH < 5 | MONTH > 10)
Summer <- acc %>%
  filter(MONTH > 5 & MONTH < 10)
var3 <- c("State Name", "State Code")
df2 <- df[var3]
df2 <- unique(df2)
winterFatal <- Winter %>%
  group_by(STATE) %>%
  summarize(FATALS = sum(FATALS))
summerFatal <- Summer %>%
  group_by(STATE) %>%
  summarize(FATALS = sum(FATALS))
summerFatal <- summerFatal %>% left_join(df2, by=c('STATE' = 'State Code'))
summerFatal$`State Name` <- tolower(summerFatal$`State Name`)
summerFatal <- summerFatal %>% right_join(states, by=c('State Name' = 'region'))
var2 <- c("STATE", "FATALS", "long", "lat", "group")
summerFatal <- summerFatal[var2]
ggplot(summerFatal, aes(x=long, y=lat, fill = FATALS)) +
  geom_polygon(aes(group = group)) +
  labs(title = "Car Accidents in the Summer by State")
winterFatal <- winterFatal %>% left_join(df2, by=c('STATE' = 'State Code'))
winterFatal$`State Name` <- tolower(winterFatal$`State Name`)
winterFatal <- winterFatal %>% right_join(states, by=c('State Name' = 'region'))
winterFatal <- winterFatal[var2]
ggplot(winterFatal, aes(x=long, y=lat, fill = FATALS)) +
  geom_polygon(aes(group = group)) +
  labs(title = "Car Accidents in the Winter by State")
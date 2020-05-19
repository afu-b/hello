## Author: Aileen Kate Fu (afu@brandeis.edu)
## Date:   2020/5/6

# Preliminary
options(scipen=100) # cleans up presentation out of scientific notation 
options(digits=2) # number of decimals 

# Load Packages
library('covid19.analytics')
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(ggplot2)
library(ggExtra)
library(maps)
library(mapproj)
library(tidyr)
library(lubridate)
library(gganimate)
library(gifski)
library(av)
library(gapminder)

## 1) World Overview
# Get latest data (source: JHU/CCSE repository)
tsc <- covid19.data(case='ts-confirmed')
# Export data for the use of Tableau analysis
write.csv(tsc,'latestcovid.csv')
# 3 plots From Tableau analysis

## 2) Top Countries Evaluation
# Load Data
data <- read.csv(file.choose(),header = T) #COVIDlatestdata
head(data)
str(data)
# Handling dates
head(data$date_confirmation)
datanew <- data %>% 
  mutate(date_confirmation= dmy(date_confirmation))
head(datanew$date_confirmation)
# Extract day and month
datanew$day <- day(datanew$date_confirmation)
datanew$month <- month(datanew$date_confirmation)
# Complete data
new <- data.frame(complete(new,day,country,
                           fill=list(count=0))) #complete
# Find top 5 countries
datanew_country <- datanew %>% group_by(country) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

## Plot 2: Animated time series plot for top 5 countries
# a. Italy 
datanew %>% filter(country=='Italy') %>% 
  group_by(date_confirmation) %>% 
  summarise(count=n()) %>% 
  mutate(cuml=cumsum(count)) %>% 
  ggplot(aes(x=date_confirmation,y=cuml))+
  geom_line(color='#FF9999')+
  geom_point(size=1.5)+
  geom_area(fill='#FF9999')+
  theme_bw()+
  ggtitle('Italy Daily Cumulative Cases')+
  labs(x='',y='')+
  transition_reveal(cuml)
# Save animation
anim_save('italy.CumlPlot')

# b. US
datanew %>% filter(country=='United States') %>% 
  group_by(date_confirmation) %>% 
  summarise(count=n()) %>% 
  mutate(cuml=cumsum(count)) %>% 
  ggplot(aes(x=date_confirmation,y=cuml))+
  geom_line(color='#FF9999')+
  geom_point(size=1.5)+
  geom_area(fill='#FF9999')+
  theme_bw()+
  ggtitle('US Daily Cumulative Cases')+
  labs(x='',y='')+
  transition_reveal(cuml)
# Save animation
anim_save('us.CumlPlot')

# c. China
datanew %>% filter(country=='China') %>% 
  group_by(date_confirmation) %>% 
  summarise(count=n()) %>% 
  mutate(cuml=cumsum(count)) %>% 
  ggplot(aes(x=date_confirmation,y=cuml))+
  geom_line(color='#FF9999')+
  geom_point(size=1.5)+
  geom_area(fill='#FF9999')+
  theme_bw()+
  ggtitle('China Daily Cumulative Cases')+
  labs(x='',y='')+
  transition_reveal(cuml)
# Save animation
anim_save('china.CumlPlot')

# d. UK
datanew %>% filter(country=='United Kingdom') %>% 
  group_by(date_confirmation) %>% 
  summarise(count=n()) %>% 
  mutate(cuml=cumsum(count)) %>% 
  ggplot(aes(x=date_confirmation,y=cuml))+
  geom_line(color='#FF9999')+
  geom_point(size=1.5)+
  geom_area(fill='#FF9999')+
  theme_bw()+
  ggtitle('UK Daily Cumulative Cases')+
  labs(x='',y='')+
  transition_reveal(cuml)
# Save animation
anim_save('uk.CumlPlot')

# e. Spain
datanew %>% filter(country=='Spain') %>% 
  group_by(date_confirmation) %>% 
  summarise(count=n()) %>% 
  mutate(cuml=cumsum(count)) %>% 
  ggplot(aes(x=date_confirmation,y=cuml))+
  geom_line(color='#FF9999')+
  geom_point(size=1.5)+
  geom_area(fill='#FF9999')+
  theme_bw()+
  ggtitle('Spain Daily Cumulative Cases')+
  labs(x='',y='')+
  transition_reveal(cuml)
# Save animation
anim_save('spain.CumlPlot')

# Plot 3. Animated bar plots (Feb/March) for top 5 countries
# Data for bar plot
new <- datanew %>% filter(country=='Italy'|
                            country=='United States'|
                            country=='China'| 
                            country=='United Kingdom'| 
                            country=='Spain') %>% 
  filter(month==2|month==3) %>% 
  group_by(country,month) %>% 
  summarise(count=n())
# Bar plot
p <- new %>% ggplot(aes(x=country,
                        y=count,
                        fill=country)) %>% +
  geom_bar(stat='identity')+
  geom_point(size=1.5)+
  theme_bw()+
  guides(fill=F) #remove guide name
# Animated bar plot by country
p + transition_states(count)+
  labs(title='Animated Bar Plot for Top Countries (Feb/March)')+
  shadow_mark()+
  enter_grow()
# Save animation
anim_save('country.BarlPlot')

# Plot 4. Gender Evaluation across contries
# performed by tableau

## 3) US Evaluation
## Plot 4:US geographic pattern
# US map
s <- map_data('state')
# US data
us <- datanew %>% filter(country=='United States') 
us <- us %>% group_by(province) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
# Merge data
us$province <- tolower(us$province)
us_data <- merge(s, us,
              by.x='region',
              by.y='province')
# Plot map
ggplot(us_data,
       aes(x=long,y=lat,group=group,fill=count))+
  geom_polygon(color='gray')+
  coord_map('polyconic')+
  scale_fill_gradient2(low='pink',high='#CB454A')+
  theme_void()+
  ggtitle('COVID cases in US')

## Plot 5: States composition
# treemap by Tableau analysis


## Plot 6: Time trends in US states
us <- datanew %>% filter(country=='United States') 
us <- us %>% group_by(province,date_confirmation) %>% mutate(count=n())
ggplot(us,
            aes(x=date_confirmation, y=count,
                size=count, color=province))+
  geom_line(show.legend = F,alpha=0.7)+
  labs(x='Confirmation date',
       y='Cases')+
  scale_size(range=c(2,15))
# Animated buble plot
p + transition_time(day)

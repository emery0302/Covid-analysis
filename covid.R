# Data source: https://www.kaggle.com/tunguz/data-on-covid19-coronavirus


#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")
library(tidyverse)
library(data.table)
library(funModeling) 
library(Hmisc)
library(ggplot2)
library(readr)

### input data
data <- read_csv("owid-covid-data-2.csv")

### view data
View(data)

### explore data
df_status(data)
#data[, lapply(.SD, function(x) sum(is.na(x))/ nrow(data))]

freq(data)
plot_num(data)
profiling_num(data)

##### select data in Taiwan
tw <- data %>% 
  dplyr::filter(data$location == "Taiwan")

fr <- data[location == "France",]

##### select data in Nethelands
nl <- data %>% 
  dplyr::filter(location  == 'Netherlands')

### select certain columns
tw <-tw %>% 
  select(iso_code, location, date, total_vaccinations, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)
nl <-nl %>% 
  select(iso_code, location, date, total_vaccinations, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)

tw <- tw[!is.na(tw$total_vaccinations),]
nl <- nl[!is.na(nl$total_vaccinations),]

#### visualize data
p_tw <- ggplot(data = tw, aes(x = date, y = people_vaccinated_per_hundred, color = location))
p_tw + geom_line(linewidth=1.2) + scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))+
  labs(title = " % of prople vaccinated in Taiwan",
       x = "Date",
       y = "% vaccinated people")

p <- ggplot(data = data, aes(x = date, y = people_vaccinated_per_hundred, color = location)) 
p + geom_line(data = tw, linewidth=1.2) + scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100))+
    geom_line(data = nl, linewidth=1.2) +
  labs(title = " % of prople vaccinated",
       x = "Date",
       y = "% vaccinated people")

p1 <- ggplot(data = data %>% 
               dplyr::filter(data$location  %in% c("Netherlands", "United Kingdom", "Taiwan", "China") & !is.na(people_vaccinated_per_hundred)), aes(x = date, y = people_vaccinated_per_hundred, color = location))
p1 + geom_line(linewidth=1.2) + scale_y_continuous(breaks=c(0,20,40,60)) + 
  labs(titles = '% of vaccinated people',
       x = 'Date',
       y = '% vaccinated people')

data_1 <- data %>% 
  dplyr::filter(!is.na(people_vaccinated_per_hundred)) %>% 
  group_by(continent,date) %>% 
  summarise(vaccinated_per_hundred = mean(people_vaccinated_per_hundred))

p2 <- ggplot(data = data_1, aes(x = date, y = vaccinated_per_hundred, color = continent))
p2 + geom_line() + scale_y_continuous(breaks=c(0,20,40,60,80,100)) + 
  labs(titles = '% of vaccinated people',
       x = 'Date',
       y = '% vaccinated people')

p2 + geom_smooth() + scale_y_continuous(breaks=c(0,20,40,60,80,100))  + 
  labs(titles = '% of vaccinated people',
       x = 'Date',
       y = '% vaccinated people')


data_2 <- data %>% 
  dplyr::filter(!is.na(people_vaccinated_per_hundred)) %>% 
  filter(location %in% c("Japan","China", "Hong Kong", "India", "South Korea",
                         "Singapore","Taiwan","Turkey","Vietnam")) 

p3 <- ggplot(data = data_2, aes(x = date, y = people_vaccinated_per_hundred, color = location))
p3 + geom_line() + scale_y_continuous(breaks=c(0,20,40,60,80,100)) + 
  labs(titles = '% of vaccinated people',
       x = 'Date',
       y = '% vaccinated people')

### Compare some countries in Asia, the trends show that Singapore is the first country
### started vaccinating people, where is the first place reached 80% of population
### getting vaccinated (around September 2021).

### Turkey and India have around 70% of vaccinated population,
### while other countries show 80% and even 90% of vaccinated population in Asia.

### However, most of countries dramatically increased their vaccinated rate between
### April 2021 and October 2021. In addition, the stable trend can be seen when the rate 
### reached to the peak, which is around beginnig of 2022.

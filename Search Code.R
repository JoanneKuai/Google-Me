# Download Google search data from https://takeout.google.com/settings/takeout
# Find My Activity.html file under Takeout/My Activity/search 
doc <- "My Activity.html"

# Load libraries for initial analysis 
library(tidyverse)
library(tidytext)
library(lubridate)
library(rvest)

# Use read_html from rvest package to read the html file
search_archive <- read_html(doc)

date_search <- search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=<br>)(.*)(?<=PM|AM)") %>%
  mdy_hms()

text_search <- search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
  str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
  str_extract(pattern = '(?<=\">)(.*)')

type_search <- search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=mdl-typography--body-1\">)(.*)(?=<a)") %>% 
  str_extract(pattern = "(\\w+)(?=\\s)")


search_data <- tibble(timestamp = date_search,
                      date = as_date(date_search),
                      year = year(date_search),
                      month = month(date_search, label = TRUE),
                      day = weekdays(date_search),
                      hour = hour(date_search),
                      type = type_search,
                      search = text_search)


search_data$day <- factor( search_data$day , 
                           levels = c("Sunday", "Monday", "Tuesday",
                                      "Wednesday","Thursday",
                                      "Friday", "Saturday"))

search_data <- na.omit(search_data)
head(search_data)


# Plotting search history data
library(ggplot2)

# Generating line chart of searches per year 
search_data %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot()+
  geom_line(aes(x=year, y = count))+
  scale_x_continuous(limit = c(2010,2018),breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  labs(title="Volumn of Google Searches Per Year")+
  theme_minimal()

# Generating bar chart of searches per month, sorted by year 
search_data %>%
  ggplot()+
  geom_bar(aes(x=month))+
  facet_grid(.~year)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 3))+
  labs(title="Volumn of Google Searches Per Month")

# Generating bar chart of searches by hour
search_data %>%
  ggplot()+
  geom_bar(aes(x=hour))+
  theme_minimal()+
  labs(title="Volumn of Google Searches By Hour")

# Generating bar chart of searches per weekday
search_data %>%
  ggplot()+
  geom_bar(aes(x=day))+
  theme_minimal()+
  labs(title="Volumn of Google Searches Per Weekday")

# Generating bar chart of searches per weekdays by hours
search_data %>%
  ggplot()+
  geom_bar(aes(x=hour))+
  facet_grid(.~day)+
  scale_x_continuous(breaks = c(0,12,24))+
  theme_minimal()+
  labs(title="Volumn of Google Searches Per Weekday/Hour")

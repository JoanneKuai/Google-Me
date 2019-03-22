# Analysing search history data 
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

# Perpare corpus for wordcloud and load library 
library(tm)
library(wordcloud)
text <- Corpus(VectorSource(search_data$search))

# Clean text: Remove numbers, English common stopwords, puncutations, etc
text <- tm_map(text, tolower)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, removeWords, c("the", "and")) 
text <- tm_map(text, removePunctuation)

# Turn it into a term document matrix
tdm <- TermDocumentMatrix(text) 

# Create wordcloud
m <- as.matrix(tdm)  
freq <- sort(rowSums(m), decreasing = TRUE)
wordcloud(words = names(freq), freq = freq, min.freq = 4, random.order = FALSE, 
          col=terrain.colors(8), scale=c(8,.2), max.words=100, rot.per=.15)


# Plotting Locaiton History Data 
# Download location hisotry jason file from https://takeout.google.com/settings/takeout
# Load libaries to prepare for analysis
library(lubridate)
library(zoo)
library(jsonlite)
system.time(x <- fromJSON("Location History.json"))

# Extracting the locations dataframe
loc = x$locations

# Converting time column from posix milliseconds into a readable time scale
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

# Converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

head(loc)        

min(loc$time)
max(loc$time)

# Calculate the number of data points per day, month and year
loc$date <- as.Date(loc$time, '%Y/%m/%d')
loc$year <- year(loc$date)
loc$month_year <- as.yearmon(loc$date)

points_p_day <- data.frame(table(loc$date), group = "day")
points_p_month <- data.frame(table(loc$month_year), group = "month")
points_p_year <- data.frame(table(loc$year), group = "year")

nrow(points_p_day)
nrow(points_p_month)
nrow(points_p_year)

# Setting up for plotting 
remove.packages("ggmap")
devtools::install_github("dkahle/ggmap")

remove.packages("ggplot2")
devtools::install_github("hadley/ggplot2")

library(ggplot2)
library(ggmap)

# Register for Google API and load key 
register_google(key = "xxx", write = TRUE)

# Setting up plotting theme
my_theme <- function(base_size = 12, base_family = "sans"){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "navy"),
      legend.position = "right",
      legend.background = element_blank(),
      panel.margin = unit(.5, "lines"),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

points <- rbind(points_p_day[, -1], points_p_month[, -1], points_p_year[, -1])

# Plot data points of Google location history 
ggplot(points, aes(x = group, y = Freq)) + 
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) + 
  geom_boxplot(aes(color = group), size = 1, outlier.colour = NA) + 
  facet_grid(group ~ ., scales = "free") + my_theme() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "Number of data points",
    title = "How many data points did Google collect about me?",
    subtitle = "Number of data points per day, month and year",
    caption = "\nGoogle collected between 0 and 1500 data points per day
    (median ~500), between 0 and 40,000 per month (median ~15,000) and 
    between 80,000 and 220,000 per year (median ~140,000)."
  )

# Mapping locations of summer 2018 in Europe
germany <- get_map(location = 'Germany', zoom = 4)

ggmap(germany) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Europe",
    caption = "\nA simple point plot shows recorded positions.")

# Mapping locaitons of summer holiday in Lisbon
lisbon <-  get_googlemap(center = 'Lisbon', zoom = 13, maptype = 'roadmap')

ggmap(lisbon) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Lisbon",
    caption = "\nA simple point plot shows recorded positions.")







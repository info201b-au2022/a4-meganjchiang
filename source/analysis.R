library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
# install.packages("usmap")
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_trends <- read.csv("~/Documents/info201/data/incarceration_trends.csv", stringsAsFactors = FALSE)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Question 1: Which states had the highest and lowest change in total 
# jail population from 1970 to 2018?
total_jail_pop <- incarceration_trends %>%
  select(year, state, county_name, total_jail_pop) %>%
  filter(!is.na(total_jail_pop))

jail_pop_1970 <- total_jail_pop %>%
  filter(year == 1970) %>%
  filter(state != "HI" & state != "VT") %>% # HI and VT do not have 2018 data
  group_by(state) %>%
  summarize(
    avg_pop = mean(total_jail_pop)
  ) %>%
  mutate(
    year = "1970"
  ) %>%
  select(year, everything())

jail_pop_2018 <- total_jail_pop %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(
    avg_pop = mean(total_jail_pop)
  ) %>%
  mutate(
    year = "2018"
  ) %>%
  select(year, everything())

diff_jail_pop <- data.frame(
  state = jail_pop_1970$state, 
  per_change_avg_pop = ((jail_pop_2018$avg_pop - jail_pop_1970$avg_pop) / 
       jail_pop_1970$avg_pop) * 100
  )

max_change <- diff_jail_pop %>%
  filter(per_change_avg_pop == max(per_change_avg_pop)) %>%
  pull(per_change_avg_pop)

state_max_change <- diff_jail_pop %>%
  filter(per_change_avg_pop == max_change) %>%
  pull(state)
  
min_change <- diff_jail_pop %>%
  filter(per_change_avg_pop == min(per_change_avg_pop)) %>%
  pull(per_change_avg_pop)

state_min_change <- diff_jail_pop %>%
  filter(per_change_avg_pop == min_change) %>%
  pull(state)

# Question 2: In 2018, which county had the most disproportionate amount of 
# Black people in jail compared to its total Black population?
black_data <- incarceration_trends %>%
  filter(year == 2018) %>%
  select(county_name, state, black_pop_15to64,
         total_pop_15to64, black_jail_pop, total_jail_pop) %>%
  na.omit() %>%
  filter(black_jail_pop < total_jail_pop) %>% # removing the counties that had a
  # higher black_jail_pop than total_jail_pop (because this doesn't make sense)
  mutate(
    location = paste0(county_name, ", ", state),
    black_prop = black_pop_15to64 / total_pop_15to64,
    black_jail_prop = black_jail_pop / total_jail_pop,
    diff_prop = abs(black_prop - black_jail_prop)
  )

county_black_disproportionate <- black_data %>%
  filter(diff_prop == max(diff_prop)) %>%
  pull(location)

# Question 3: For the county identified in Question 2, what was the difference 
# between the proportion of Black people in jail and the proportion of Black 
# people in the county's total population?
diff_prop <- black_data %>%
  filter(diff_prop == max(diff_prop)) %>%
  pull(diff_prop)
#----------------------------------------------------------------------------#


## Section 3  ---- 
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function returns a data frame of the U.S. prison population every year 
# from 1970 to 2018.
get_year_jail_pop <- function() {
  year_jail_pop <- incarceration_trends %>%
    filter(!is.na(total_jail_pop)) %>%
    select(year, total_jail_pop)
return(year_jail_pop)   
}

# This function plots a bar chart of the U.S. prison population from 
# 1970 to 2018. 
plot_jail_pop_for_us <- function() {
  plot_jail_pop_for_us <- get_year_jail_pop() %>%
    ggplot(aes(x = year, y = total_jail_pop)) +
    geom_col() +
    scale_y_continuous(labels = comma) + 
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year", 
      y = "Total Jail Population",
      caption = "Fig. 1. This bar chart shows that from 1970 to 2018, there was an overall increase in the U.S. prison population.") +
    theme(plot.caption.position = "panel",
          plot.caption = element_text(hjust = 0)
    )
  return(plot_jail_pop_for_us)   
} 

plot_jail_pop_for_us <- plot_jail_pop_for_us()
#----------------------------------------------------------------------------#


## Section 4  ---- 
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#
# This function returns a data frame of the U.S. prison population every year 
# from 1970 to 2018 for the given state(s).
get_jail_pop_by_states <- function(states) { 
  jail_pop_by_states <- incarceration_trends %>%
    filter(!is.na(total_jail_pop)) %>%
    filter(state %in% c(states)) %>%
    group_by(year, state) %>%
    summarize(
      total_jail_pop = sum(total_jail_pop)
    ) %>%
  return(jail_pop_by_states)   
}

# This function plots a line chart of the change in the U.S. prison population 
# from 1970 to 2018 for the given state(s).
plot_jail_pop_by_states <- function(states) {
  plot_jail_pop_by_states <- get_jail_pop_by_states(states) %>%
    ggplot(aes(x = year, y = total_jail_pop, group = state, color = state)) +
    geom_line() +
    labs(
      title = "Change in U.S. Prison Population Per State (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      caption = "Fig. 2. This line chart shows the change in the U.S. prison population for various states from 1970 to 2018.") +
    theme(plot.caption.position = "panel",
          plot.caption = element_text(hjust = 0)
    )
  return(plot_jail_pop_by_states)
} 
#----------------------------------------------------------------------------#

## Section 5  ---- 
# Comparing the Proportion of Black People in the Prison Population and the 
# Proportion of Black People in the Total Population in 2018 (Per Region)
#----------------------------------------------------------------------------#
# This function returns a data frame of the proportions of Black people in 
# the general population and in jail for every division of the U.S.
get_black_pop_by_region <- function() {
  black_region_data <- incarceration_trends %>%
    filter(year == 2018) %>%
    select(county_name, state, region, division, black_pop_15to64,
           total_pop_15to64, black_jail_pop, total_jail_pop) %>%
    na.omit() %>%
    filter(black_jail_pop < total_jail_pop) %>% # removing counties that had a 
    # higher black_jail_pop than total_jail_pop (because this doesn't make sense)
    mutate(
      location = paste0(county_name, ", ", state),
      black_prop = black_pop_15to64 / total_pop_15to64,
      black_jail_prop = black_jail_pop / total_jail_pop
    ) %>%
    group_by(division) %>%
    summarize(
      black_prop = mean(black_prop),
      black_jail_prop = mean(black_jail_prop)
    ) 
  colnames(black_region_data) <- c("Division", "In General Population", "In Jail") 
  black_region_data <- black_region_data %>%
    pivot_longer("In General Population":"In Jail")
  colnames(black_region_data) <- c("Division", "Proportion of Black People", "value") 
  return(black_region_data)
}
# This function plots a side-by-side bar chart that shows the disproportionate 
# number of Black people in jail compared to the proportion of Black people in 
# the overall population.
plot_black_pop_by_region <- function() {
  plot_black_pop_by_region <- get_black_pop_by_region() %>%
    ggplot(aes(x = Division, y = value, fill = `Proportion of Black People`)) +
    geom_bar(stat = "identity", position = 'dodge') +
    labs(
      title = "Proportion of Black People in Jail vs. Proportion of Black People in the Total Population",
      x = "Division",
      y = "Proportion",
      caption = "Fig. 3. This side-by-side bar chart shows how there is a disproportionate number of Black people in jail compared to the proportion of Black people in the overall population in each division of the United States, most notably in the South Atlantic.") +
    theme(plot.caption.position = "panel",
          plot.caption = element_text(hjust = 0)
    ) 
  return(plot_black_pop_by_region)
}

plot_black_pop_by_region <- plot_black_pop_by_region()
#----------------------------------------------------------------------------#


## Section 6  ---- 
# A Map of the Difference in the Proportion of Black People in Prison vs. the 
# Proportion of Black People in the General Population (Per County)
#----------------------------------------------------------------------------#
# This function returns a data frame of the proportions of Black people in the 
# general population and in jail for every county of the U.S., as well as each 
# county's coordinates.
get_black_prop_coordinates <- function() {
  # source for county csv file: https://simplemaps.com/data/us-counties
  counties <- read.csv("~/Documents/info201/data/uscounties.csv", 
                       stringsAsFactors = FALSE) %>%
    mutate(
      location = paste0(county_full, ", ", state_id)
    ) %>%
    select(location, lat, lng)
  
  black_prop_coordinates <- inner_join(counties, black_data) %>%
    mutate(
      radius = (diff_prop / max(diff_prop) * 3)
    )
  return(black_prop_coordinates)
}

get_black_prop_coordinates <- get_black_prop_coordinates() 

# This function creates a map of the United States with a circle marker for each 
# county; each marker represents the difference between the proportion of Black 
# people in prison and the proportion of Black People in that county's 
# population. The bigger the circle, the greater the difference.
map_black_prop <- function() {
  map <- get_black_prop_coordinates %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lat = ~lat,
      lng = ~lng,
      popup = ~location,
      stroke = FALSE,
      radius = ~radius,
      fillOpacity = .5
    )
}

map_black_prop <- map_black_prop()

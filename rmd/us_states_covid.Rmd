---
title: "COVID Analysis for US States"
author: "Shreejit Poudyal"
date: "12/10/2021"
output: html_notebook
---

## Introduction

Countries around the world are responding to an outbreak of respiratory illness caused by a novel coronavirus, COVID-19. The outbreak first started in Wuhan, China, but cases have been identified in a growing number of other locations internationally, including the United States. In this report we explore how the trajectory of the total deaths in a number of states for roughly a half-yearly period since 2020.

The data comes from a data set posted by nytimes in their github page. The data package provides a raw format dataset of the 2019 Novel Coronavirus COVID-19 (2019-nCoV) epidemic. The data is at ("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") and is updated daily.

For our analysis, in addition to the state-wise data package, we will use the following packages for data wrangling and visualisation.

- **tidyverse** for data wrangling and visualization
- **lubridate** package for handling dates
- **glue** package for constructing text strings
- **scales** package for formatting axis labels
- **ggrepel** package for pretty printing of country labels
- **here** to specify where files/data is in the computer relative to a particular file

We will make use of the **DT** package for interactive display of tabular output in the Appendix.

```{r load-packages}
library(coronavirus) 
# devtools::install_github("RamiKrispin/coronavirus")
library(tidyverse)
library(lubridate)
library(glue)
library(scales)
library(ggrepel)
library(DT)
library(here)
```

### Loading Data

```{r}
us_states <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```

## Data prep

The data frame called `us_states` extracted from the link provides a daily summary of the Coronavirus (COVID-19) cases for selected states. Each row in the data frame represents a state. A full list of the states in the data frame is provided in the [Appendix] in the dataset `us_states` and the data for this code is filtered in `state_data`. Note that the data provided in this package provides daily number of deaths and confirmed cases. For this report, we will focus on the deaths for particular states. 

We will start by making our selection for the states we want to explore.

``` {r select-states}
states <- c(
  "New York",
  "Florida",
  "Connecticut",
  "Massachusetts",
  "New Jersey",
  "Texas",
  "California",
  "Washington",
  "Illinois",
  "Idaho"
)
```

In the following code chunk we filter the data frame for deaths in the states we specified above and calculate total number of deaths as of date required. We will only visualise data since 10th confirmed death. 


```{r prep-state-data}
state_data <- us_states %>%
  # filter for deaths in states of interest
  filter(
    state %in% states,
  ) %>%
  # calculate number of total cases for each state and date
  group_by(date, state) %>%
  summarise(tot_deaths = sum(deaths)) %>%
  # arrange by date in ascending order
  arrange(date) %>%
  # record daily cumulative cases as cumulative_cases
  mutate(cumulative_deaths = cumsum(tot_deaths)) %>%
  # only use days since the 10th confirmed death
  filter(cumulative_deaths > 9) %>%
  # record days elapsed, end date, and end label
  mutate(
  #days_elapsed = as.numeric(max(date)) - as.numeric(min(date)),
  end_date     = if_else(date == max(date), TRUE, FALSE),
  end_label    = if_else(end_date, state, NULL)
  ) %>%
  # ungroup
  ungroup()
```
We also need to take a note of the "as of date" for the data so that we can properly label our visualisation.

```{r record-as-of-date}
  as_of_date <- state_data %>% 
  summarise(max(date)) %>% 
  pull()

as_of_date_formatted <- glue("{wday(as_of_date, label = TRUE)}, {month(as_of_date, label = TRUE)} {day(as_of_date)}, {year(as_of_date)}")
```

## Visualisation

The following visualization shows the dates from the past year till the present vs. total deaths overtime in each of the states. 
  
```{r visualise, warning=FALSE}
ggplot(state_data, mapping = aes (x = date, 
                        y = tot_deaths,
                        color = state, 
                        label= end_label
                      ))+
  scale_x_discrete(breaks=c("2020-05-24", "2020-11-24", "2021-05-24", "2021-09-25"))+
  # represent total deaths with lines
  geom_line(width=0.2, size = 0.3, alpha = 0.3) +
  # add points to line endings
  geom_point(data = state_data %>% filter(end_date)) +
  # add state labels, nudged above the lines
  geom_label_repel(nuindge_y = 1, direction = "y", hjust = 1) + 
  # better formatting for y-axis
  scale_y_continuous(labels = label_comma()) +
  # use minimal theme
  theme_minimal() +
  # customize labels
  labs(
    x = "Date",
    y = "Number of deaths",
    title = "Cumulative deaths from COVID-19, selected states",
    subtitle = glue("Data as of", as_of_date_formatted, .sep = " "),
    caption = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  )
```

The `ggsave()` code has been used with the `here` package. `ggsave()` ensures that the previous plot created gets saved with the name and device as declared bu the user. In this case, it will save it in the file/folder as instructed by the user. I will be saving all my necessary plots in the output folder.

```{r}
ggsave(here("output", "covid_cases.jpg"))
```

## Appendix   

A list of states in the `us_states` data frame is provided below.

```{r list-us_states, echo = FALSE}
  glimpse(us_states)
  names(us_states)
````

The filtered data set for the selected states is provided below.

```{r list-state_data, echo=FALSE}
  glimpse(state_data)
  names(state_data)
````


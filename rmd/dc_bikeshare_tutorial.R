---
title: "dcbikeshare"
author: "Shreejit Poudyal"
date: "11/1/2021"
output: html_notebook
---

### Loading Packages

For the analysis, I used the following packages for data wrangling and visualization.

- **tidyverse** for data wrangling and visualization
- **tidymodels** to install and load tidyverse packages related to modeling and analysis
- **dsbox** package will provide us with all the necessary data for DC Bikeshare
- **here** to specify where files/data is in the computer relative to a particular file

```{r}
library(tidyverse)
library(tidymodels)
library(dsbox)
library(here)
```

### Recode season

Recode the season variable to be a factor with meaningful level names as outlined in the code book, with spring as the baseline level.

```{r}
dcbikeshare <- dcbikeshare %>%
  mutate(
    season = case_when(
    season == 1 ~ "winter",
    season == 2 ~ "spring",
    season == 3 ~ "summer",
    season == 4 ~ "fall"),
    season = fct_relevel(season, c("spring", "summer", "fall", "winter")))
```

### Recode holiday and working day

Recode the binary variables holiday and workingday to be factors with levels no (0) and yes (1), with no as the baseline level.

```{r}
dcbikeshare <- dcbikeshare %>%
  mutate(
    holiday = ifelse(holiday == 0, "no", "yes"),      
    holiday = fct_relevel(holiday, "no", "yes"),    
    workingday = ifelse(workingday == 0, "no", "yes"),
    workingday = fct_relevel(workingday, "no", "yes")
  )
```

### Recode year

Recode the yr variable to be a factor with levels 2011 and 2012, with 2011 as the baseline level.

```{r}
dcbikeshare <- dcbikeshare %>%
  mutate(
    yr = ifelse(yr == 0, "2011", "2012"),
    yr = fct_relevel(yr, "2011", "2012")
  )
```

### Recode weathersit

Recode the `weathersit` variable as 1 - clear, 2 - mist, 3 - light precipitation, and 4 - heavy precipitation, with clear as the baseline.

```{r}
dcbikeshare <- dcbikeshare %>%
  mutate(
    weathersit = case_when(
      weathersit == 1 ~ "clear",
      weathersit == 2 ~ "mist",
      weathersit == 3 ~ "light precipitation",
      weathersit == 4 ~ "heavy precipitation"
    ),
 #   weathersit = fct_relevel(weathersit, "clear", "mist", "light precipitation", "heavy precipitation")
  )
```

#### Create raw values for temperature, humidity, and windspeed

Calculate raw temperature, feeling temperature, humidity, and windspeed as their values given in the dataset multiplied by the maximum raw values stated in the codebook for each variable. Instead of writing over the existing variables, create new ones called `temperature_raw`, `feeling_temperature_raw`, `humidity_raw`, `windspeed_raw`.

```{r}
dcbikeshare <- dcbikeshare %>%
  mutate(
    temperature_raw = temp * 41,
    feeling_temperature_raw = atemp * 50,
    humidity_raw = hum * 100,
    windspeed_raw = windspeed * 67
  )
```

#### Check cnt variable

Check that the sum of casual and registered adds up to cnt for each record. Do this by creating a new column that takes on the value TRUE if they add up and FALSE if not, and then checking if all values in that column are TRUEs.

```{r}
dcbikeshare %>%
  mutate(cas_plus_reg = casual + registered) %>%
  summarise(all_zero = all(cas_plus_reg == cnt))
```

### Recreating the visualization

```{r}
dcbikeshare %>%
  ggplot(mapping = aes(x = dteday, y = cnt, color = feeling_temperature_raw)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Bike rentals in DC, 2011 and 2012",
      subtitle = "Warmer temperatures associated with more bike rentals",
      x = "Date",
      y = "Bike rentals",
      color = "Temperature (C)"
    ) +
  theme_minimal()
```
The `ggsave()` code has been used with the `here` package. `ggsave()` ensures that the previous plot created gets saved with the name and device as declared bu the user. In this case, it will save it in the file/folder as instructed by the user. I will be saving all my necessary plots in the output folder.

```{r}
ggsave(here("output", "temperature_vs_rental.jpg"))
```

#### Bike rentals vs temperature

Fit a linear model predicting total daily bike rentals from raw daily temperature and answer the questions below.

```{r}
cnt_tmp <- linear_reg() %>%
  set_engine("lm") %>%
  fit(cnt ~ temperature_raw, data = dcbikeshare)
cnt_tmp %>%
  tidy()
glance(cnt_tmp)$r.squared
```

#### Bike rentals vs feeling temperature

Fit another linear model predicting total daily bike rentals from raw daily feeling temperature. Then, proceed to answering the questions below.

```{r}
cnt_atmp <- linear_reg() %>%
  set_engine("lm") %>%
  fit(cnt ~ feeling_temperature_raw, data = dcbikeshare)
cnt_atmp %>%
  tidy()
glance(cnt_atmp)$r.squared
```

### Full model

Fit a model predicting total daily bike rentals from season, year, whether the day is holiday or not, whether the day is a `workingday` or not, the weather category, `temperature`, `feeling temperature`, `humidity`, and `windspeed`, as well as the interaction between `feeling temperature` and `holiday`.

```{r}
cnt_full <- linear_reg() %>%
  set_engine("lm") %>%
  fit(cnt ~ season + yr + holiday + workingday + weathersit +
        temperature_raw + feeling_temperature_raw + humidity_raw +
        windspeed_raw + feeling_temperature_raw * holiday, 
      data = dcbikeshare) %>%
tidy(cnt_full)
```

Record adjusted R^2 of the model.

```{r}
glance(cnt_full)$adj.r.squared
```

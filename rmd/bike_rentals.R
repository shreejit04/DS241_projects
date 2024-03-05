---
title: "Bike Rentals Analysis"
author: "Shreejit Poudyal"
output: html_notebook
---

### Loading Packages

- **tidyverse** for data wrangling and visualization
- **lubridate** package for handling dates
- **readr** for reading csv file locally
- **janitor** for examining and cleaning untidy data
- **here** to specify where files/data is in the computer relative to a particular file

```{r}
library(tidyverse)
library(readr)
library(lubridate)
library(janitor)
library(here)
```

### Load Data

The data is available on data folder on the github repository.

```{r}
main <- read_csv("~/DS 241/bikedata/202109-capitalbikeshare-tripdata.csv")
```

##### Date prepping

using `mdy_hm`, we can edit the date to the fomat of our choice. AN advanage of this is that we can use other `lubridate` functions such as `day()`, `month()` to manipulate data or output as needed.

```{r}
hourly <- main %>%
 mutate(
   started_at = mdy_hm(started_at),
   ended_at = mdy_hm(ended_at)) %>%
  filter(mday(started_at)==1)
```


### Number of Hourly Rentals

THe following code shows us the exact number of rentals for each hour as a line graph. We can see that the highest number of rental is in the morning around 8 AM (possibly because people go to work) and 5 PM (possibly because people want to go for an evening ride to the park or any other place). The graph below can be interpreted for various possibilities about why during certain hors do we see a spike and when the rentals are low.

```{r}
num_rentals <- hourly %>%
  mutate(
    hr = hour(started_at)) %>%
  count(hr)

ggplot(num_rentals, aes(hr, n))+
  geom_line()+
  labs(
    x="Hour of the day",
    y="Number of bikes rented",
  )
```
The `ggsave()` code has been used with the `here` package. `ggsave()` ensures that the previous plot created gets saved with the name and device as declared bu the user. In this case, it will save it in the file/folder as instructed by the user. I will be saving all my necessary plots in the output folder.

```{r}
ggsave(here("output", "hourly_rentals.jpg"))
```

### Distance Calculation

The haversine function below is used to calculate the total distance traveled by a bike traveler.

```{r}
haversine <- function(start_lng, start_lat, end_lng, end_lat, round = 3)
  {
  # convert to radians
  long1 = start_lng * pi / 180
  lat1  = start_lat * pi / 180
  long2 = end_lng * pi / 180
  lat2  = end_lat * pi / 180
 
  R = 6371 # Earth's mean radius in km
 
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
 
  return(round(d,round)) # distance in km
}
```

The ggplot shows us a graph with the distance traveled by a biker in each hour. It helps us understand how far people have traveled with their bikes with respect to each hour.

```{r}
hourly <- hourly %>%
  mutate(distance = haversine(start_lng, start_lat, end_lng, end_lat))

ggplot(hourly, aes(hour(started_at), distance))+
  geom_jitter(alpha=0.3)+
  labs(
    x="Hour of the day",
    y="Distance traveled",
  )
```

### Hourly bike rentals

This chunk calculates a variable called `duration`. It is a variable that stores the total time for which a bike was rented. The function `difftime` was used to calculate the time difference between variables `started_at` and `ended_at` since both of these variables were in date-time format.

```{r}
hourly <- hourly %>%
  mutate(
    hr = hour(started_at),
    duration= as.numeric(difftime(ended_at, started_at, units="mins")),
    duration= as.numeric(sprintf(duration, fmt='%#.1f')))%>%
  group_by(hr)%>%
  arrange(desc(ymd_hms(started_at)))
```

### Average speed of Bikers

I thought it was necessary that we fund out the speed at which the bikers ride their bikes. This can help us know what the chances are that a rider might have an accident. I used the `sprintf()` is a built-in R function that returns a character vector containing the formatted combination of text and variable values. I used it to limit the number of significant figures in the value of speed to 3 and converted to numeric value using `as.numeric()`.

```{r}
speed <- hourly %>%
  mutate(
    speed = distance/(duration/60),
    speed = as.numeric(sprintf(speed, fmt='%#.3f'))
  )
```

### Visualising Average speed of bikes rented

```{r}
ggplot(speed, aes(x = hr , y = speed, group=TRUE)) +
  geom_jitter(outline=FALSE, alpha=0.2)+
  labs(
    x="Hour of the day",
    y="Speed (km/h)",
    title="Average speed of bikes rented"
  )
```


### Hourly data plotting

The code below gives us a box plot of all the hours i.e. the median of all the hours and duration for Sept 1st.

```{r}
hourly %>%
ggplot(aes(x = hr , y = duration, group=TRUE)) +
  geom_boxplot(outline=FALSE)+
  coord_cartesian(ylim=c(0, 80))+
  labs(
    x="Hour of the day",
    y="Duration of use (hours)",
    title="Average time of hourly bike usage"
  )
```

### Hourly data plotting

The code below gives us a separate box plot for duration of bike usage for each hour.

```{r}
hourly %>%
ggplot(aes(x = factor(hr) , y = duration)) +
  geom_boxplot()+
  coord_cartesian(ylim=c(0, 40))+
  labs(
    x="Hour of the day",
    y="Duration of use (minutes)",
    title="Average time of hourly bike usage",
    fill="Hour"
  )
```
```{r}
ggsave(here("output", "duration_of_rental.jpg"))
```

### Data for each hour

The code below gives us a separate box plot for each hour with the average time of bike usage for each hour.

```{r}
hourly %>%
ggplot(aes(minute(started_at) , duration, fill = desc(hr), group=TRUE)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0, 40))+
  facet_wrap(~hr, nrow=4, ncol=6, shrink=TRUE)+
   labs(
    x="Minute",
    y="Duration of use (minutes)",
    title="Average time of hourly bike usage",
    fill="Hour"
  )+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

### Function for particular hours

The function `perhour` filters for the particular hour the user wishes to see the data for. It specifies the data values for the hour entered.

```{r}
perhour = function (hour) {
  hour_num <- hourly %>%
  filter(hr == hour)
  }
```

### Total times used for rent per minute

The chunk below gives us a graph of the 14th hour of the day.

```{r}
hour_14 = perhour(14)
 
ggplot(hour_14, aes(x = minute(started_at) , y = duration)) +
  geom_smooth() +
   labs(
    x="Minute",
    y="Duration of use (minutes)",
    title="Average time of hourly bike usage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

The chunk below gives us a graph of the 3rd hour of the day.

```{r}
hour_3 = perhour(3)
   
ggplot(hour_3, aes(x = minute(started_at) , y = duration)) +
  geom_smooth() +
  labs(
    x="Minute",
    y="Duration of use (minutes)",
    title="Average time of hourly bike usage") 
```

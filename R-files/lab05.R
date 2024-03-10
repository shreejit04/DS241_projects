---
title: "Lab 05 - La Quinta is Spanish for next to Denny's, Pt. 2"
author: "Shreejit Poudyal"
date: "10/13/2021"
output: html_notebook
---

### Load packages and data

For the analysis, I used the following packages for data wrangling and visualisation.

- **tidyverse** for data wrangling and visualization
- **dsbox** package will provide us with all the necessary data for DC Bikeshare.
- **sf** to convert coordinates into geometrical points

```{r}
library(tidyverse)
library(dsbox)  
library(sf)
```

### Read Data

The exercise can be found here: https://rstudio-education.github.io/datascience-box/course-materials/lab-instructions/lab-05/lab-05-wrangle-sp-data.html

```{r load-data, message = FALSE, warning=FALSE}
states <- read_csv("https://raw.githubusercontent.com/rstudio-education/datascience-box/master/course-materials/lab-instructions/lab-04/data/states.csv")
```

### Exercise 1

Filter the `dennys` data frame for Alaska (AK) and save the result as `dn_ak`. How many Denny’s locations are there in Alaska?

There are 3 `Denny's` locations in Alaska.

```{r}
dn_ak <- dennys %>%
  filter(state == "AK")
nrow(dn_ak)
```

### Exercise 2

Filter the `laquinta` dataframe for Alaska (AK) and save the result as `lq_ak`. How many La Quinta locations are there in Alaska?

There are 3 `La Quinta` locations in Alaska.

```{r}
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(lq_ak)
```

### Exercise 3

How many pairings are there between all Denny’s and all La Quinta locations in Alaska, i.e. how many distances do we need to calculate between the locations of these establishments in Alaska?

We need 6 pairings of distances between all Denny’s and all La Quinta locations in Alaska.

### Exercise 4

How many observations are in the joined dn_lq_ak data frame? What are the names of the variables in this data frame.

There were 6 observations for this data set, `dn_lq_ak`. The variables in the data frame are `address.x`, `city.x`, `state`, `zip.x`, `longitude.x`, `latitude.x`, `address.y`, `city.y`, `zip.y`, `longitude.y` and `latitude.y`.

`.x` in the variable names means the variable comes from the x data frame (the first argument in the full_join call, i.e. `dn_ak`), and `.y` means the variable comes from the y data frame. These variables are renamed to include `.x` and `.y` because the two data frames have the same variables and it’s not possible to have two variables in a data frame with the exact same name.

```{r}
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
dn_lq_ak
names(dn_lq_ak)
```

### Exercise 5

What function from the tidyverse do we use the add a new variable to a data frame while keeping the existing variables?

One way of calculating the distance between any two points on the earth is to use the Haversine distance formula. This formula takes into account the fact that the earth is not flat, but instead spherical.

### Function Prep

```{r}
haversine <- function(longitude.x, latitude.x, longitude.y, latitude.y, round = 3) 
  {
  # convert to radians
  long1 = longitude.x * pi / 180
  lat1  = latitude.x  * pi / 180
  long2 = longitude.y * pi / 180
  lat2  = latitude.y  * pi / 180
  
  R = 6371 # Earth's mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return(round(d,round)) # distance in km
}
```

### Exercise 6

Calculate the distances between all pairs of Denny’s and La Quinta locations and save this variable as distance. Make sure to save this variable in the `dn_lq_ak` data frame so that you can use it later.

```{r}
dn_lq_ak %<>%
mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y))%>%
  arrange(distance)
```

### Exercise 7

Calculate the minimum distance between a Denny’s and La Quinta for each Denny’s location. To do so we group by Denny’s locations and calculate a new variable that stores the information for the minimum distance.

```{r}
dn_lq_ak_mindist = dn_lq_ak %>%
  select(address.x, address.y, distance)%>%
  group_by(address.x, address.y) %>%
  summarise(closest = min(distance))%>%
  filter(closest<=10)%>%
  arrange(closest)
dn_lq_ak_mindist
```

### Exercise 8

Describe the distribution of the distances Denny’s and the nearest La Quinta locations in Alaska. Also include an appropriate visualization and relevant summary statistics.

We can see the 3 pairs of locations. 2 of them are between 5-6 km away and one of them (a pair o Denny's and La Quinta) is roughly 2 km away.

```{r}
ggplot(dn_lq_ak_mindist, aes(address.x, closest))+
  geom_line(color="steelblue")+
  geom_point()+
  facet_wrap(vars(address.y))+
labs(
  x="Denny's",
  y="Distance (km)"
)
```


### Prepping the Data

```{r}
laquinta %>%
  filter(!(state %in% states$abbreviation))
dennys %>%
  filter(!(state %in% states$abbreviation))
dennys %>%
  mutate(country = "United States")
dennys <- dennys %>%
  mutate(dennys, country = case_when(
    state %in% state.abb  ~ "United States",
  state=="DC" ~ "United States"))
laquinta <- laquinta %>%
  mutate(laquinta, country = case_when(
    state %in% state.abb  ~ "United States",
    state %in% c("ON", "BC") ~ "Canada",
    state == "ANT" ~ "Colombia",
    state %in% c("AG", "QR", "CH", "NL", "VE", "PU", "SL") ~ "Mexico",
    state == "FM" ~ "Honduras"
  ))
```

### Exercise 9

Repeated the same analysis for North Carolina:

(i) Filtered Denny’s and La Quinta Data Frames for NC.
(ii) Joined these data frames to get a complete list of all possible pairings.
(iii) Calculated the distances between all possible pairings of Denny’s and La Quinta in NC.
(iv) Found the minimum distance between each Denny’s and La Quinta location.
(v) Visualize and describe the distribution of these shortest distances using appropriate summary statistics.

```{r}
dennys_nc = dennys %>%
filter(state=="NC")%>%
mutate(establishment = "Denny's")
laquinta_nc = laquinta %>%
  filter(state=="NC")%>%
  mutate(establishment = "La Quinta")
dn_lq_nc <- full_join(dennys_nc, laquinta_nc, by = "state")
 
dn_lq_nc %<>%
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y))%>%
arrange(distance)

dn_lq_nc_mindist = dn_lq_nc %>%
  select(address.x, address.y, distance)%>%
  group_by(address.x, address.y) %>%
  summarise(closest = min(distance))%>%
  filter(closest<=10)%>%
  arrange(closest)

dn_lq_nc_mindist
```

### Exercise 10

Repeated the same analysis for Texas.

```{r}
dennys_tx = dennys %>%
filter(state=="TX")%>%
mutate(establishment = "Denny's")
laquinta_tx = laquinta %>%
  filter(state=="TX")%>%
  mutate(establishment = "La Quinta")
dn_lq_tx <- full_join(dennys_tx, laquinta_tx, by = "state")

dn_lq_tx%<>%
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y))%>%
arrange(distance)

dn_lq_tx_mindist = dn_lq_tx %>%
  select(address.x, address.y, distance)%>%
  group_by(address.x, address.y) %>%
  summarise(closest = min(distance))%>%
  filter(closest<=10)%>%
  arrange(closest)
dn_lq_tx_mindist
```

### Exercise 11

Repeated the same analysis for New York.

```{r}
dennys_ny = dennys %>%
filter(state=="NY")%>%
mutate(establishment = "Denny's")
laquinta_ny = laquinta %>%
  filter(state=="NY")%>%
  mutate(establishment = "La Quinta")
dn_lq_ny <- full_join(dennys_ny, laquinta_ny, by = "state")

dn_lq_ny%<>%
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y))%>%
arrange(distance)

dn_lq_ny_mindist = dn_lq_ny %>%
   select(address.x, address.y, distance)%>%
  group_by(address.x, address.y) %>%
  summarise(closest = min(distance))%>%
  filter(closest<=10)%>%
  arrange(closest)
dn_lq_ny_mindist
```

### Exercise 12

Among the states you examined, where is Mitch Hedberg’s joke most likely to hold true? Explain your reasoning.

We can see that various Denny's and La Quinta locations in Texas are very close. In Texas there are 39 pairs of Denny's and La Quinta that are less than 0.5 km further from each other and various others that are 1-10 km away from each other. Thus the joke is most likely to hold true for Texas because the corresponding Denny's and La Quinta locations are very close to one another compared to any other states.

New York also has various locations that are 1-2 km away.

For Alaska, we have only 3 locations (with respect to Denny's) that are closer to each other (Denny's and La Quinta). 


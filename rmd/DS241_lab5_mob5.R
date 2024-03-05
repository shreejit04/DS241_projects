---
title: "Lab 5: Better answers to Denny's questiom"
author: "Group 5"
output: html_notebook
---

### Load packages and data

- **tidyverse** for data wrangling and visualization
- **dsbox** package will provide us with all the necessary data for DC Bikeshare.
- **readr** for reading csv file locally
- **ggplot2** to create complex plots from data in a data frame

```{r load-packages, message = FALSE}
library(tidyverse) 
library(dsbox) 
library(ggplot2)
library(readr)
```

```{r load-data, message = FALSE}
#states <- read.csv("U:/ds241/ds241/lab-04-viz-sp-data/data/states.csv")
states <- read_csv("https://raw.githubusercontent.com/rstudio-education/datascience-box/master/course-materials/starters/lab/lab-04-viz-sp-data/data/states.csv")
```

### Exercise 1

Filter the Denny's data frame for Alaska (AK) and save the result as dn_ak.
How many Denny's locations are there in Alaska?

```{r}
dn_ak <- dennys %>%
  filter(state == "AK")
nrow(dn_ak)
```

There are 3 Denny's locations in Alaska

### Exercise 2

Filter the La Quinta dataframe for Alaska (AK) and save the result as lq_ak.
How many La Quinta locations are there in Alaska?

```{r}
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(lq_ak)
```

There are two La Quinta locations in Alaska.

### Exercise 3

How many pairings are there between all Denny's and all La Quinta locations in Alaska, i.e. how many distances do we need to calculate between the locations of these establishments in Alaska?

There are six possible pairings.

Let's join the data on Denny's and La Quinta locations in Alaska, and take a look at what it looks like:

```{r}
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
dn_lq_ak
```

### Exercise 4

How many observations are in the joined dn_lq_ak data frame?
What are the names of the variables in this data frame.

As expected, there are 6 observations in the dn_lq_ak dataset (one for each pairing).
Varaibles in this frame are state, and the address, city, zipcode, latitude, and longitude for both the Denny's location and the La Quinta location

### Exercise 5

What function from the tidyverse do we use the add a new variable to a data frame while keeping the existing variables?

The mutate function adds a new variable to a data frame while keeping the existing variables

One way of calculating the distance between any two points on the earth is to use the Haversine distance formula.
This formula takes into account the fact that the earth is not flat, but instead spherical.

```{r}
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
```

### Exercise 6

Calculate the distances between all pairs of Denny's and La Quinta locations and save this variable as distance.
Make sure to save this variable in THE dn_lq_ak data frame so that you can use it later.

```{r}
dn_lq_ak <- dn_lq_ak %>%
  mutate(distance =haversine(longitude.x,latitude.x,longitude.y,latitude.y))
dn_lq_ak
```

### Exercise 7

Calculate the minimum distance between a Denny's and La Quinta for each Denny's location.
To do so we group by Denny's locations and calculate a new variable that stores the information for the minimum distance.

```{r}
dn_lq_ak_mindist <- dn_lq_ak %>%
  group_by(address.x) %>%
  summarise(closest = min(distance))
dn_lq_ak_mindist
```

### Exercise 8

Describe the distribution of the distances Denny’s and the nearest La Quinta locations in Alaska. Also include an appropriate visualization and relevant summary statistics

Distribution: 2.03, 5.2, 6 km
```{r}
summary(dn_lq_ak_mindist)
dn_lq_ak_mindist %>% ggplot(aes(x=closest)) + geom_histogram()
```

### Exercise 9

Repeat the same analysis for North Carolina: (i) filter Denny's and La Quinta Data Frames for NC, (ii) join these data frames to get a complete list of all possible pairings, (iii) calculate the distances between all possible pairings of Denny's and La Quinta in NC, (iv) find the minimum distance between each Denny's and La Quinta location, (v) visualize and describe the distribution of these shortest distances using appropriate summary statistics.

Build function that gives the distance to the nearest Denny's from each La Quinta

```{r}
analyze_state = function (selected_state) {
  
  dn_state <- dennys %>%
  filter(state == selected_state)
  
  lq_state <- laquinta %>%
  filter(state == selected_state)
  
  dn_lq_state_mindist <- full_join(dn_state, lq_state, by = "state") %>%
    mutate(distance=
           haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 4)) %>%
  group_by(address.y) %>%
  summarise(closest = min(distance)) 
  
}
```

For North Carolina (NC):

```{r}
NC = analyze_state("NC")
summary(NC)
NC %>% ggplot(aes(x=closest)) + geom_histogram()
```

### Exercise 10
Repeat the same analysis for Texas (TX):

```{r}
TX = analyze_state("TX")
summary(TX)
TX %>% ggplot(aes(x=closest)) + geom_histogram()
```

### Exercise 11
Repeat the same analysis for a state of your choosing, different than the ones we covered so far.

Repeat the same analysis for New York (NY):

```{r}
NY = analyze_state("NY")
summary(NY)
NY %>% ggplot(aes(x=closest)) + geom_histogram()
```

### Exercise 12

Among the states you examined, where is Mitch Hedberg’s joke most likely to hold true? Explain your reasoning.

Among the states examined, the joke is most likely to hold true in Texas since over 50 (approximately 1/4) Denny's locations have a La Quinta location less than 1 km away. However, to more accurately make conclusions, we will now include other restaurants in to our analysis.

### Starting class analysis.

Read in the fast food dataset.

```{r}
#food <- read.csv("U:/ds241/ds241/FastFoodRestaurants.csv")
food <- read.csv("FastFoodRestaurants.csv")
```


Build function that gives the distance to the nearest restaurant from each LaQuinta. The number of randomly selected restaurants will be equal to the number of Denny's in the selected state. Here we use a seed of 0 to keep the results consistent for our random sample.

```{r}
restaurant_state = function (selected_state) {
  
  set.seed(0) # This makes the randomly selected states the same every time the code runs
  
  dn_state <- dennys %>%
  filter(state == selected_state)
   

  lq_state <- laquinta %>%
  filter(state == selected_state)
  
  food_state <- food %>%
    rename(state=province)%>%
    filter(state == selected_state) %>%
    #Does this make more sense to be nrows(lq) or nrows(dennys)?
    slice_sample(n=nrow(dn_state)) 
    #slice_sample(n=nrow(lq_state)) 
  
  #browser()
  
  food_lq_state_mindist <- full_join(food_state, lq_state, by = "state") %>%
    mutate(distance=
           haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 4)) %>%
  group_by(address.y) %>%
  summarise(closest = min(distance)) 
}
```

Select a state and plot distributions of both Denny's and the randomly selected restaurants.

```{r}
NY_dn = analyze_state("NY")
summary(NY_dn)
NY_dn %>% ggplot(aes(x=closest)) + geom_histogram()+labs(title="LaQuinta and Dennys")

NY_food = restaurant_state("NY")
summary(NY_food)
NY_food %>% ggplot(aes(x=closest)) + geom_histogram()+labs(title="LaQuinta and Other Restaurants")
```
Perform one tailed t test to see if Dennys are indeed closer than in selected state. The null hypothesis is that there is no difference between average distances between la Quinta and Dennys and average distances between La Quinta and other restaurants, that is $H_{o} : \sigma_{dennys} = \sigma_{other}$. The alternate hypothesis will be that the average distance between La Quintas and Dennys is less than the average distance between la quintas and other restaurants, that is $H_{a} : \sigma_{dennys} < \sigma_{other}$. Let's try this for the New York data, using $\alpha = 0.05$.

```{r}
t.test(x = NY_dn$closest, y = NY_food$closest, alternative = 'less')
```
Since the p-value is greater than the $\alpha$ value, we cannot reject the null hypothesis. This means that dennys are not any closer to la quintas than other restaurants for this distribution.

Let's look at the box and whiskers now.

```{r}
both_data_TX = rbind(NY_food, NY_dn) %>%
  mutate( group= case_when(
    row_number() <= 19 ~ 'dennys',
    TRUE ~ 'other'
  )) %>%
  ggplot(aes(x=group, y=closest, color=group)) + geom_boxplot()
both_data_TX
```

As you can see from this plot, the medians are very similar between the dennys and sample group of restaurants. Which supports the result of the t test.


Each time the code is run, the randomly selected locations change, resulting in very different distributions between the LaQuinta and other restaurants. This makes analysis hard, more work may be needed to find a way to improve comparison. NOTE: If you are worried about the randomly selected locations changing, you can use a pseudo-random generator to make it randomly select but in the same way every time the code runs. 

Let's look at Texas now:

```{r}
TX_dn = analyze_state("TX")
summary(TX_dn)
TX_dn %>% ggplot(aes(x=closest)) + geom_histogram()+labs(title="LaQuinta and Dennys")

TX_food = restaurant_state("TX")
summary(TX_food)
TX_food %>% ggplot(aes(x=closest)) + geom_histogram()+labs(title="LaQuinta and Other Restaurants")
```

Running the code several times does not seem to have major impacts on the distribution between the LaQuinta and other restaurants. Now let's perform a t-test for this distribution, using the same hypothesis and $\alpha$ as for the previous example.

```{r}
t.test(x = TX_dn$closest, y = TX_food$closest, alternative = 'less')
```
Since the p-value is greater than $\alpha$ we cannot reject the null hypothesis. So on average, dennys are not closer than other resturants for this distribution.

```{r}
both_data_TX = rbind(TX_food, TX_dn) %>%
  mutate( group= case_when(
    row_number() <= 237 ~ 'dennys',
    TRUE ~ 'other'
  )) %>%
  ggplot(aes(x=group, y=closest, color=group)) + geom_boxplot()
both_data_TX
```
As you can see from the plot, the medians are almost the same between the two groups, supporting the result of the t test. Also, you can see that some laquinta locations had no dennys and some la quintas had no other resturants near them. This is shown by the outliers.

If desired, view the data sets for the selected states

```{r}
  dn_state <- dennys %>%
  filter(state == "NY")
   

  lq_state <- laquinta %>%
  filter(state == "NY")
  
  food_state <- food %>%
    rename(state=province)%>%
    filter(state == "NY") %>%
    slice_sample(n=nrow(dn_state))
```

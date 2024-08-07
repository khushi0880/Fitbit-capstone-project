# Fitbit fitness tracker project
### Project Overview: How Can a Wellness Technology Company Play It Smart?
#### Scenario
You are a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused
products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the
global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart
device fitness data could help unlock new growth opportunities for the company. You have been asked to focus on one of
Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The
insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat
executive team along with your high-level recommendations for Bellabeat’s marketing strategy.
### Data Source
The onlt dataset I have used for this project is FitBit Fitness Tracker Data [Download here](https://www.kaggle.com/datasets/arashnic/fitbit/data)
### Tool
- R programming language
### Explanatory Analysis

### Setting up my environment
Note:  setting up my R environment by loading packages required
```r
install.packages(tidyverse)
install.packages(skimr)
install.packages(janitor)
install.packages(readr)
install.packages(ggplot2)
install.packages(plotrix)
install.packages(dplyr)
install.packages(scales)
```
#### Importing Data
Note: Importing these 3 CSV files- 
1. daily_activity [Download Here](https://drive.google.com/file/d/1K-sYq3hrD_WG3ru0Ghav7D7kc1EjHRlO/view?usp=sharing)
3. sleepDay_merged [Download Here](https://drive.google.com/file/d/1TVqEDO3gZc5sOuocfUC0CF8mgHmmXz6C/view?usp=drive_link)
4. hourlySteps_merged [Download Here](https://drive.google.com/file/d/1jOTKQjk9BLNqpcZF4kH64EpS_zqkoWVo/view?usp=sharing)
```r
daily_activity <- read_csv("Fitabase Data 4.12.16-5.12.16/inputfitbitFitabase Data 4.12.16-5.12.16dailyActivity_merged.csv.csv")
sleepDay_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourlySteps_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
```

## Data Exploration
Note: Cheking for column names and data types. Taking glimpse of dataset
```r
head(daily_activity)
head(sleepDay_merged)
head(hourlySteps_merged)
colnames(daily_activity)
colnames(sleepDay_merged)
colnames(hourlySteps_merged)
```
## Data Cleaning steps
Note: Making data cleaning by performing these steps-

1. Checking for duplicates and removing them
2. Date-Time Consistency
```r
n_distinct(daily_activity$Id)
n_distinct(sleepDay_merged$Id)
n_distinct(hourlySteps_merged$Id)
nrow(daily_activity)
nrow(sleepDay_merged)
nrow(hourlySteps_merged)
sum(duplicated(daily_activity))
sum(duplicated(sleepDay_merged))
sum(duplicated(hourlySteps_merged))

duplicates <- sleepDay_merged[duplicated(sleepDay_merged), ]
duplicate_counts <- table(sleepDay_merged[duplicated(sleepDay_merged), ])
View(duplicate_count)

duplicate_counts2 <- table(daily_activity[duplicated(daily_activity), ])
View(duplicate_counts2)

duplicate_counts3 <- table(hourlySteps_merged[duplicated(hourlySteps_merged), ])
View(duplicate_counts3)

duplicates <- sleepDay_merged[duplicated(sleepDay_merged), ]
View(duplicates)

sleepDay_new <- distinct(sleepDay_merged)
nrow(sleepDay_new)
View(sleepDay_new)

daily_activity_new <- daily_activity %>%
  rename(date = ActivityDate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
View(daily_activity_new)

sleepDay_new <- sleepDay_new %>%
  rename(date = SleepDay) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p"))
View(sleepDay_new)

activity_and_sleep <- merge(x = daily_activity_new, y = sleepDay_new, by.x = c("Id","date"), by.y = c("Id","date"))
View(activity_and_sleep)

hourly_steps_new <- hourlySteps_merged %>% 
  rename(date_time = ActivityHour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p"))
```
## Analyzing data
Note: Analyzing data by performing these steps-
1. I will calculate the step average to assign a level of activity type
```r
per_day_avg <- activity_and_sleep %>% 
  group_by(Id) %>% 
  summarise(mean_steps = mean(TotalSteps), mean_calories = mean(Calories), mean_sleep=mean(TotalMinutesAsleep), mean_bedtime = mean(TotalTimeInBed))

user_type <- per_day_avg %>% 
  mutate(type_of_user = case_when(
    mean_steps < 5000 ~ "sedentary",
    mean_steps >= 5000 & mean_steps < 7500 ~ "lightly active", 
    mean_steps >= 7500 & mean_steps < 10000 ~ "fairly active", 
    mean_steps >= 10000 ~ "very active"))
```
- Creating a chart that shows the user distribution
```r
slices <- c(5, 5, 9, 5)
lbls <- c("sedentary", "lightly active", "fairly active", "very active")
piepercent<- round(100 * slices / sum(slices), 1)
lbls <- paste(lbls, piepercent, "%",sep=" ")
pie3D(slices,labels=lbls,labelcex = 0.75,main="User Distribution Pie Chart")
```
- Including Plot [Download Here](https://drive.google.com/file/d/1ycpf4BymQ-hEh70KRE2fF224nKk502om/view?usp=drive_link)
2. Finding out relation between calories burnt and user type
```r
usertype_calories <- user_type %>% 
  select(type_of_user, mean_calories) %>% 
  arrange(type_of_user)
```
- Graphical representation
```r
ggplot(usertype_calories, aes(type_of_user, mean_calories, fill=type_of_user)) +
  geom_boxplot()+
  labs(title = "Calories burned per User Type")
```
- Including Plot [Download Here](https://drive.google.com/file/d/1eax-gawsij2ntCHRgbWRRZLTje-wZwNT/view?usp=drive_link)
3. Finding correlation between the number of steps and calories burned per day
```r
steps_vs_calories <- activity_and_sleep %>% 
  select(TotalSteps, Calories) %>% 
  rename(Steps=TotalSteps)
```
- Visualization of correlation
```r
ggplot(steps_vs_calories)+
  aes(x=Steps, y=Calories)+
  geom_point()+
  geom_smooth()+
  labs(title = "Steps vs Calories")
```
- Including Plot [Download Here](https://drive.google.com/file/d/1OljT2AKYENLBPBL7Yl_4CtpZECZGfD-S/view?usp=drive_link)
4. Figuring out relation between user type and sleep quality
```r
sleep_habbit <- user_type %>% 
  select(mean_sleep, type_of_user) %>% 
  mutate(sleep = case_when(
    mean_sleep < 360 ~ "Bad",
    mean_sleep >= 360 & mean_sleep < 420 ~ "Enough",
    mean_sleep >= 420 & mean_sleep < 480 ~ "Good",
    mean_sleep >= 480 ~ "Oversleep"
    ))
```
- Visualization
```r
ggplot(sleep_habbit)+
  aes(sleep, fill = type_of_user)+
  geom_bar()
```
- Including Plot [Download Here](https://drive.google.com/file/d/1J9eA7P60VJhuLrM7Irfr-KI3UqGa83Mp/view?usp=drive_link)
3. Devise usage per month
```{r}
devise_usage <- activity_and_sleep %>% 
  group_by(Id) %>%
  summarize(used_days=sum(n())) %>% 
  mutate(usage = case_when(
    used_days <= 10 ~ "little use",
    used_days >= 11 & used_days <= 20 ~ "medium use", 
    used_days >= 21 & used_days <= 31 ~ "high use"))

devise_use <- devise_usage %>% 
  group_by(usage) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percentage = `n` / sum(`n`), labels = scales::percent(percentage)) %>% 
  arrange(percentage)
```

- Visualization

```r
labels <- c("Medium use", "Little use", "High use")
values <- c(3, 9, 12)
pie(values, labels = paste(labels, "-", round(values/sum(values)*100), "%"), col = rainbow(length(values)), main = "Devise Usage")
```
- Including Plot [Download Here](https://drive.google.com/file/d/1mNpmS_jlRBSwyr3oElESKC_dR0fZVIR0/view?usp=drive_link)
## Insights
1. There is a key relation between steps and calories burned per day. People that are on a journy to lose weight can be motivated to make more steps a day through push notifications.
2. increase in activity can improve sleep.
3. Improve Ivy battery life. The average use per month of smart devices is 17 days, while our device 'Ivy' only has 8 days battery life.

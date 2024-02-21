install.packages(dplyr)
install.packages(tidyverse)
install.packages(lubridate)
install.packages(ggplot2)
install.packages(lubridate)
install.packages(tidyr)
install.packages(janitor)
install.packages(here)
install.packages(skimr)

daily_activity <- read_csv("Fitabase Data 4.12.16-5.12.16/inputfitbitFitabase Data 4.12.16-5.12.16dailyActivity_merged.csv.csv")
sleepDay_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourlySteps_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

head(daily_activity)
head(sleepDay_merged)
head(hourlySteps_merged)
colnames(daily_activity)
colnames(sleepDay_merged)
colnames(hourlySteps_merged)

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

per_day_avg <- activity_and_sleep %>% 
  group_by(Id) %>% 
  summarise(mean_steps = mean(TotalSteps), mean_calories = mean(Calories), mean_sleep=mean(TotalMinutesAsleep), mean_bedtime = mean(TotalTimeInBed))

user_type <- per_day_avg %>% 
  mutate(type_of_user = case_when(
    mean_steps < 5000 ~ "sedentary",
    mean_steps >= 5000 & mean_steps < 7500 ~ "lightly active", 
    mean_steps >= 7500 & mean_steps < 10000 ~ "fairly active", 
    mean_steps >= 10000 ~ "very active"))

slices <- c(5, 5, 9, 5)
lbls <- c("sedentary", "lightly active", "fairly active", "very active")
piepercent<- round(100 * slices / sum(slices), 1)
lbls <- paste(lbls, piepercent, "%",sep=" ")
pie3D(slices,labels=lbls,labelcex = 0.75,main="User Distribution Pie Chart")

usertype_calories <- user_type %>% 
  select(type_of_user, mean_calories) %>% 
  arrange(type_of_user)

ggplot(usertype_calories, aes(type_of_user, mean_calories, fill=type_of_user)) +
  geom_boxplot()+
  labs(title = "Calories burned per User Type")

steps_vs_calories <- activity_and_sleep %>% 
  select(TotalSteps, Calories) %>% 
  rename(Steps=TotalSteps) 

ggplot(steps_vs_calories)+
  aes(x=Steps, y=Calories)+
  geom_point()+
  geom_smooth()+
  labs(title = "Steps vs Calories")

sleep_habbit <- user_type %>% 
  select(mean_sleep, type_of_user) %>% 
  mutate(sleep = case_when(
    mean_sleep < 360 ~ "Bad",
    mean_sleep >= 360 & mean_sleep < 420 ~ "Enough",
    mean_sleep >= 420 & mean_sleep < 480 ~ "Good",
    mean_sleep >= 480 ~ "Oversleep"
    ))

ggplot(sleep_habbit)+
  aes(sleep, fill = type_of_user)+
  geom_bar()

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
View(devise_use)

labels <- c("Medium use", "Little use", "High use")
values <- c(3, 9, 12)
pie(values, labels = paste(labels, "-", round(values/sum(values)*100), "%"), col = rainbow(length(values)), main = "Devise Usage")

  

  


  


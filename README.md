# Bellabeat-Analysis-of-Fitness-Tracker
Analysis of Fitness Tracker
#I. Ask

#1.About the Company: Bellabeat is a high-tech company that manufactures health-focused smart products, which are designed to empower women in managing their sleep, stress, and reproductive health. Bellabeat aims to bring more awareness on health and habits.

#Since it was founded in 2013, Bellabeat has grown rapidly. By 2016, Bellabeat had opened offices around the world and launched multiple products. Bellabeat products became available through a growing number of online retailers in addition to their own e-commerce channel on their website.

#The company has invested in traditional advertising media, such as radio, out-of-home billboards, print, and television. However, the Bellabeat also notes that value of using digital marketing extensively. Bellabeat invests year-round in Google Search, maintaining active Facebook and Instagram pages, and consistently engages consumers on Twitter. Additionally, Bellabeat runs video ads on Youtube and display ads on the Google Display Network to support campaigns around key marketing dates.

#2.Requested Data Analysis An analysis of Bellabeat’s available consumer data would reveal more opportunities for growth. The founders have tasked the the marketing analytics team to focus on a Bellabeat product and analyze smart device usage data in order to gain insight into how people are already using their smart devices. High-level recommendations for how discerned trends can inform Bellabeat marketing strategy.

#3.Specific Questions to Be Asked: Insomnia is common. For many of us, sleep is elusive and is the least of our priorities. Hence, the following analysis will explore what leads to better sleep quality. This is the direction I chose because of all the data regarding activities on a daily and hourly basis.

#The null hypothesis would there is no relationship between activity and sleep quality. The following analysis sets out to demonstrate there is. Ultimately, the analysis should highlight trends that would influence how Bellabeat could have a marketing strategy by focusing on how we could use our devices to solve the common problem of poor sleep quality.

#4.Stakeholders: The main stakeholders here are Urška Sršen, Bellabeat’s co-founder and Chief Creative Officer; Sando Mur, Mathematician and Bellabeat’s cofounder; And the rest of the Bellabeat marketing analytics team.


#I. Ask

#1.About the Company: Bellabeat is a high-tech company that manufactures health-focused smart products, which are designed to empower women in managing their sleep, stress, and reproductive health. Bellabeat aims to bring more awareness on health and habits.

#Since it was founded in 2013, Bellabeat has grown rapidly. By 2016, Bellabeat had opened offices around the world and launched multiple products. Bellabeat products became available through a growing number of online retailers in addition to their own e-commerce channel on their website.

#The company has invested in traditional advertising media, such as radio, out-of-home billboards, print, and television. However, the Bellabeat also notes that value of using digital marketing extensively. Bellabeat invests year-round in Google Search, maintaining active Facebook and Instagram pages, and consistently engages consumers on Twitter. Additionally, Bellabeat runs video ads on Youtube and display ads on the Google Display Network to support campaigns around key marketing dates.

#2.Requested Data Analysis An analysis of Bellabeat’s available consumer data would reveal more opportunities for growth. The founders have tasked the the marketing analytics team to focus on a Bellabeat product and analyze smart device usage data in order to gain insight into how people are already using their smart devices. High-level recommendations for how discerned trends can inform Bellabeat marketing strategy.

#3.Specific Questions to Be Asked: Insomnia is common. For many of us, sleep is elusive and is the least of our priorities. Hence, the following analysis will explore what leads to better sleep quality. This is the direction I chose because of all the data regarding activities on a daily and hourly basis.

#The null hypothesis would there is no relationship between activity and sleep quality. The following analysis sets out to demonstrate there is. Ultimately, the analysis should highlight trends that would influence how Bellabeat could have a marketing strategy by focusing on how we could use our devices to solve the common problem of poor sleep quality.

#4.Stakeholders: The main stakeholders here are Urška Sršen, Bellabeat’s co-founder and Chief Creative Officer; Sando Mur, Mathematician and Bellabeat’s cofounder; And the rest of the Bellabeat marketing analytics team.

install.packages("tidyverse")
install.packages("dplyr")
install.packages("skimr")
install.packages("lubridate")
install.packages("strucchange")
install.packages("stats")
install.packages("datarium")
install.packages("broom")
install.packages("carData")
install.packages("car")


library(tidyverse) # collection of R packages designed for data science.
library(dplyr) #  provides a consistent set of verbs that help you solve the most common data manipulation challenges
library(skimr) # provides summary statistics about variables in data frames, tibbles, data tables and vectors
library(lubridate) # makes it easier to work with dates and times.
library(strucchange) # Testing, monitoring and dating structural changes in (linear) regression models. strucchange features tests/methods from the generalized fluctuation test framework as well as from the F test (Chow test) framework
library(stats) # This package contains functions for statistical calculations and random number generation.
library(datarium) # Contains data organized by topic: categorical data, regression model, means comparisons, independent and repeated measures ANOVA, mixed ANOVA and ANCOVA
library(broom) # takes the messy output of built-in functions in R, such as lm , nls , or t.test , and turns them into tidy tibble
library(carData) # Companion to Applied Regression Data Sets
library(car) # contains mostly functions for applied regression, linear models, and generalized linear models, with an emphasis on regression diagnostics, particularly graphical diagnostic methods.


# Upload raw data
activity <- read.csv("dailyActivity.csv")
sleep <- read.csv("sleepDay.csv")


#Examine column names for the datasets
colnames(activity)
colnames(sleep)


#Check the data
head(activity)
head(sleep)


# Mutate ActivtyHour data type to date / time and create new time, date, month, and weekday columns in df
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")

# Mutate SleepDay data type to date / time and create new date and month columns in df
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")


#Examine the number of unique observations for each dataset
n_distinct(activity$Id)
n_distinct(sleep$Id)

#Examine summary statistics for each dataset
activity %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

sleep %>%
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

#There are notable results indicating what the fitbit users are spending their time. On average, most of the users are getting about 7 hours of sleep. That is good.

#The average for the total steps per day is 7639. The number of steps is good, but it may not be enough to reap benefits. According to healthline, 10,000 steps is reasonable to counter a sedentary lifestyle. Most of the participants are lightly active. They are probably taking steps as they do normal things like shopping, going work, walking around the house etc.


#Merge the datasets
activitySleep <- merge(activity, sleep, by = c("Id", "date"))


#Examine the merged datasets
head(activitySleep)


#Eliminate unnecessary columns to declutter
activitySleep <- select(activitySleep,-"LoggedActivitiesDistance", -"TrackerDistance", -"TotalSleepRecords", -"SedentaryActiveDistance")

#Perform Multiple Regression Analysis
analysis <- lm(TotalMinutesAsleep ~ TotalSteps + TotalDistance + Calories + VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes + VeryActiveDistance + ModeratelyActiveDistance + LightActiveDistance, data = activitySleep)
summary(analysis)

#Interpretation of Statistics The larger the t-value, the more likely that the variable is not close to 0 and that the standard error is small. The smaller the p-value, the more statistical significance there is between the dependent variable of "TotalMinutesAsleep" and five of independent variables presented in this table. Two of the independent variables have a somewhat strong relationship with Total Minutes Asleep. With regards to both p-value and t-value, the independent variable "SedentaryMinutes" has the strongest correlation.

#We can see from our model, the F-statistic is very large and our p-value is so small it is basically zero. As such, the null hypothesis can be rejected. I conclude that there is evidence that a relationship the independent variables and Total Minutes Asleep.

#Examine multiple regression analysis visually
avPlots(analysis, col="Red", col.lines="green")

#Upload raw data
intensity <- read.csv("hourlyIntensities.csv")

#Examine the dataset
head(intensity)

# Mutate ActivtyHour data type to date / time and create new time, date, and month columns in df
intensity$ActivityHour=as.POSIXct(intensity$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensity$time <- format(intensity$ActivityHour, format = "%H:%M:%S")
intensity$date <- format(intensity$ActivityHour, format = "%m/%d/%y")

#Create new variable for sleep latency for sleep dataset
sleep <- mutate(sleep, SleepInefficiency = TotalTimeInBed - TotalMinutesAsleep)

# Merge daily activity and hourly intensities by id and date, and save as new df
sleepIntensity <- merge(sleep, intensity, by=c('Id', 'date'))
glimpse(sleepIntensity)

# Create new df with average intensity, grouped by the time, and na values dropped
hourlyIntensity <- sleepIntensity %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(AverageTotalIntensity = mean(TotalIntensity))

# Plot relationship between average intensity and time of day
bar_intensity <- ggplot(data=hourlyIntensity, aes(x=time, y=AverageTotalIntensity)) + 
  geom_bar(stat = "identity", fill='#00008B') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time", y="Average Intensity", x="Time of Day")

bar_intensity

#Obviously the participants are exercising mostly between 5 - 7 pm. Most likely the participants are exercising after they get back from work. But the more focused question to be answered is whether or not the hour of exercise has an impact on sleep efficiency.

# Create new df with average sleepLatency, grouped by activity for each hour, and na values dropped
sleepInefficiencyIntensity <- sleepIntensity %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(MedianSleepInefficiency = median(SleepInefficiency))

#Merge the datasets for that has the Total Average Intensity by hour with the dataset for average Sleep Latency
sleepInefficiency_intensityHour <- merge(sleepInefficiencyIntensity, hourlyIntensity, by=c("time"))

#Examine the merged dataset
head(sleepInefficiency_intensityHour)

#Conduct multiple regression on what impacts sleep latency
analysis2 <- lm(MedianSleepInefficiency ~ AverageTotalIntensity, data = sleepInefficiency_intensityHour)
summary(analysis2)

#Interpretation

#The F-statistic value is slightly more than the minimum of 3.95 so that the null hypothesis of sleep efficiency and hour of intensity can be rejected. The small p-value motivates me continue exploring. There seems to be a slight relationship between hour of exercise and sleep efficiency. The alternative hypothesis that there is relationship can be considered.

#Examine visual of multiple regression
avPlots(analysis2, col="Red", col.lines="green")

#This graph demonstrates a positive correlation.

#Examine visual of intensity by hour as correlated with the number of sleep latency minutes
ggplot(data=sleepInefficiency_intensityHour, aes(x=time, y=AverageTotalIntensity, fill=MedianSleepInefficiency)) + geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Sleep Inefficency vs. Average Total Intensity of the Hour")

#The graph still demonstrates that users are doing the most exercise between 5-7pm. The gradient of the blue color for bars demonstrates the level of sleep efficiency as it relates to the intensity of activity during a certain hour. The darker shades demonstrate that intense exercise done in the morning may lead to better sleep efficiency as opposed to intense exercise later in the day.

#Discussion

#It could be hypothesized that the majority of the fitbit users work full time during the day. They don't get many chances to break up their morning to exercise. Most likely they are sitting in front of the computer too focused on deadlines to be concerned about how what they do during the day impacts their sleep quality. Probably to a lot of these, sleep is a luxury. Bellabeat can take steps to increase awareness of how activity during the day impacts that the most basic need for all humans: sleep. Without sleep, your cogntive abilities to get work done suffers.

#According to clevelandclinic.org, inefficent sleep can cause myraid of problems mentally and physically Over time, poor sleep quality can lead to "health problems like diabetes, hypertension and weight gain." The mental challenges of lack of sleep are obvious: mood disorders and poor memory.

#Ideas for the Bellabeat app

#Bellabeat users should be educated on how activity impacts sleep quality. There should be more awareness about how the time of exercise could make a difference. Generally, the amount of exercise done during the day can impact how much tossing and turning one does. However, exercising in the morning could maintain a better circadium rythm for sleep.Conventional wisdom tells us that exercising at night could cause you to be alert because your core temperature, heartrate and cortisol levels could increase. Nighttime habits such as a turning off electronics and relaxing activities to segue to a natural transition to sleep. According to previous studies, morning exercise could promote deeper sleep than evening exercise..

#If users want to improve their sleep, Bellabeat should consider using app notifications encourage users to step away from their electronic devices and taking a warm shower for relaxation. Also Bellabeat users would sleep earlier in the night so they wake up for some exercise in the morning.

#Limitations of the Study:

#The sample size is relatively small. There were only 30 participants. The clear rule is that the minimum for adequate sample is 30. Ideally, more would have participated. Also there were other pertinent variables missing with regards to determing impacts on sleep quality:

#Age of participants
#Hormone levels
#Exposure to blue light after hours since this can impact the circadian rythmn
#Specific details about diet
#Nighttime habits such as turning off electronics and doing any work
#I welcome comment and feedback. Thank you for your attention.

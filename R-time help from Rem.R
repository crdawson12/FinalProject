library(lubridate)

d <- read.csv("allroadkilldata.csv")
names(d)

# d$tp <- as.POSIXct(d$t,format = "%m/%d/%Y %H%M")
# sum(is.na(d$tp))
# d$tp
# d$t[895]
# d$t[981]
# d$t[949:950]
# str(d)
# d$CRASHTIME <- hm(d$CRASHTIME)
# print(d$CRASHTIME)

# Convert military time numbers to standard time 
d$CRASHTIME <- sprintf("%02d:%02d %s",
                          d$CRASHTIME %/% 100, # Extract the hour
                          d$CRASHTIME %% 100,  # Extract the minutes
                          ifelse(d$CRASHTIME < 1200, "AM", "PM")  # Determine AM or PM
)



# Print the result
print(d$CRASHTIME)
d$t <- paste(d$CRASHDATE,d$CRASHTIME)
d$t

d$tp <- as.POSIXct(d$t,format = "%m/%d/%Y %H:%M")
d$tp
sum(is.na(d$tp))

hour(d$tp)
minute(d$tp)

hist(hour(d$tp))
stem(hour(d$tp))
table(hour(d$tp))

# histogram of crashes by hour in july
hist(hour(d$tp)[month(d$tp)==7])

# histogram of crashes by hour in feb
hist(hour(d$tp)[month(d$tp)==2])

# histogram of crashes by hour in dec
hist(hour(d$tp)[month(d$tp)==12]) +
  ylab("Count") +
  xlab("Hour of the day")

# histogram of crashes by hour in june
hist(hour(d$tp)[month(d$tp)==6])

library(tidyverse)

# create hour column
d$hour <- hour(d$tp)

# create month column
d$month <- month(d$tp)
d$month_name <- month(d$tp, label = TRUE)

# histogram of hour of crash (all months and years)
ggplot(d, aes(x = hour)) +
  geom_histogram()

# histogram of hour of crash (facet by month)
ggplot(d, aes(x = hour)) +
  geom_histogram(bins = 24) +
  facet_wrap(~ month) +
  theme_bw() +
  ylab("Count") +
  xlab("Hour of the day")

# histogram of hour of crash (facet by month)
ggplot(d, aes(x = hour)) +
  geom_histogram(bins = 24) +
  facet_wrap(~ month_name) +
  theme_bw() +
  ylab("Count") +
  xlab("Hour of the day")


# create year column
d$year <- year(d$tp)

# 
ggplot(d, aes(x = hour)) +
  geom_histogram(bins = 24) +
  facet_wrap(~ year) +
  theme_bw() +
  ylab("Count") +
  xlab("Hour of the day")

# create day of the week column
d$wday <- wday(d$tp, label = TRUE)

# get new column that is weekday vs. weekend
d$wday_2 <- ifelse(d$wday %in% c("Sat","Sun"), # condition statement
                   "Weekend", # value if condition statement is true
                   "Weekday") # value if condition statement is false

# show what is happening above
data.frame(d$wday,d$wday %in% c("Sat","Sun"))

# crashes by hour faceted by weekend vs weekday
ggplot(d, aes(x = hour)) +
  geom_histogram(bins = 24) +
  facet_wrap(~ wday_2) +
  theme_bw() +
  ylab("Count") +
  xlab("Hour of the day")

  
table(d$CRASHTYPE,d$year)

#make an object just on moose collision data
moose <- d %>%
  filter(CRASHTYPE == "Moose(Animal)")

#make an object just on deer collision data
deer <- d %>%
  filter(CRASHTYPE == "Deer(Animal)")

#make an object just on bear collision data
bear2 <- d %>%
  filter(CRASHTYPE == "Black Bear(Animal)")

#make an object just on turkey collision data
turkey <- d %>%
  filter(CRASHTYPE == "Turkey(Animal)")

#create new column with only 5 species-specific items- clean names
d <- d %>%
  mutate(Species = case_when(
    CRASHTYPE %in% c("Animal", "Animal(Other)") ~ "Animal",
    CRASHTYPE %in% c("Black Bear(Animal)") ~ "Black Bear", 
    CRASHTYPE %in% c("Deer(Animal)") ~ "Deer", 
    CRASHTYPE %in% c("Moose(Animal)") ~ "Moose", 
    CRASHTYPE %in% c("Turkey(Animal)") ~ "Turkey", 
  ))

#create new object with just four species (clean version)
speciesdata <- d %>%
  filter(year==2017) %>%
  filter(Species != "Animal")

#plot species specific crash data for each month (without general animals)
ggplot(data = speciesdata, aes(x = month_name, y = Species, fill= Species)) + 
  geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Months")

#plot species specific crash data for each month 
ggplot(data = d, aes(x = month_name, y = Species, fill= Species)) + 
  geom_bar(stat = "identity") +
  ylab("Count") +
  xlab("Months")

library(dplyr)

#plot the relation between deer crashes and the hour of day
ggplot(data = deer, aes(x = hour)) +
  geom_histogram(bins = 24) +
  theme_bw() + 
  ylab("Count") +
  xlab("Hour of the Day")

#plot the relation between moose crashes and hour of day
ggplot(data = moose, aes(x = hour)) +
  geom_histogram(bins = 24) +
  theme_bw() + 
  ylab("Count") +
  xlab("Hour of the Day")

#plot the relation between bear crashes and hour of day
ggplot(data = bear2, aes(x = hour)) +
  geom_histogram(bins = 24) +
  theme_bw() + 
  ylab("Count") +
  xlab("Hour of the Day")

#plot the relation between turkey crashes and hour of day
ggplot(data = turkey, aes(x = hour)) +
  geom_histogram(bins = 24) +
  theme_bw() + 
  ylab("Count") +
  xlab("Hour of the Day") 

#plot the relationship between moose collisons and light condition 
ggplot(data = moose, aes(x = hour, y = LIGHTCONDI)) +
  geom_bar(stat = "identity")

#plot the relationship between moose collisions and weather condition   
ggplot(data = moose, aes(x = hour, y = WEATHERCON)) +
  geom_bar(stat = "identity")


#plot the relationship between collisions and light condition 
ggplot(data = d, aes(x = hour, y = LIGHTCONDI)) +
  geom_bar(stat = "identity") +
  ylab("Light Condition") +
  xlab("Collision")


#plot the relationship between collisions and weather condition 
ggplot(data = d, aes(x = hour, y = WEATHERCON)) +
  geom_bar(stat = "identity") +
  ylab("Weather Condition") +
  xlab("Collision")


#plot the relationship between collisions and road alignment 
ggplot(data = d, aes(x = hour, y = ROADALIGNM)) +
  geom_bar(stat = "identity") +
  ylab("Road Alignment") +
  xlab("Collision")


#plot the relationship between species specific collisions and county with axis labels
ggplot(data = data2017b, aes(x = hour, y = COUNTY)) +
  geom_bar(stat = "identity") +
  ylab("County") +
  xlab("Collisions")


#run an anova for hour vs collision type
aov_hour <- aov(hour~CRASHTYPE, data = d)
summary(aov_hour)

#run an anova for month vs collision type
aov_month <- aov(month~CRASHTYPE, data = d)
summary(aov_month)

#run an anova for month vs hour
aov_hour2 <- aov(month~hour, data = d)
summary(aov_hour2)

#run an anova for month vs hour
aov_hour3 <- aov(hour~month, data = d)
summary(aov_hour3)

#run an anova for year vs collision type
aov_year <- aov(year~CRASHTYPE, data = d)
summary(aov_year)

#run an anova for year vs hour
aov_year2 <- aov(hour~year, data = d)
summary(aov_year2)

#run an anova for species vs hour
aov_speciesmonth <- aov(hour~Species, data = d)
summary(aov_speciesmonth)

#run an anova for month vs species
aov_speciesmonth2 <- aov(month~Species, data = d)
summary(aov_speciesmonth2)

#run an anova for month vs species
aov_speciesmonth3 <- aov(month~Species, data = speciesdata)
summary(aov_speciesmonth3)

#run an anova for hour vs county (general roadkill)
aov_county <- aov(hour~COUNTY, data = d)
summary(aov_county)

#run an anova for hour vs county (species-specifc data)
aov_county2 <- aov(hour~COUNTY, data = speciesdata)
summary(aov_county2)

#run an anova for hour vs light condition
aov_light <- aov(hour~LIGHTCONDI, data = d)
summary(aov_light)

#run an anova for hour vs road alignment
aov_road <- aov(hour~ROADALIGNM, data = d)
summary(aov_road)

#run an anova for hour vs weather condition
aov_weather <- aov(hour~WEATHERCON, data = d)
summary(aov_weather)


ggplot(data = d, aes(x = hour, y = COUNTY, fill = Species)) +
  geom_bar(stat = "identity") +
  ylab("County") +
  xlab("Hour of the day")


#plot the relationship between species specific collisions and county with axis labels
ggplot(data = speciesdata, aes(x = hour, y = COUNTY, fill= Species)) +
  geom_bar(stat = "identity") +
  ylab("County") +
  xlab("Collisions")

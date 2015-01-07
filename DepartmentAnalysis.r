install.packages("dplyr")
install.packages("tidyr")
install.packages("ggvis")
install.packages("forecast")
library(forecast)
library(ggvis)
library(dplyr)
library(tidyr)
#read in csv
classbyprof<- tbl_df(data = read.csv("classbyprof.csv"))
relclasses<- classbyprof %>% filter(SUB=="REL") %>%
  separate(TERM, into=c("Semester","Year"),"\\ ") %>%
  arrange(COURSE, Year, Semester) 

# group by course
relclassdata <- relclasses %>% 
  select (PROF.:Year, TITLE_SHORT_DESC, ACTUAL, MAX) %>%
  group_by(COURSE)%>%
  filter (n()>4)

#plot course and total students
#future: modify so that can adjust the actual number dynamically
relclassdata %>% ggvis(~factor(COURSE), ~ACTUAL)

#total classes by year by semester
sumclassdata <- relclasses %>% 
  select(COURSE, Year, Semester, TITLE_SHORT_DESC, ACTUAL) %>%
  group_by(COURSE, Year, Semester) %>%
  summarize(totalstd = sum(ACTUAL)) %>%
  arrange(COURSE)

#isolate just world religions
worldrel <- sumclassdata %>% filter(COURSE==1110 , Year>2008 , Year < 2015) 
#create time series from total students field
worldrelts=ts(worldrel$totalstd, start = 2009, end=2014, frequency=2)
plot(worldrelts)
forcast(worldrelts)
plot(forecast(worldrelts))

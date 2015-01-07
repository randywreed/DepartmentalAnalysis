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

#function to create time series object for classes, set Sem=all

selclasses <- function (Crn, yearin, yearout, fillyin, fillyout, Sem) {
  
  #isolate course, filters out anything not between yearin and yearout
  if (Sem == "all") {
    newrel<- sumclassdata %>% filter (COURSE==Crn, Year>yearin, Year<yearout)
    #create time series from total students, fillyin is the beginning year, fillyout is endin year
    # frequency set to twice yearly (semester)
    newts<-ts(newrel$totalstd, start=fillyin, end=fillyout, frequency=2)
    
    } else {
  newrel<- sumclassdata %>% filter (COURSE==Crn, Year>yearin, Year<yearout, Semester==Sem)
  #create time series from total students, fillyin is the beginning year, fillyout is endin year
  # frequency set to twice yearly (semester)
  newts<-ts(newrel$totalstd, start=fillyin, end=fillyout, frequency=1)
  
    }
   return(newts)
}
#isolate just world religions
#worldrel <- sumclassdata %>% filter(COURSE==1110 , Year>2008 , Year < 2015) 
#create time series from total students field
#worldrelts=ts(worldrel$totalstd, start = 2009, end=2014, frequency=2)

#World religions all semesters
worldrelts<-selclasses(1110, 2008, 2015, 2009, 2014, "all")
plot(worldrelts)
forecast(worldrelts)
plot(forecast(worldrelts))

#old testament all semesters
otrelts<-selclasses (2010, 2008, 2015, 2009, 2014, "all")
plot(otrelts)
forecast(otrelts)
plot(forecast(otrelts))

#new testament all semesters
ntrelts<-selclasses (2020, 2008, 2015, 2009, 2014, "all")
plot(ntrelts)
forecast(ntrelts)
plot(forecast(ntrelts))

ntfallts<-selclasses(2020, 2008, 2015, 2009, 2014, "Fall")
plot(ntfallts)
forecast(ntfallts)
plot(forecast(ntfallts))

ntspringts<-selclasses(2020,2008,2015,2009,2014,"Spring")
plot(ntspringts)
forecast(ntspringts)
plot(forecast(ntspringts))


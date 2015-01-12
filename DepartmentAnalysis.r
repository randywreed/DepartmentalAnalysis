require("dplyr")
require("forecast")
require("ggvis")
require("tidyr")
#read in csv
classbyprof<- tbl_df(data = read.csv("enrollment_history_data.csv"))
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
#relclassdata %>% plot(factor(COURSE), ACTUAL)

#mean classes by year by semester
meanclassdata <- relclasses %>% 
  select(COURSE, ACADEMIC_YEAR, Year, Semester, TITLE_SHORT_DESC, ACTUAL) %>%
  group_by(Year, Semester, COURSE) %>%
  summarize( avg_students=mean(ACTUAL)) %>%
  arrange(COURSE)

yrmeanclassdata <- relclasses %>% 
  select (COURSE, ACADEMIC_YEAR, TITLE_SHORT_DESC, ACTUAL) %>%
  group_by(ACADEMIC_YEAR, COURSE) %>%
  summarize(avg_students=mean(ACTUAL)) %>%
  arrange(COURSE)


#function to create time series object for classes, set Sem=all

selclasses <- function (classdata, Crn, yearin, yearout, fillyin, fillyout, Sem) {
  
  #isolate course, filters out anything not between yearin and yearout
  if (Sem == "all") {
    newrel<- classdata %>% filter (COURSE==Crn, Year>yearin, Year<yearout)
    #create time series from total students, fillyin is the beginning year, fillyout is endin year
    # frequency set to twice yearly (semester)
    newts<-ts(newrel[4], start=fillyin, end=fillyout, frequency=2)
    
    } else {
  newrel<- classdata %>% filter (COURSE==Crn, Year>yearin, Year<yearout, Semester==Sem)
  #create time series from total students, fillyin is the beginning year, fillyout is endin year
  # frequency set to twice yearly (semester)
  newts<-ts(newrel[4], start=fillyin, end=fillyout, frequency=1)
  
    }
   return(newts)
}

#this function is the same as selclasses except it use the variable ACADEMIC_YEAR instead of year
aselclasses <- function (classdata, Crn, yearin, yearout, fillyin, fillyout, Sem) {
  
  #isolate course, filters out anything not between yearin and yearout
  if (Sem == "all") {
    newrel<- classdata %>% filter (COURSE==Crn, ACADEMIC_YEAR>yearin, ACADEMIC_YEAR<yearout)
    #create time series from total students, fillyin is the beginning ACADEMIC_YEAR, fillyout is endin ACADEMIC_YEAR
    # frequency set to twice ACADEMIC_YEARly (semester)
    newts<-ts(newrel[4], start=fillyin, end=fillyout, frequency=2)
    
  } else {
    newrel<- classdata %>% filter (COURSE==Crn, ACADEMIC_YEAR>yearin, ACADEMIC_YEAR<yearout, Semester==Sem)
    #create time series from total students, fillyin is the beginning ACADEMIC_YEAR, fillyout is endin ACADEMIC_YEAR
    # frequency set to twice ACADEMIC_YEARly (semester)
    newts<-ts(newrel[4], start=fillyin, end=fillyout, frequency=1)
    
  }
  return(newts)
}

#isolate just world religions
#worldrel <- sumclassdata %>% filter(COURSE==1110 , Year>2008 , Year < 2015) 
#create time series from total students field
#worldrelts=ts(worldrel$totalstd, start = 2009, end=2014, frequency=2)

#World religions all semesters
worldrelts<-selclasses(meanclassdata, 1110, 2008, 2015, 2009, 2014, "all")
plot(worldrelts)
forecast(worldrelts)
plot(forecast(worldrelts))
plot(hw(worldrelts, h=5))

#old testament all semesters
otrelts<-selclasses (meanclassdata, 2010, 2008, 2015, 2009, 2014, "all")
plot(otrelts)
forecast(otrelts)
plot(forecast(otrelts))
accuracy(forecast(otrelts))
plot(hw(otrelts, h=5))
accuracy(hw(otrelts))

#new testament all semesters
ntrelts<-selclasses (meanclassdata, 2020, 2008, 2015, 2009, 2014, "all")
plot(ntrelts)
forecast(ntrelts)
fit1<-forecast(ntrelts)
plot(fit1)
fit2<-rwf(ntrelts, h=3, drift=TRUE)
plot(fit2)
fit3<-meanf(ntrelts,h=3)
plot(fit3)
fit4<-ets(ntrelts)
fit4f<-forecast(fit4)
plot(fit4f)
fit5<-hw(ntrelts, h=5)
plot(fit5)
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
accuracy(fit4f)
accuracy(fit5)

ntfallts<-selclasses(meanclassdata, 2020, 2008, 2015, 2009, 2014, "Fall")
plot(ntfallts)
forecast(ntfallts, h=3)
plot(forecast(ntfallts, h=3))
plot(holt(ntfallts, h=3))

ntspringts<-selclasses(meanclassdata, 2020,2008,2015,2009,2014,"Spring")
plot(ntspringts)
forecast(ntspringts, h=3)
plot(forecast(ntspringts, h=3))
plot(rwf(ntspringts,h=3, drift=TRUE))
plot(holt(ntspringts, h=3))

load.fun <- function(x) { 
  x <- as.character(substitute(x)) 
  if(isTRUE(x %in% .packages(all.available=TRUE))) { 
    eval(parse(text=paste("require(", x, ")", sep=""))) 
  } else { 
    #update.packages() # recommended before installing so that 
    #dependencies are the latest version 
    eval(parse(text=paste("install.packages('", x, "')", sep=""))) 
  } 
} 
#--Produces a data.frame with the Source Data+Training Data, Fitted Values+Forecast Values, forecast data Confidence Intervals
funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}

load.fun("dplyr")
load.fun("forecast")
load.fun("ggvis")
load.fun("tidyr")
load.fun("tseries")
#install.packages("xtsExtra", repos="http://R-Forge.R-project.org")
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
#computer seasonal differencing
SeasonDiff <- function(x) {
  ns<-nsdiffs(x)
if(ns > 0) {
  xstar <- diff(x,lag=frequency(x),differences=ns)
} else {
  xstar <- x
}
nd <- ndiffs(xstar)
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
}
return(xstar)
}
#isolate just world religions
#worldrel <- sumclassdata %>% filter(COURSE==1110 , Year>2008 , Year < 2015) 
#create time series from total students field
#worldrelts=ts(worldrel$totalstd, start = 2009, end=2014, frequency=2)

#World religions all semesters
worldrelts<-selclasses(meanclassdata, 1110, 2008, 2015, 2009, 2014, "all")
plot(worldrelts)
wfit1<-forecast(worldrelts)
plot(wfit1)
wfit2<-hw(worldrelts, h=5)
plot(wfit2, xlab="Year", ylab="Average class Size", sub = "World Religions")
pd<-funggcast(worldrelts, wfit1)
adf.test(worldrelts, alternative="stationary")
out<-SeasonDiff(worldrelts)
plot(out)
#bad model
#wfit3<-Arima(worldrelts, seasonal=c(0,0,0))
#wfit3
wfit4<-Arima(worldrelts, seasonal=c(0,1,1))
wfit4
res<-residuals(wfit4)
tsdisplay(res)
Box.test(res,lag=16, fitdf=2, type="Ljung")
plot(forecast(wfit4, h=8))
auto.arima(worldrelts)

wfit5<-Arima(worldrelts, seasonal=c(1,1,0))
wfit5
plot(forecast(wfit5, h=8))

#old testament all semesters
otrelts<-selclasses (meanclassdata, 2010, 2008, 2015, 2009, 2014, "all")
plot(otrelts)
forecast(otrelts)
plot(forecast(otrelts))
accuracy(forecast(otrelts))
ofit0<-hw(otrelts,h=8)
ofit0
plot(ofit0, xlab="Year", ylab="Average Class Size", sub="Old Testament")
accuracy(ofit0)
auto.arima(otrelts)
ofit1<-Arima(otrelts, order=c(0,0,0) )
plot(forecast(ofit1, h=8))
ofit2<-Arima(otrelts, seasonal=c(1,1,0))
ofit2
plot(forecast(ofit2, h=8))
accuracy(ofit2)


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
fit5<-hw(ntrelts, h=8)
fit5
plot(fit5, xlab="Year",ylab="Average Class Size",sub="New Testanment")
fit6<-snaive(ntrelts, h=5)
plot(fit6, xlab="Year", ylab="Average Class Size", sub="New Testament")
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
accuracy(fit4f)
accuracy(fit5)
accuracy(fit6)


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


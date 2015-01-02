install.packages("dplyr")
install.packages("tidyr")
install.packages("ggvis")
library(ggvis)
library(dplyr)1834
library(tidyr)
#read in csv
classbyprof<- tbl_df(data = read.csv("classbyprof.csv"))
relclasses<- classbyprof %>% filter(SUB=="REL") %>%
  separate(TERM, into=c("Semester","Year"),"\\ ") %>%
  arrange(COURSE, Year, Semester) 
  
relclassdata <- relclasses %>% 
  select (PROF.:Year, TITLE_SHORT_DESC, ACTUAL, MAX) %>%
  group_by(COURSE)%>%
  filter (n()>4)

relclassdata %>% ggvis(~factor(COURSE), ~ACTUAL)

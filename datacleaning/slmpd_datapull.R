library(compstatr)
library(data.table)
library(tidyverse)
library(lubridate)

# Don't Run this code chunk

path="C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/hw2-lgunathi/datacleaning/policedata"

setwd(path)

  i <- cs_create_index()

  df19<-cs_get_data(year = 2019,index = i)
  df18<-cs_get_data(year = 2018,index = i)
  df20 <- cs_get_data(year = 2020, index =i)
#  df20<-cs_get_data(year = 2020,index = i)
  
  for(i in seq_along(df19)) {
    write.table(df19[[i]],paste(names(df19)[i], "2019.csv", sep = ""), 
                col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)
  }
  
  for(i in seq_along(df18)) {
    write.table(df18[[i]],paste(names(df18)[i], "2018.csv", sep = ""), 
                col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)
  }
  

  for(i in seq_along(df20)) {
    write.table(df18[[i]],paste(names(df18)[i], "2020.csv", sep = ""), 
                col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)
  }
  
  
  
  myMergedData <-  
    do.call(rbind,
            lapply(list.files(path = path,full.names = TRUE), fread))
  
  
# Data Preparation

myMergedData<- myMergedData%>% select(-c(flag_crime,flag_unfounded,flag_cleanup,
                                         ileads_address,location_name,location_comment,flag_administrative))


# Extracting the date values
myMergedData$date<-
  lubridate::mdy_hm(myMergedData$date_occur)
myMergedData$coded_year<-
  year(lubridate::ymd(myMergedData$coded_month, truncated = 1))
myMergedData$codedmonth<-
  month(lubridate::ymd(myMergedData$coded_month, truncated = 1))
myMergedData$Month<-month(as.Date(myMergedData$date,format='%Y/%m/%d'),label = TRUE)
myMergedData$timeofday <- hour(myMergedData$date)



police_data<-myMergedData%>%
  filter(count!="-1")

violent_crimecodes<-c(21000:29999, # Rape
                      10000, # Homicide
                      30000:39999, #Roberry
                      40000:49999, #Assault
                      50000:59999) # Burglary


violent_crimes<-police_data%>%
  mutate(UCR_casetype=case_when(crime %in% 21000:29999~"Rape",
                                crime %in% 30000:39999~"Roberry",
                                crime %in% 40000:49999~"Assault",
                                crime %in% 50000:59999~"Burglary",
                                crime %in% 10000~"Homicide"))


violent_crimes <- violent_crimes%>% filter(UCR_casetype!='NA')

fwrite(violent_crimes,"C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/hw2-lgunathi/app/violent_crimes.csv")
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


police_data<-myMergedData%>%
  filter(count!="-1")

fwrite(police_data,"C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/hw2-lgunathi/app/policedata.csv")
library(tidyverse)
setwd("~/Desktop/dataScience")

#---------------house rank------
Town = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, Country)


House_price = read_csv("CleanedData/HousePrices.csv")

Houseprice= House_price %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()
housePrices=Houseprice  %>% 
  filter(Year=="2022") %>% 
  group_by(Town) %>% 
  summarise(Price=mean(Price)) %>% 
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(Town, HouseScore)
housePrices



#Broadband rank

speed_downloads = read_csv("CleanedData/Broadband.csv")

Speed_Download = speed_downloads %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()

download_speed=Speed_Download%>% 
  group_by(Town) %>% 
  summarise(downloadSpeed=AverageDownload) %>% 
  arrange(downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/120000)) %>% 
  select(Town,DownloadScore)
download_speed


#crime score rank
crime_score=read_csv("CleanedData/Crime.csv")
crime_rank = crime_score %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(Town) %>% 
  summarise(score=mean(n)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1200)) %>% 
  select(Town,score)
crime_rank


#school score
school_score=read_csv("CleanedData/School.csv")
school_rank = school_score %>%
  rename(shortPostcode=shortPostCode) %>% 
  left_join(Town,by="shortPostcode") %>% 
  na.omit()

school_rank=school_rank%>% 
  group_by(District,SchoolName) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1800)) %>% 
  select(District,score,SchoolName)
school_rank



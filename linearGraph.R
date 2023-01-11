library(tidyverse)
library(ggplot2)
setwd("~/Desktop/dataScience")

#liner regression 
#Reading the cleaned files
Towns = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, Country)
prices = read_csv("CleanedData/HousePrices.csv")

speeds = read_csv("CleanedData/Broadband.csv") %>% 
  na.omit()  

crime=read_csv("CleanedData/Crime.csv")

schools=read_csv("CleanedData/School.csv") %>% 
  na.omit()



#------------------------------House prices vs Download Speed----------------------------------------


options(scipen=999)

HousePrices = prices %>%
  filter(Year=="2022") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,Country) %>%
  summarise(Price=mean(Price))

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,Country) %>%
  summarise(AverageDownload=mean(AverageDownload))


lm_res = HousePrices %>% 
  left_join(BroardbandSpeeds,by="Town")
model = lm(data= lm_res, Price~AverageDownload)
summary(model)

color= c("LANCASHIRE" = "red", "LEICESTERSHIRE" = "blue")

ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
  geom_point(data = filter(lm_res,Country.y=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res,Country.y=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Download Speed (Mbit/s)",y="Price",title="House Prices vs Download Speed",color="Country")


#-----------------------------------------------------------------------------------------

#----------------------------------House price and drug offence--------------------------------------------------


HousePrices = prices %>%
  filter(Year=="2022") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,Country) %>%
  summarise(Price=mean(Price))

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,Country) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res1 = HousePrices %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model1 = lm(data= lm_res1, Price~n)
summary(model1)

color= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "green")

ggplot(lm_res1,aes(x=n,y=Price)) +
  geom_point(data = filter(lm_res1,Country.x=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res1,Country.x=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drug count",y="Price",title="House Prices vs Drug",color="Country")


#----------------------------Drug and  school -----------------------------------

school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,Country) %>%
  
  summarise(score=mean(Attainment8Score)) 

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,Country) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res2 = school_lm %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model2 = lm(data= lm_res2, n~score)
summary(model2)

color1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

ggplot(lm_res2,aes(x=n,y=score)) +
  geom_point(data = filter(lm_res2,Country.x=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res2,Country.x=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drug Count",y="Score",title="Attainment8Score vs Drug",color="Country")


#---------------------------------Average download and drug-------------------
BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,Country) %>%
  summarise(AverageDownload=mean(AverageDownload))


Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,Country) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res3 = BroardbandSpeeds %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model3 = lm(data= lm_res3, AverageDownload~n)
summary(model3)

color1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

ggplot(lm_res3,aes(x=n,y=AverageDownload)) +
  geom_point(data = filter(lm_res3,Country.x=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res3,Country.x=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drug Count",y="Average Download",title="Average Download vs Drug",color="Country")


#-----------------------average download and school-------------------------

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,Country) %>%
  summarise(AverageDownload=mean(AverageDownload))

school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,Country) %>%
  
  summarise(score=mean(Attainment8Score)) 


lm_res4 = BroardbandSpeeds %>% left_join(school_lm,by="Country") %>% 
  na.omit()
model4 = lm(data= lm_res4, AverageDownload~score)
summary(model4)

color1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

ggplot(lm_res4,aes(x=score,y=AverageDownload)) +
  geom_point(data = filter(lm_res4,Country=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res4,Country=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Average Download",title="Score Vs AverageDownload",color="Country")


#-----------------------Attainment8Score and HousePrice-------------------------

HousePrices = prices %>%
  filter(Year=="2022") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,Country) %>%
  summarise(Price=mean(Price))



school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,Country) %>%
  
  summarise(score=mean(Attainment8Score)) 


lm_res5 = HousePrices %>% left_join(school_lm,by="Town") %>% 
  na.omit()
model5 = lm(data= lm_res5, score~Price)
summary(model5)

color1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

ggplot(lm_res5,aes(x=score,y=Price)) +
  geom_point(data = filter(lm_res5,Country.x=="LEICESTERSHIRE"),aes(color="LEICESTERSHIRE"))+
  geom_point(data = filter(lm_res5,Country.x=="LANCASHIRE"), aes(color="LANCASHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Price",title="Score Vs House Price",color="Country")



            

            
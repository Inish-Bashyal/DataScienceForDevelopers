library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)

setwd("~/Desktop/dataCcience")

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")


#---------------graph of Average house price ------------------
Towns = read_csv("CleanedData/Towns.csv")
HousePrices=read_csv("CleanedData/HousePrices.csv")

HousePricesclean <- HousePrices %>% 
  left_join(Towns, by ="shortPostcode")

House_town = HousePricesclean %>% 
  filter(Country=="LANCASHIRE"|Country=="LEICESTERSHIRE") %>% 
  group_by(Town,District,Country) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,Country) %>%
  na.omit()

# BOXPLOT Average house prices by district (2019-2022)
House_town %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2022 house prices by district")



# BARGRAPH houseprices by district (2019)
HousePricesclean %>% 
  filter(Year.x == 2019) %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice)) +
  geom_bar(position = "stack",stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(limits=c(0,5000000),breaks = seq(0, 5000000, 30000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.25) +
  labs(title = "2019 Average house prices by district") +
  coord_flip()


#LINEGRAPH Average house prices by year (2019-2022)
HousePricesclean %>% 
  group_by(Year.x) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Year.x, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 300000, 5000), 
                     label = euro) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2022 Average house prices by year")


# BOXPLOT Average house prices by district (2019-2022)
HousePricesclean %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2022 house prices by district")



# BOXPLOT Average house prices by district (2019)
HousePricesclean %>% 
  filter(Year.x == 2019) %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019 house prices by district")


# Piechart for 2022 Robbery by District

crime_Data = read_csv("CleanedData/Crime.csv")

crimeData = crime_Data %>% 
  left_join(Towns, by = "shortPostcode") %>% 
  na.omit()

RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year.y == 2022) %>%
  group_by(Town) %>%
  mutate(sumCount = sum(n)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(Town, sumCount, perc, labels) %>% 
  select(Town, sumCount, perc, labels)

RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = Town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2020 Robbery by District")


# Boxplot for 2019-2022 Drugs count by District
crimeData %>% 
  filter(CrimeType == "Drugs") %>% 
  ggplot(aes(x=District, y=n, fill=CrimeType)) + 
  geom_boxplot() +
  labs(title=" 2019-2022 Drugs count by District")+
  coord_flip()


#-----------------school graph---------------

schoolData = read_csv('CleanedData/School.csv', show_col_types = FALSE)



# Linegraph Average Attainment8Score by year
schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by 2019-2021")


# Boxplot of year 2019-2021 where Attainment8Score is greater than 30
schoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000))+
  labs(title="2019-2021 Attainment8Score of Schools")+
  coord_flip() 
  
schoolData

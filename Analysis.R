# # # # # # # # # # # # # # # # # # # # # # # # 
#  PROJECT II: San Francisco Crime (2016)     #
# # # # # # # # # # # # # # # # # # # # # # # # 

setwd("SanfranciscoCrime")

# # # # # # # # # # # # # # # # 
#  Install and Load libraries #
# # # # # # # # # # # # # # # #

if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")


#  Load Libraries
library(ggthemes)
library(Hmisc)
library(tidyr)
library(dslabs)
library(tidyverse)
library(caret)
library(data.table)
library(corrplot)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(gridExtra)

# # # # # # # # # # # # 
#  Load  data         #
# # # # # # # # # # # #
url <- https://www.kaggle.com/roshansharma/sanfranciso-crime-dataset
sf_crime <- read.csv("sf_crime.csv", header=TRUE, sep = ",", row.names = NULL, quote = "\"")

# Names of columns without spaces
names(sf_crime) <- make.names(names(sf_crime), unique=TRUE)

#  Change the format from scientific to numerical
options(scipen = 999)

sf_crime<-as.data.frame(sf_crime, stringsAsFactors=TRUE) %>% mutate(IncidntNum = as.numeric(IncidntNum), 
                                                                    Category = as.character(Category),
                                                                    Descript = as.character(Descript),
                                                                    Resolution = as.character(Resolution),
                                                                    DayOfWeek = factor(DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                                                   "Friday", "Saturday", "Sunday")),
                                                                    PdDistrict = as.character(PdDistrict))




# # # # # # # # # # # # # # # # # #
#   Data Description and Cleaning # 
# # # # # # # # # # # # # # # # # #

#  Describe sf_crime data set
describe(sf_crime)

#  Check missing values
sum(is.na(sf_crime))

#  Show first 10 rows 
print(head(sf_crime,10))

#  Summary of the data set
summary(sf_crime)

#  Number of variables
print(ncol(sf_crime))

#  Number of observations
print(nrow(sf_crime))

#  Convert year character into class Date
sf_crime <- transform(sf_crime, Date_time = as.Date(Date, "%m/%d/%Y", tz = "UTC"))

#  Create variables Date_month, Date_day, Date_year
sf_crime<-sf_crime %>% mutate(Date_month = month(Date_time), 
                              Date_day = day(Date_time), Date_year = year(Date_time)) %>%
                       mutate(Month = month.name[Date_month])

sf_crime <- sf_crime  %>% mutate (Month = factor(Month, levels = month.name))


#  Order the data by increasing incident number IncidntNum(incident ID)
sf_crime<-arrange(sf_crime, by=IncidntNum)

#   Find duplicates among IncidntNum(incident ID)
duplicated(sf_crime$IncidntNum)

#  Total number of duplicates among among IncidntNum(incident ID)
print(sum(duplicated(sf_crime$IncidntNum)))

#  Find index of the duplicates among IncidntNum(incident ID)
dup <- sf_crime$IncidntNum[duplicated(sf_crime$IncidntNum)]

#  Print 20 incident ID's with more than one crime
head(dup, 20) %>% knitr::kable("simple")

#  Summary of all variables
lapply(sf_crime, summary)

Week_Day<-weekdays(sf_crime$Date_time)

#  Remove columns after data cleaning
within(sf_crime, rm(Date))

#  Processed sf_crime data
head(sf_crime,10)%>%knitr::kable("simple")

#  Description of variables in processed sf_crime
str(sf_crime)


# # # # # # # # # # # # # # # # # # # # # # # # # # 
#    Visualization and Descriptive statistics     # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 

#  Top 10 crimes with the greatest number of occurrence
sf_crime%>% group_by(Category)%>%
            summarize(count = n()) %>% arrange(desc(count)) %>% 
            top_n(10, count) %>%knitr::kable()

#  Number of unique incidents, crime categories, description of crime, addresses, and districts
sf_crime %>% summarise(n_incidents = n_distinct(IncidntNum), 
                       n_crime_categorie = n_distinct(Category),
                       n_description = n_distinct(Descript),
                       n_district = n_distinct(PdDistrict),
                       n_address = n_distinct(Address)) %>%knitr::kable()

#  Number of crime occurrences for each district
sf_crime %>% group_by(PdDistrict) %>% 
  summarize(count=n())%>%
  ggplot(aes(x= reorder(PdDistrict, count), y = count))+
  geom_bar(stat='identity', alpha=.5, color = "black", fill = "red")+
  scale_fill_continuous(type = "viridis") +
  ggtitle("Occurrence of Crime by District") +
  coord_flip(y=c(0, 30000))+
  labs(x="District", y="Number of Crime Occurrences")+
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  theme(plot.title = element_text(hjust = 0.5))

#  TOP 10 most frequent Crimes by Category
sf_crime %>%group_by(Category) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10, count) %>%
  ggplot(aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "blue") + theme_bw()+
  ggtitle("TOP 10 Crimes by Category") +
  labs(x = "Category", y = "Number of Occurrences")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Pie-Chart Crime Frequency on weekdays
sf_crime %>% 
  group_by(DayOfWeek) %>% 
  summarize(count=n()) %>%
  mutate(percent = count /sum(count) *100.0) %>%
  ggplot( aes(x="", y = - percent, fill = DayOfWeek)) +
  geom_bar(width = 1, stat = "identity", color = "black") + coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percent), "%", sep = "\n", DayOfWeek)), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Crime Occurances on Weekdays") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("orange", "red", "yellow", "blue",  "green", "grey", "purple")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

#  Number of Crime Occurrences on Weekdays
sf_crime  %>% group_by(Month) %>% 
 summarize(count=n()) %>% arrange(desc(count))  %>% 
  ggplot(aes(x = Month, y = count)) +
  geom_bar(stat="identity", alpha=.5, color="red", fill= "purple", position = 'dodge') + theme_bw()+
  scale_fill_continuous(type = "viridis") +
  ggtitle("Crimes on Weekdays") +
  labs(title = "Crime over Months",
           x = "Month", 
           y = "Number of Occurrences")+
  geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .70), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  scale_y_continuous(breaks=seq(0, 15000, by= 5000)) +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Top 10 of most common crime resolutions
sf_crime %>%group_by(Resolution) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10, count) %>%
  ggplot(aes(x = reorder(Resolution, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "green") + theme_bw()+
  ggtitle("TOP 10 Crime Resolutions") +
  labs(x = "Resolution Category", y = "Number of Occurrences")+
  geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .70), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2.25 of incidents included more than one crime categories




sf_crime %>%filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", "ASSAULT",    
                                   "VANDALISM", "VEHICLE THEFT", "WARRANTS", "BURGLARY", 
                                   "SUSPICIOUS OCC", "MISSING PERSON"), Resolution %in% c("NONE", "ARREST, BOOKED")) %>%
  group_by(Category) %>% 
  mutate(count=n()) %>% arrange(desc(count)) %>%
  ggplot(aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color = "blue", fill = "grey" ) + theme_bw()+
  scale_fill_continuous(type = "viridis") +
  ggtitle("TOP 10 Crimes by Category") +
  facet_wrap(~Resolution) +
  ggtitle("TOP 10 Crime Resolutions") +
  labs(x = "Resolution Category", y = "Number of Occurrences")+
  #geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .60), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Top 10 of the most frequent addresses in San Francisco

sf_crime  %>% group_by(Address) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% 
  top_n(10, count) %>%knitr::kable()





  
  

  
  
  
  
  
  
  

#  Correlation between numeric class variables
cor.data<-cor(sf_crime[ , c("IncidntNum","Month", "Date_month", "X", "Y")])
corrplot(cor.data, method='circle', type = "upper")


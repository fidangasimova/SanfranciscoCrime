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

# # # # # # # # # # # # # # # # # #
#   Data Processing               # 
# # # # # # # # # # # # # # # # # #

sf_crime<-as.data.frame(sf_crime, stringsAsFactors=TRUE) %>%
  mutate(IncidntNum = as.numeric(IncidntNum),
         PdId = as.numeric(PdId),
         Category = as.character(Category),
         Descript = as.character(Descript),
         Resolution = as.character(Resolution),
         DayOfWeek = factor(DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday",
                                                  "Thursday","Friday", "Saturday", "Sunday")),
         PdDistrict = as.character(PdDistrict))

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

#  Convert char variable "Time" into a numeric variable
hhmm2dec <- function(x) {
  xlist <- strsplit(x,split=":")
  h <- as.numeric(sapply(xlist,"[",1))
  m <- as.numeric(sapply(xlist,"[",2))
  xdec <- h+(m/60)
  return(xdec)
}

#  Round variable "Time" without minutes
sf_crime$Time <- round(hhmm2dec(sf_crime$Time), 0)

# Replace Hours = 24 with Hours = 0
sf_crime$Time <-replace(sf_crime$Time, sf_crime$Time ==24, 0) 

#  Convert "Date" character into class Date
sf_crime <- transform(sf_crime, Date_time = as.Date(Date, "%m/%d/%Y", tz = "UTC"))

#  Create variables "Date_month", "Date_day", "Date_year"
sf_crime<-sf_crime %>% mutate(Date_Month = month(Date_time), 
                              Date_Day   = day(Date_time),
                              Date_Year  = year(Date_time)) %>%
                      mutate(Month = month.name[Date_Month])
                      
#  Convert "Month" variable into an ordered factor                      
sf_crime<- sf_crime %>% mutate(Month = factor(Month, levels = month.name))

#  Create new dummy variable "Resolution_dummy" (1 = resolved, 0 = unresolved)
sf_crime$Resolution_dummy <- ifelse(sf_crime$Resolution == "NONE", 0, 1)

#  Create a new dummy variable "Category_violent" (1 = violent, 0 = nonviolent)
sf_crime$Category_Violent_dummy <- ifelse (sf_crime$Category %in% c("ASSAULT", "ROBBERY", "SEX OFFENSES, FORCIBLE", "KIDNAPPING"), 1, 0)  

#  Create a new variable "Count_Address_Occur" to count the number of Occurrences of Addresses 
sf_crime <- sf_crime %>% group_by(Address) %>%
  mutate(Count_Address_Occur = n())  %>%
  arrange(desc(Count_Address_Occur))  

#  Remove columns after data cleaning
rm(Date, Date_time)


#  Reorder columns of sf_crime data
sf_crime<-sf_crime[, c("PdId",  "IncidntNum", "Category","Category_Violent_dummy", "Descript",  "Resolution","Resolution_dummy", 
                      "DayOfWeek","Date_Day", "Date_Year", "Date_Month", "Month", "Time", "Date_time",                                                       
                      "PdDistrict", "Address", "Count_Address_Occur", "Y",  "X", "Location")]

#  Order the data by increasing incident number "PdId"(police department ID)
sf_crime <- arrange(sf_crime, by = PdId)

#  View processed data set 
View(sf_crime)

# # # # # # # # # # # # # # # # # # # # # # # # # # 
#    Visualization and Descriptive statistics     # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 

#   Find duplicates among "IncidntNum"(incident ID)
duplicated(sf_crime$IncidntNum)

#  Total number of duplicates among among "IncidntNum"(incident ID)
print(sum(duplicated(sf_crime$IncidntNum)))

#  Find index of the duplicates among "IncidntNum" (incident ID)
dup <- sf_crime$IncidntNum[duplicated(sf_crime$IncidntNum)]

#  Print 20 incident ID's with more than one crime
head(dup, 20) %>% knitr::kable("simple")

#  Summary of all variables
lapply(sf_crime, summary)

#  Processed sf_crime data
head(sf_crime, 10) %>% knitr::kable("simple")

#  Description of variables in processed sf_crime
str(sf_crime)

#  Number of unique incidents, crime categories, description of crime, addresses, districts and resolution
sf_crime %>% summarise(n_incidents = n_distinct(IncidntNum), 
                       n_crime_category = n_distinct(Category),
                       n_description    = n_distinct(Descript),
                       n_district       = n_distinct(PdDistrict),
                       n_resolution     = n_distinct(Resolution),
                       n_address        = n_distinct(Address)) %>% knitr::kable()



# # # # # # # # # # 
#   District      # 
# # # # # # # # # #

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

# # # # # # # # # # 
#   Category      # 
# # # # # # # # # #

#  Crime Category
print(unique(sf_crime$Category))

#  Top 10 crimes with the greatest number of occurrence
sf_crime %>% group_by(Category)%>%
  summarize(count = n()) %>% arrange(desc(count)) %>% 
  top_n(10, count) %>%knitr::kable()

#  TOP 10 most frequent Crimes by Category
sf_crime %>%group_by(Category) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10, count) %>%
  ggplot(aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "blue") + theme_bw()+
  ggtitle("TOP 10 Crimes by Category") +
  labs(x = "Category", y = "Number of Occurrences")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # # # # # # # # # 
#   Weekdays      # 
# # # # # # # # # #

#  Pie-Chart Crime Frequency on Weekdays
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
           y = "Number of Occurrences") +
  geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .70), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  scale_y_continuous(breaks=seq(0, 15000, by= 5000)) +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # # # # # # # # # 
#  Resolution     # 
# # # # # # # # # #

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

#  Top 10 Crimes among 2 Resolution categories "NONE" and "BOOKED ARREST"
sf_crime %>%filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", "ASSAULT",    
                                   "VANDALISM", "VEHICLE THEFT", "WARRANTS", "BURGLARY", 
                                   "SUSPICIOUS OCC", "MISSING PERSON"), Resolution %in% c("NONE", "ARREST, BOOKED")) %>%
  group_by(Category) %>% 
  mutate(count=n()) %>% arrange(desc(count)) %>%
  ggplot(aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color = "blue", fill = "grey" ) + theme_bw()+
  scale_fill_continuous(type = "viridis") +
  ggtitle("TOP 10 Crimes by Category") +
  facet_wrap(~Resolution, ) +
  ggtitle("TOP 10 Crime Resolutions") +
  labs(x = "Resolution Category", y = "Number of Occurrences")+
  #geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .60), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#  Top 10 of the most frequent crime descriptions in San Francisco
sf_crime  %>% group_by(Descript) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% 
  top_n(10, count) %>%knitr::kable()

# # # # # # # # # # 
#   Day Time      # 
# # # # # # # # # #

#  Occurrence of crime during a day time
sf_crime %>%group_by(Time) %>% 
  summarize(count=n(), count_min = min(count), count_max = max(count)) %>%
  ggplot(aes(x = Time, y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "green") + theme_bw()+
  geom_vline(xintercept = 5, colour = "red") +
  geom_vline(xintercept = 18, colour = "red") +
  ggtitle("Hour of the Day") +
  labs(x = "Hours", y = "Number of Occurrences")+
  scale_x_continuous(breaks=seq(0, 23, by= 1))+
  geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .60), hjust=0.5, size = 2, inherit.aes = TRUE) 

#  Top 10 crime category over day time
sf_crime%>% group_by(Time, Category) %>% 
  summarise(count = n()) %>%
  filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", "ASSAULT",    
                         "VANDALISM", "VEHICLE THEFT", "WARRANTS", "BURGLARY", 
                         "SUSPICIOUS OCC", "MISSING PERSON")) %>%
  ggplot(aes(x = Time, y = count)) +
  geom_line(aes(color=Category)) +
  ggtitle("Hour of the Day") +
  scale_x_continuous(breaks=seq(0, 23, by= 1))+
  scale_fill_brewer(palette = "Paired")+ 
  theme(plot.title = element_text(hjust = 0.5))

# # # # # # # # # # 
#   Address       # 
# # # # # # # # # #

#  Top 20 of the most frequent addresses in San Francisco
sf_crime  %>% group_by(Address) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% 
  top_n(20, count) %>%knitr::kable()

#  Most common Address
sf_crime %>% group_by(Address) %>%
  summarize(count = n())  %>% arrange(desc(count)) %>% top_n(20) %>%
  ggplot(aes(reorder(Address, count), count, fill = count)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = "PuBuGn") +
  ggtitle("") + xlab("Address") +
  ggtitle("Address for Number of  Crime Occurrences") +
  theme_classic()

# 800 Block of BRYANT ST is the Address of a county jail

# # # # # # # # # # # # # # # # # # # # # # 
#  Validation set will be 10% of edx data   
# # # # # # # # # # # # # # # # # # # # # # 

#  Code to generate the validation set
set.seed(342)
test_index <- createDataPartition(y = sf_crime$Category, times = 1, p = 0.1, list = FALSE)
sf_crime_p <- sf_crime[-test_index,]
temp <- sf_crime[test_index,]

#  Make sure "IncidntNum" in validation set are also in sf_crime_p set
validation <- temp %>% 
  semi_join(sf_crime_p, by = "IncidntNum")%>%
  
#  Add rows removed from validation set back into sf_crime_p set
removed <- anti_join(temp, validation)
sf_crime_p <- rbind(sf_crime_p, removed)

rm(test_index, temp, removed)  
  

# # # # # # # #  
#  Method     # 
# # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  Generate train and test sets, 20% of edx data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(110)
test_index <- createDataPartition(y=sf_crime_p$Category, times = 1, p = 0.2, list = FALSE)
test_set <- sf_crime_p[test_index, ]
train_set <- sf_crime_p[-test_index, ]  

#  To make sure we don’t include incident Ids in the test set that do not appear in the training set, 
#  remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "IncidntNum") 

# RMSE FUNCTION: For the evaluation of the model RMSE function will be used
RMSE <- function(true_category, predicted_category){
  sqrt(mean((true_category - predicted_category)^2, na.rm = T))
}


# define the outcome and predictors
y <- sf_crime$Category %>% factor()
x <- sf_crime$Time

test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
train_set <- sf_crime %>% slice(-test_index)
test_set <- sf_crime %>% slice(test_index)

test_set$Category
train_qda <- train( Category ~ Count_Address_Occur , method = "qda", data = train_set) %>% head()
train_qda <- train( Category ~ Count_Address_Occur , method = "lda", data = train_set) %>% head()
train_qda <- train( Category ~ Count_Address_Occur , method = "knn", tuneGrid = data.frame(k = seq(1, 10, 2)), data = train_set) %>% head()

                    
                    # Obtain predictors and accuracy
predicted_category <- predict(train_qda, test_set) %>% factor()

confusionMatrix(data = predicted_category, reference = test_set$Category%>% factor())$overall["Accuracy"]

rmse <-RMSE(predicted_category, test_set$Category)  

  
# As the correlation shows predictors are not highly correlated, so PCA analysis would not be appropriate method
#  Correlation between numeric class variables
cor.data <- cor(sf_crime[, c("Resolution_dummy","Category_violent_dummy", 
                             "Count_Address_Occur", "Time","Date_Day", "Date_Month")]) %>% as.matrix
corrplot(cor.data, order = "hclust", addrect = 2, type = "lower")


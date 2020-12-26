#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
#   PROJECT II: San Francisco Crime (2016)      # 
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  


#  #  #  #  #  #  #  #  #  #  #  #  
#   Install and Load libraries   # 
#  #  #  #  #  #  #  #  #  #  #  #  

if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
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


#   Load Libraries
library(tinytex)
library(factoextra)
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

#  #  #  #  #  #  #  #  #  
#   Load  data          # 
#  #  #  #  #  #  #  #  #  

# Install tinytex (for running latex) if having not installed
# tinytex::install_tinytex()


# url <- https://www.kaggle.com/roshansharma/sanfranciso-crime-dataset
sf_crime <- read.csv("sf_crime.csv", header=TRUE, sep = ",", row.names = NULL, quote = "\"")

#  Names of columns without spaces
names(sf_crime) <- make.names(names(sf_crime), unique=TRUE)

#  Change the format from scientific to numerical
options(scipen = 999)

#  #  #  #  #  #  #  #  #  #  #  #  #  
#    Data Processing                #  
#  #  #  #  #  #  #  #  #  #  #  #  #  

sf_crime<-as.data.frame(sf_crime, stringsAsFactors=TRUE) %>%
         mutate(IncidntNum = as.numeric(IncidntNum),
                PdId       = as.numeric(PdId),
                Category   = factor(Category, levels = c("MISSING PERSON", "LARCENY/THEFT", "OTHER OFFENSES", "BURGLARY",                   
                                                "SUSPICIOUS OCC","WARRANTS", "ASSAULT","NON-CRIMINAL",               
                                                "STOLEN PROPERTY", "WEAPON LAWS", "DRUG/NARCOTIC","EMBEZZLEMENT",               
                                                "RUNAWAY", "DRUNKENNESS", "FORGERY/COUNTERFEITING","ROBBERY",                     
                                                "VEHICLE THEFT", "FRAUD", "SEX OFFENSES, FORCIBLE", "SECONDARY CODES",            
                                                "KIDNAPPING", "VANDALISM", "PROSTITUTION", "DRIVING UNDER THE INFLUENCE",
                                                "RECOVERED VEHICLE", "SEX OFFENSES, NON FORCIBLE","TRESPASS", "ARSON",                      
                                                "DISORDERLY CONDUCT", "LIQUOR LAWS", "FAMILY OFFENSES","EXTORTION",                  
                                                "BAD CHECKS", "LOITERING", "SUICIDE", "BRIBERY", "GAMBLING","PORNOGRAPHY/OBSCENE MAT", "TREA")),
              Descript    = as.character(Descript) %>% factor(),
              Resolution  = as.character(Resolution),
              DayOfWeek   = factor(DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday",
                                                  "Thursday","Friday", "Saturday", "Sunday")),
               PdDistrict = as.character(PdDistrict))

#   Number of variables
print(ncol(sf_crime))

#   Number of observations
print(nrow(sf_crime))

#   Describe sf_crime data set
describe(sf_crime)

#   Check missing values
sum(is.na(sf_crime))

#   Show first 10 rows 
print(head(sf_crime,10))

#   Summary of the data set
summary(sf_crime)

#   Convert char variable "Time_Hour" into a numeric variable
hhmm2dec <- function(x) {
  xlist <- strsplit(x,split=":")
  h <- as.numeric(sapply(xlist,"[",1))
  m <- as.numeric(sapply(xlist,"[",2))
  xdec <- h+(m/60)
  return(xdec)
}

#   Round variable "Time_Hour" without minutes
sf_crime$Time_Hour <- round(hhmm2dec(sf_crime$Time), 0)

#  Replace Hours = 24 with Hours = 0
sf_crime$Time_Hour <-replace(sf_crime$Time_Hour, sf_crime$Time_Hour ==24, 0) 

#   Convert "Date" character into class Date
sf_crime <- transform(sf_crime, Date_time = as.Date(Date, "%m/%d/%Y", tz = "UTC"))

#   Create variables "Date_month", "Date_day", "Date_year"
sf_crime<-sf_crime %>% mutate(Date_Month = month(Date_time), 
                              Date_Day   = day(Date_time),
                              Date_Year  = year(Date_time)) %>%
                      mutate(Month = month.name[Date_Month])
                      
#   Convert "Month" variable into an ordered factor                      
sf_crime<- sf_crime %>% mutate(Month = factor(Month, levels = month.name))

#   Create new dummy variable "Resolution_dummy" (1 = resolved, 0 = unresolved)
sf_crime$Resolution_dummy <- ifelse(sf_crime$Resolution == "NONE", 0, 1)

#   Create a new dummy variable "Category_violent" (1 = violent, 0 = nonviolent)
sf_crime$Category_Violent_dummy <- ifelse (sf_crime$Category %in% c("ASSAULT", "ROBBERY", "SEX OFFENSES, FORCIBLE", "KIDNAPPING"), 1, 0)  

#   Create a new variable "Count_Address_Occur" to count the number of Occurrences of Addresses 
sf_crime<- sf_crime %>% group_by(Address) %>%
             mutate(Count_Address_Occur = n(), .groups = 'drop')  %>% ungroup() %>%
             arrange(desc(Count_Address_Occur))  

#   Create a new variable "Count_Category_Occur" to count the number of Occurrences of Categories 
sf_crime<- sf_crime %>% group_by(Category) %>%
             mutate(Count_Category_Occur = n(), .groups = 'drop')  %>% ungroup() %>%
             arrange(desc(Count_Category_Occur))  

#   Reorder columns of sf_crime data
sf_crime<-sf_crime[, c("PdId",  "IncidntNum", "Category", "Count_Category_Occur", "Category_Violent_dummy", "Descript",  "Resolution","Resolution_dummy", 
                      "DayOfWeek","Date_Day", "Date_Year", "Date_Month", "Month", "Time_Hour", "Date_time",                                                       
                      "PdDistrict", "Address", "Count_Address_Occur", "Y",  "X", "Location")]

#   Order the data by increasing incident number "PdId"(police department ID)
sf_crime <- arrange(sf_crime, by = PdId)

#   View processed data set 
View(sf_crime)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
#     Visualization and Descriptive statistics     #  
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 

#   Find duplicates among "IncidntNum"(incident number)
duplicated(sf_crime$IncidntNum)

#   Total number of duplicates among among "IncidntNum"(incident number)
print(sum(duplicated(sf_crime$IncidntNum)))

#   Find index of the duplicates among "IncidntNum" (incident number)
dup <- sf_crime$IncidntNum[duplicated(sf_crime$IncidntNum)]

#   Print 10 incident ID's with more than one crime
head(dup, 10) %>% knitr::kable("simple")

#   Summary of all variables
lapply(sf_crime, summary)

#   Processed sf_crime data
head(sf_crime, 5) 

#   Description of variables in processed sf_crime
str(sf_crime)

#   Number of unique incidents, crime categories, description of crime, addresses, districts and resolution
sf_crime %>% summarise(n_incidents      = n_distinct(PdId), 
                       n_crime_category = n_distinct(Category),
                       n_description    = n_distinct(Descript),
                       n_district       = n_distinct(PdDistrict),
                       n_resolution     = n_distinct(Resolution),
                       n_address        = n_distinct(Address), .groups = 'drop') 



#  #  #  #  #  #  #  #  #  #  
#    District              #  
#  #  #  #  #  #  #  #  #  # 

#   Number of crime occurrences for each district
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

#  #  #  #  #  #  #  #  #  #  
#    Category              #  
#  #  #  #  #  #  #  #  #  # 

#   Crime Category
class(sf_crime$Category)
nlevels(sf_crime$Category)
print(unique(sf_crime$Category))

#   Top 10 crimes with the greatest number of occurrence
sf_crime %>% group_by(Category)%>%
  summarize(count = n()) %>% arrange(desc(count)) %>% 
  top_n(10, count) %>%knitr::kable()

#   TOP 10 most frequent Crimes by Category
sf_crime %>%group_by(Category) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10, count) %>%
  ggplot(aes(x = reorder(Category, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "blue") + theme_bw()+
  ggtitle("TOP 10 Crimes by Category") +
  labs(x = "Category", y = "Number of Occurrences")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  #  #  #  #  #  #  #  
#    Weekdays        #  
#  #  #  #  #  #  #  # 

#   Pie-Chart Crime Frequency on Weekdays
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

#  #  #  #  #  #  #  #  
#   Months    #  
#  #  #  #  #  #  #  # 

#   Number of Crime Occurrences over Months
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

#  #  #  #  #  #  #  #  #  #  
#   Resolution             #  
#  #  #  #  #  #  #  #  #  # 

#   Top 10 of most common crime resolutions
sf_crime %>%group_by(Resolution) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10, count) %>%
  ggplot(aes(x = reorder(Resolution, -count), y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "green") + theme_bw()+
  ggtitle("TOP 10 Crime Resolutions") +
  labs(x = "Resolution Category", y = "Number of Occurrences")+
  geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .70), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#   Top 10 Crimes among 2 Resolution categories "NONE" and "BOOKED ARREST"
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
  # geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .60), hjust=0.5, size = 2, inherit.aes = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  #  #  #  #  #  #  #  #  #  
#   Description            #  
#  #  #  #  #  #  #  #  #  # 

#   Top 10 of the most frequent crime descriptions in San Francisco
sf_crime  %>% group_by(Descript) %>% 
             summarize(count=n()) %>% arrange(desc(count)) %>% 
             top_n(10, count) %>% knitr::kable()

#  #  #  #  #  #  #  #  #  #  
#    Day Time              #  
#  #  #  #  #  #  #  #  #  # 

#   Occurrence of crime during a day Time_Hour
sf_crime %>%group_by(Time_Hour) %>% 
  summarize(count=n(), count_min = min(count), count_max = max(count)) %>%
  ggplot(aes(x = Time_Hour, y = count)) +
  geom_bar(stat="identity", alpha=.5, color="black", fill= "green") + theme_bw()+
  geom_vline(xintercept = 5, colour = "red") +
  geom_vline(xintercept = 18, colour = "red") +
  ggtitle("Hour of the Day") +
  labs(x = "Hours", y = "Number of Occurrences")+
  scale_x_continuous(breaks=seq(0, 23, by= 1))+
  geom_text(aes(label= count),vjust = -0.5, position = position_dodge(width = .60), hjust=0.5, size = 2, inherit.aes = TRUE) 

#   Top 10 crime category over day time
sf_crime%>% group_by(Time_Hour, Category) %>% 
  summarise(count = n()) %>%
  filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", "ASSAULT",    
                         "VANDALISM", "VEHICLE THEFT", "WARRANTS", "BURGLARY", 
                         "SUSPICIOUS OCC", "MISSING PERSON")) %>%
  ggplot(aes(x = Time_Hour, y = count)) +
  geom_line(aes(color=Category)) +
  ggtitle("Hour of the Day") +
  scale_x_continuous(breaks=seq(0, 23, by= 1))+
  scale_fill_brewer(palette = "Paired")+ 
  theme(plot.title = element_text(hjust = 0.5))

#  #  #  #  #  #  #  #  
#    Address         #  
#  #  #  #  #  #  #  # 

#   Top 20 of the most frequent addresses in San Francisco
sf_crime  %>% group_by(Address) %>% 
  summarize(count=n()) %>% arrange(desc(count)) %>% 
  top_n(20, count) %>%knitr::kable()

#   Most common Address
sf_crime %>% group_by(Address) %>%
  summarize(count = n())  %>% arrange(desc(count)) %>% top_n(20) %>%
  ggplot(aes(reorder(Address, count), count, fill = count)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = "PuBuGn") +
  ggtitle("") + xlab("Address") +
  ggtitle("Address for Number of  Crime Occurrences") +
  theme_classic()

#  #  #  #  #  #   
#   Analysis   #  
#  #  #  #  #  #  


#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
#   Validation set will be 20% of sf_crime data    #   
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

#   Code to generate the validation set
set.seed(342)
test_index <- createDataPartition(y = sf_crime$Category_Violent_dummy , times = 1, p = 0.2, list = FALSE)
sf_crime_p <- sf_crime[-test_index,]
temp <- sf_crime[test_index,]

#   Make sure "IncidntNum" in validation set are also in sf_crime_p set
validation <- temp %>% 
          semi_join(sf_crime_p, by = "IncidntNum")
  
#   Add rows removed from validation set back into sf_crime_p set
removed <- anti_join(temp, validation)
sf_crime_p <- rbind(sf_crime_p, removed)

#  Remove redundant columns
rm(test_index, temp, removed)  
  

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
#   Generate train and test sets, 20% of sf_crime data        # 
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

set.seed(110)
test_index <- createDataPartition(y = sf_crime_p$Category_Violent_dummy, times = 1, p = 0.2, list = FALSE)
test_set <- sf_crime_p[test_index, ]
train_set <- sf_crime_p[-test_index, ]  

#   To make sure we don’t include incident Ids in the test set that do not appear in the training set, 
#   remove these entries using the semi_join function:
test_set <- test_set %>% 
        semi_join(train_set, by = "IncidntNum") 


#  As the correlation shows predictors are not highly correlated, so PCA analysis would not be appropriate method
#   Correlation between numeric class variables
cor.data <- cor(sf_crime[, c("Count_Category_Occur","Resolution_dummy","Category_Violent_dummy", 
                             "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y")]) %>% as.matrix
corrplot(cor.data, order = "hclust", addrect = 2, type = "lower")

# # # # # # # # # # # # # # # # # # # # # # # # 
#   Logistic regression with one predictor    # 
# # # # # # # # # # # # # # # # # # # # # # # # 

#   Fit Logistic regression with one predictor
fit_glm <- glm(Category_Violent_dummy ~ Resolution_dummy, data=train_set, family = "binomial")

#   Predict "Category_Violent_dummy"
p_hat_glm <- predict(fit_glm, test_set, type = "response")

#  Set values to 1 (violent) if > 0.15 and otherwise 0 (non-violent)
y_hat_glm <- ifelse(p_hat_glm > 0.15, 1, 0) 

#   Set factors to the same factor level
y_hat_glm<-y_hat_glm %>%factor()
test_set$Category_Violent_dummy<-test_set$Category_Violent_dummy %>%factor()

#   Check the factor levels
nlevels(y_hat_glm )
nlevels(test_set$Category_Violent_dummy)

#   Show accuracy of the model
accuracy_1<- confusionMatrix(y_hat_glm, test_set$Category_Violent_dummy)$overall["Accuracy"]
accuracy_results <- data_frame(Model = "Logistic regression with one predictor", Accuracy = accuracy_1)

#   Print results
accuracy_results %>% knitr::kable()

#   #   #   #   #   #   #   #   #   #   #   #   #   #    #  
#    Logistic regression with more than one predictor    #  
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 

#   Fit the model
fit_glm_mp <- glm(Category_Violent_dummy ~   Resolution_dummy + Time_Hour + Date_Day + Date_Month + X + Y, family = "binomial",
                  data = train_set)

summary(fit_glm_mp)

#   Predict "Category_Violent_dummy"
p_hat_glm_mp <- predict(fit_glm_mp, test_set, type = "response")

#  Set values to 1 (violent) if > 0.15 and otherwise 0 (non-violent)
y_hat_glm_mp <- ifelse(p_hat_glm_mp > 0.15, 1, 0) 

#   Set factors to the same factor level
y_hat_glm_mp <- y_hat_glm_mp %>% factor()
test_set$Category_Violent_dummy <- test_set$Category_Violent_dummy %>% factor()

#   Check the factor levels
nlevels(y_hat_glm_mp )
nlevels(test_set$Category_Violent_dummy)

#   Show byClass to check the sensitivity, specificity 
confusionMatrix(y_hat_glm_mp, test_set$Category_Violent_dummy)$byClass %>% knitr::kable()

#   Show accuracy of the model
accuracy_2<- confusionMatrix(y_hat_glm_mp, test_set$Category_Violent_dummy)$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Logistic regression with more than one predictor", Accuracy = accuracy_2))
#   Print results
accuracy_results %>% knitr::kable()

#   Because specificity and sensitivity are rates, it is more appropriate to compute the harmonic average.
#   In fact, the F1-score, a widely used one-number summary, is the harmonic average of precision and recall p. 509
F_meas(y_hat_glm_mp, test_set$Category_Violent_dummy)%>% knitr::kable()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Final Logistic regression with more than one predictor
#   on Validation (test) and sf_crime_p
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

fit_glm_mp_v <- glm(Category_Violent_dummy ~ Resolution_dummy + Time_Hour + Date_Day + Date_Month + X + Y, family = "binomial",
                  data=sf_crime_p)

summary(fit_glm_mp_v)

#   Predict "Category_Violent_dummy"
p_hat_glm_mp_v <- predict(fit_glm_mp_v, validation, type = "response")

#  Set values to 1 (violent) if > 0.15 and otherwise 0 (non-violent)
y_hat_glm_mp_v <- ifelse(p_hat_glm_mp_v > 0.15, 1, 0) 

#   Set factors to the same factor level
y_hat_glm_mp_v <- y_hat_glm_mp_v %>% factor()
validation$Category_Violent_dummy <- validation$Category_Violent_dummy %>% factor()

#   Check the factor levels
nlevels(y_hat_glm_mp_v )
nlevels(validation$Category_Violent_dummy)

#   Show byClass to check the sensitivity, specificity 
confusionMatrix(y_hat_glm_mp_v, validation$Category_Violent_dummy)$byClass %>% knitr::kable()

#   Show accuracy of the model
accuracy_final<- confusionMatrix(y_hat_glm_mp_v, validation$Category_Violent_dummy)$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Final Logistic regression with more than one predictor", Accuracy = accuracy_final))
#   Print results
accuracy_results %>% knitr::kable()




#   #   #   #   #   #   #   #   #   #   #
#    Principal component Analyses (PCA) #  
#  #  #  #  #  #  #  #  #  #  #  #  #   # 

#set.seed(102)
#test_index <- createDataPartition(y = sf_crime$Category_Violent_dummy, times = 1, p = 0.2, list = FALSE)
#test_set<- sf_crime[test_index, ]
#train_set <- sf_crime[-test_index, ]  



#  PCA works best with numerical data, categorical variables were excluded.
#  You are left with a matrix of 6 columns and 8631 rows

train_set_pca <- train_set[ , c( "Resolution_dummy",
                                "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y") ]

test_set_pca <- test_set[ , c(  "Resolution_dummy", 
                               "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y") ]

#  This data is passed to the prcomp() function, assigning your output to sf_crime.pca


#  two arguments, center = TRUE and scale = TRUE. That means the columns are centered. scale only makes sense if variables are measured on the same scale
#sf_crime.pca <- prcomp(train_set_pca, center = TRUE, scale = TRUE)
sf_crime.pca <- prcomp(train_set_pca, center = TRUE)


#   The center and scale components correspond to the means and standard deviations of the variables
#   Mean value
sf_crime.pca$center


# 6 principal components were obtained. Each of these explains a percentage of the total variation in the dataset. That is to say: PC1 explains 21% of the total variance,
#  PC2 explains 17% of the variance. So, by knowing the position of a sample in relation to just PC1 and PC2, you can get a very accurate view on where it stands in relation to other samples, as just PC1, PC2 and PC3 can explain 55%
#  of the variance.

#   Summary of the output
summary(sf_crime.pca)
summary(sf_crime.pca)$importance[,1:7]


#   PCA returns 3 parameters
#   PC components
sf_crime.pca$x
head(sf_crime.pca$x)

#  Standard Deviation of the Components
sf_crime.pca$sdev

#   Rotation parameter. The rotation matrix provides the principal component loadings;
#   each column of sf_crime.pca$rotation contains the corresponding principal component 
sf_crime.pca$rotation
head(sf_crime.pca$rotation)

#   Calculate the Variation
sf_crime.pca.var <- sf_crime.pca$sdev^2


#   Percentage of Variation
sf_crime.pca.var.per <- round(sf_crime.pca.var/sum(sf_crime.pca.var) * 100, 1)

sf_crime.pca.rotation <- sf_crime.pca$rotation[ , 1]
abs(sf_crime.pca.rotation) 
sort(abs(sf_crime.pca.rotation) , decreasing = TRUE)

#  Explore the variance of the PCs
#  This turning point in the level of explained variance indicates the number of principal components present in the data.
plot(sf_crime.pca, type="lines")
#  Visualization of the explained Variable
fviz_eig(sf_crime.pca, addlabels = TRUE)


#  Plot PC1 and PC2 to have more information
data.frame(PC1 = sf_crime.pca$x[,1], PC2 = sf_crime.pca$x[,2], label=factor(train_set$Category_Violent_dummy)) %>%
  sample_n(2000) %>% ggplot(aes(PC1, PC2, fill=label))+ geom_point(cex=3, pch=21) 

# Preparation for model fitting: calculate means of Columns 
col_means<- colMeans(test_set_pca)

#  Set x_train equal to PC's
x_train <- sf_crime.pca$x

y <- factor(train_set$Category_Violent_dummy)

#  Fit knn with k = 5 model
fit <- knn3(x_train, y)

#  Transform the test_set
x_test <- as.matrix(sweep(test_set_pca, 2, col_means)) %*% sf_crime.pca$rotation

#  Predict Category 
y_hat <- predict(fit, x_test, type = "class")

# Print Accuracy of the model
confusionMatrix(y_hat, factor(test_set$Category_Violent_dummy))$overall["Accuracy"] %>% knitr::kable("pipe")


#   #   #   #   #   #   #   #   #   #   #  #   #   #   #   #   #
#    Principal component Analyses (PCA) on Validation Data set #  
#  #  #  #  #  #  #  #  #  #  #  #  #   #   #   #   #   #   #  #

#  Keep only numerical variables in sf_crime_p_pca data set
sf_crime_p_pca <- sf_crime_p[ , c( "Resolution_dummy",
                                   "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y") ]

#  Keep only numerical variables in validation_pca data set
validation_pca <- validation[ , c( "Resolution_dummy",
                                   "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y") ]

#  Perform PCA 
sf_crime.pca <- prcomp(sf_crime_p_pca, center = TRUE)

# Preparation for model fitting: calculate means of columns 
col_means<- colMeans(validation_pca)

#  Set x_train equal to PC's
x_train <- sf_crime.pca$x

y <- factor(sf_crime_p$Category_Violent_dummy)

#  Fit knn with k = 5 model
fit <- knn3(x_train, y)

#  Transform the test_set
x_test <- as.matrix(sweep(validation_pca, 2, col_means)) %*% sf_crime.pca$rotation

#  Predict Category 
y_hat <- predict(fit, x_test, type = "class")

# Print Accuracy of the model on Validation data
confusionMatrix(y_hat, factor(validation$Category_Violent_dummy))$overall["Accuracy"] %>% knitr::kable("pipe")










#######################################################################
#  
#  PCA works best with numerical data, categorical variables were excluded.
#  You are left with a matrix of 6 columns and 8631 rows
set.seed(202)
test_index <- createDataPartition(y = sf_crime$Category, times = 1, p = 0.2, list = FALSE)
test_set<- sf_crime[test_index, ]
train_set <- sf_crime[-test_index, ]  

train_set_pca <- train_set[ , c("Category_Violent_dummy", "Resolution_dummy",
                                "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y") ]

test_set_pca <- test_set[ , c("Category_Violent_dummy", "Resolution_dummy",
                              "Count_Address_Occur", "Time_Hour","Date_Day", "Date_Month", "X", "Y") ]

#  This data is passed to the prcomp() function, assigning your output to sf_crime.pca


#  two arguments, center = TRUE and scale = TRUE. That means the columns are centered. scale only makes sense if variables are measured on the same scale
#sf_crime.pca <- prcomp(train_set_pca, center = TRUE, scale = TRUE)
sf_crime.pca <- prcomp(train_set_pca,  center = TRUE)

# Get eigenvalue
eig.val <- get_eigenvalue(sf_crime.pca)
eig.val

#   The center and scale components correspond to the means and standard deviations of the variables
#   Mean value
sf_crime.pca$center


# 6 principal components were obtained. Each of these explains a percentage of the total variation in the dataset. That is to say: PC1 explains 21% of the total variance,
#  PC2 explains 17% of the variance. So, by knowing the position of a sample in relation to just PC1 and PC2, you can get a very accurate view on where it stands in relation to other samples, as just PC1, PC2 and PC3 can explain 55%
#  of the variance.

#   Summary of the output
summary(sf_crime.pca)
summary(sf_crime.pca)$importance[,1:8]


#   PCA returns 3 parameters
#   PC components
sf_crime.pca$x
head(sf_crime.pca$x)

#  Standard Deviation of the Components
sf_crime.pca$sdev

#   Rotation parameter. The rotation matrix provides the principal component loadings;
#   each column of sf_crime.pca$rotation contains the corresponding principal component 
sf_crime.pca$rotation
head(sf_crime.pca$rotation)

#   Calculate the Variation
sf_crime.pca.var <- sf_crime.pca$sdev^2


#   Percentage of Variation
sf_crime.pca.var.per <- round(sf_crime.pca.var/sum(sf_crime.pca.var) * 100, 1)

sf_crime.pca.rotation <- sf_crime.pca$rotation[ , 1]
abs(sf_crime.pca.rotation) 
sort(abs(sf_crime.pca.rotation) , decreasing = TRUE)

#  Explore the variance of the PCs
#  This turning point in the level of explained variance indicates the number of principal components present in the data.

plot(sf_crime.pca, type="lines")

#  Visualization of the explained Variable
fviz_eig(sf_crime.pca, addlabels = TRUE)

#Extract Results
names_pca<-names((summary(sf_crime.pca)$importance[3, ]))
values<-data.frame(x=summary(sf_crime.pca)$importance[3, ], y=names(summary(sf_crime.pca)$importance[3, ]))%>%top_n(10) 

plot(summary(sf_crime.pca)$importance[3,])


boxplot(sf_crime.pca$x[,2] ~ train_set$Category[, 1:10], main = paste("PC", 2), axis.text.x = element_text(angle = 180))





#The following plot shows which observations are close to each other. The plot depicts two Principal Components PC1 and PC2 with color representing Category.
#We can see category cluster
#  Plot PC1 and PC2 to have more information
data.frame(PC1 = sf_crime.pca$x[,1], PC2 = sf_crime.pca$x[,2], label=factor(train_set$Category)) %>%
  sample_n(10000) %>% ggplot(aes(PC1, PC2, fill=label))+ geom_point(cex=3, pch=21) 

# Preparation for model fitting: calculate means of Columns 
col_means<- colMeans(test_set_pca)

#  Set x_train equal to PC's
x_train <- sf_crime.pca$x

y <- factor(train_set$Category)

#  Fit knn with k = 5 model
fit <- knn3(x_train, y)

#  Transform the test_set
x_test <- as.matrix(sweep(test_set_pca, 2, col_means)) %*% sf_crime.pca$rotation

#  Predict Category 
y_hat <- predict(fit, x_test, type = "class")


confusionMatrix(y_hat, factor(test_set$Category))$overall["Accuracy"] %>% knitr::kable("pipe")

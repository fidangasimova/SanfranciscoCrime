# # # # # # # # # # # # # # # # # # # # # # # # 
#  PROJECT II: San Francisco Crime            #
# # # # # # # # # # # # # # # # # # # # # # # # 

setwd("SanfranciscoCrime")

# # # # # # # # # # # # # # # # 
#  Install and Load libraries
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

sf_crime <- read.csv("sf_crime.csv", header=TRUE, sep = ",", row.names = NULL, quote = "\"")

# Versions without spaces
names(sf_crime) <- make.names(names(sf_crime), unique=TRUE)

#  Change the format from scientific to numerical
options(scipen = 999)

sf_crime<-as.data.frame(sf_crime, stringsAsFactors=TRUE) %>% mutate(IncidntNum = as.numeric(IncidntNum), 
                                                                    Category = as.character(Category),
                                                                    Descript = as.character(Descript),
                                                                    Resolution = as.character(Resolution),
                                                                    PdDistrict = as.character(PdDistrict))




# # # # # # # # # # # # # # # # # #
#   Data Description and Cleaning # 
# # # # # # # # # # # # # # # # # #

#  Describe sf_crime data set
describe(sf_crime)

str(sf_crime)

#  Check missing values
sum(is.na(sf_crime))

#  Show first 10 rows
#  head(sf_crime,10)%>%knitr::kable("simple")
print(head(sf_crime,10))

#  Summary of the data set
summary(sf_crime)

#  Number of variables
print(ncol(sf_crime))

#  Number of observations
print(nrow(sf_crime))

#  Convert year character into class Date
sf_crime <- transform(sf_crime, Date_time = as.Date(Date, "%m/%d/%Y", tz = "UTC"))

sf_crime <- transform(sf_crime, Date_time = as.Date(Date))
#  Create variables Date_month, Date_day, Date_year
sf_crime<-sf_crime %>% mutate(Date_month = month(Date_time), Date_day = day(Date_time), Date_year = year(Date_time))

sf_crime<-arrange(sf_crime, by=IncidntNum)
lapply(sf_crime, summary)

#  Remove columns after data cleaning
within(sf_crime, rm(Date))

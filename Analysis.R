# # # # # # # # # # # # # # # # # # # # # # # # 
#  PROJECT II: San Francisco Crime            #
# # # # # # # # # # # # # # # # # # # # # # # # 

setwd("SanfranciscoCrime")

# # # # # # # # # # # # # # # # 
#  Install and Load libraries
# # # # # # # # # # # # # # # #

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
sf_crime<-as.data.frame(sf_crime, stringsAsFactors=TRUE) %>% mutate(Id = as.numeric(IncidntNum), 
                                                                    Category = as.character(Category),
                                                                    Descript = as.character(Descript),
                                                                    Resolution = as.character(Resolution),
                                                                    PdDistrict = as.character(PdDistrict))

# # # # # # # # # # # # # # # # # 
#   Data Cleaning               # 
# # # # # # # # # # # # # # # # # 

#  Check missing values
sum(is.na(sf_crime))

# Show first 10 rows
head(sf_crime, 10)

# Summary of the data set
summary(sf_crime)

#  Number of variables
print(ncol(sf_crime))

# Number of observations
print(nrow(sf_crime))





# some of our column names have spaces in them. This line changes the column names to 
# versions without spaces, which let's us talk about the columns by their names.
names(chocolateData) <- make.names(names(chocolateData), unique=TRUE)


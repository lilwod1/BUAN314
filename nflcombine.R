
library(sqldf)
library(ggplot2)
library(tidyverse)

nfl_combine <- read.csv("nfl_combine_2010_to_2023.csv", header = TRUE)

#######################
#####DATA CLEANING#####
#######################
#create a data frame without ANY null values in any column (ONLY players who were drafted)
nfl_A<-na.omit(nfl_combine)
#create a data frame with ONLY players who were NOT drafted
nfl_B<-sqldf("SELECT * 
             FROM nfl_combine
             WHERE Drafted = 'False'")
#delete Round & Pick columns from nfl_A and nfl_B
nfl_A$Round <- NULL
nfl_A$Pick <- NULL
nfl_B$Round <- NULL
nfl_B$Pick <- NULL
#remove null values
nfl_A<-na.omit(nfl_A)
nfl_B<-na.omit(nfl_B)
#convert height to inches for a numeric variable
nfl_A <- nfl_A %>%
  separate(Height, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
  mutate(height_total_in = feet*12 + inches)

nfl_B <- nfl_B %>%
  separate(Height, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
  mutate(height_total_in = feet*12 + inches)

#correlation matrix for both nfl_A and nfl_B
cor_A <- cor(nfl_A[,c(7,8,9,10,11,12,13,15)]) 
pairs(nfl_A[,c(7,8,9,10,11,12,13,15)])

cor_B <- cor(nfl_B[,c(7,8,9,10,11,12,13,15)]) 
pairs(nfl_B[,c(7,8,9,10,11,12,13,15)])

#join nfl_A and nfl_B to create a clean data frame with both drafted and undrafted players
nfl_C <- sqldf('
      SELECT *
      FROM nfl_A
      UNION 
      SELECT *
      FROM nfl_B
      ')

#change the Drafted column-- instead of using TRUE/FALSE, 0 and 1
nfl_C$Drafted[nfl_C$Drafted=='True']<-1
nfl_C$Drafted[nfl_C$Drafted=='False']<-0

##############################
#####DESCRIPTIVE ANALYSIS#####
##############################
#create scatter plots comparing player metrics
ggplot(nfl_C, aes(x=Weight, y=X40yd,color=Drafted))+
  geom_jitter()+
  geom_smooth(aes())+
  facet_wrap(vars(Pos))

ggplot(nfl_C, aes(x=height_total_in, y=Vertical,color=Drafted))+
  geom_jitter()+
  geom_smooth()

ggplot(nfl_C, aes(x=Weight, y=Broad.Jump,color=Drafted))+
  geom_jitter()+
  geom_smooth()

#create boxplots comparing 40yd times of players who were drafted or not
boxplot(nfl_A$X40yd, #only players who were drafted
        main="Average Drafted 40yd Dash",
        xlab="Time",
        ylab="",
        col="purple",
        horizontal = TRUE
)
summary(nfl_A$X40yd)

boxplot(nfl_B$X40yd, #only players who were drafted
        main="Average Undrafted 40yd Dash",
        xlab="Time",
        ylab="",
        col="purple",
        horizontal = TRUE
)
summary(nfl_B$X40yd)

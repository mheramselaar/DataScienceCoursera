## Coursera Clean Data Week 4 Assignment

## WD should be set to wherever you would like to be saving this data. I have a
## folder for this course, which I will add a data folder to. If the directory
## does not already exist, this will create it.

currdir <- "./data"
if(!dir.exists("./data")) dir.create("./data")
setwd(currdir)


## load the packages you expect you will need.

library(dplyr)
library(tidyr)
library(R.utils)
library(data.table)
library(plyr)

## Download in the file
downloadurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI HAR Dataset.zip"
download.file(downloadurl, zipfile)

## Unzip the file, check if it exists first
if(file.exists(zipfile)) unzip(zipfile)

## Read the data you've just downloaded
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  

## Let's start making tidy data!

## Merge the training and the test sets to create one data set.
dataSet <- rbind(X_train,X_test)

## Extract only the measurements on the mean and standard deviation for each 
## measurement. Firstly, create a vector of only mean and std, use the vector 
## to subset.
MeanStdOnly <- grep("mean()|std()", features[, 2]) 
dataSet <- dataSet[,MeanStdOnly]

## Appropriately label the data set with descriptive activity names.
## Create vector of "Clean" feature names by getting rid of "()" apply to the 
## dataSet to rename labels.
CleanFeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(dataSet) <- CleanFeatureNames[MeanStdOnly]

## combine test and train of subject data and activity data, give the resulting 
## combination descriptive labels
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'

## combine subject, activity, and mean and std only data set to create final 
## data set.
dataSet <- cbind(subject,activity, dataSet)

## Use descriptive activity names to name the activities in the data set
## group the activity columns of dataSet, re-name label of levels with 
## activity_levels, and apply it to dataSet.
act_group <- factor(dataSet$activity)
levels(act_group) <- activity_labels[,2]
dataSet$activity <- act_group

## Create a second, independent tidy data set with the average of each variable 
## for each activity and each subject. 

## check if reshape2 package is installed, you do need this
if (!"reshape2" %in% installed.packages()) {
  install.packages("reshape2")
}
library("reshape2")

## melt data to tall skinny data and cast means. Finally write the tidy data 
## to the working directory as "tidy_data.txt"
baseData <- melt(dataSet,(id.vars=c("subject","activity")))
secondDataSet <- dcast(baseData, subject + activity ~ variable, mean)
names(secondDataSet)[-c(1:2)] <- paste("[mean of]" , names(secondDataSet)[-c(1:2)] )
write.table(secondDataSet, "tidy_data.txt", sep = ",")

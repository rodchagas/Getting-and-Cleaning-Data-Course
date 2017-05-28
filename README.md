---
title: "Getting and Cleaning Data Course Project"
output: github_document
---
## Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Dataset information

The dataset includes the following files:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

There was created a folder into the working directory called UCI HAR Dataset("./UCI HAR Dataset/") to mantain the files.

## GUIDE

### Initial Steps

#### Load required packages
```
# Load required packages 
library(data.table)
library(dplyr)
library(reshape2)
```

#### Load required files
Files can be located in the folder./UCI HAR Dataset/. The following files shoudl be uploaded:

- test/subject_test.txt

- train/subject_train.txt

- test/X_test.txt

- train/X_train.txt

- test/y_test.txt

- train/y_train.txt

- features.txt 

- activity_labels.txt 

```
#Upload all files
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
```

### Request 1: Merges the training and the test sets to create one data set.
```
subjectData   <- rbind(subject_train, subject_test) 
collectedData <- rbind(X_train, X_test)
labelData     <- rbind(y_train, y_test)

## Set the label for all columns 
names(subjectData)    <- "subject_ID"
names(collectedData)  <- features$V2
names(labelData)      <- "activity_ID"

#rm(subject_train, subject_test, X_train, X_test, y_train, y_test) #optional: just to free memory
allData       <- cbind(subjectData, collectedData, labelData)
#rm(subjectData, collectedData, labelData, features) #optional: just to free memory
# allData data.frame contains now all information from test and train
```

### Request 2: Extracts only the measurements on the mean and standard deviation for each measurement.
As we had included the "subject_ID" and "activity_ID" we also need to consider than to filter

```
# find subject|mean|std|activity at the column labels of allData data.frame
filter_cols   <- grepl("subject|mean|std|activity", names(allData))
# create a new data.fram with only the columns that have in their labels the values #"subject|mean|std|activity"
mean_std_Data  <- allData[,filter_cols]
# mean_std_Data only contains now the columns with the above filter
```

### Request 3: Uses descriptive activity names to name the activities in the data set

```
mean_std_Data$activity_Name = activity_labels[mean_std_Data$activity_ID,2]
```

### Request 4: Appropriately labels the data set with descriptive variable names.

check features_info.txt for further information
replace/substitute the following values using gsub

```
names(mean_std_Data) <- gsub("^t", "Time", names(mean_std_Data)) #time domain signals (prefix 't' to denote time) 
names(mean_std_Data) <- gsub("^f", "Frequency", names(mean_std_Data)) #'f' to indicate frequency domain signals
names(mean_std_Data) <- gsub("Acc", "Accelerometer", names(mean_std_Data))
names(mean_std_Data) <- gsub("Gyro", "Gyroscope", names(mean_std_Data))
names(mean_std_Data) <- gsub("BodyBody", "Body", names(mean_std_Data))
names(mean_std_Data) <- gsub("Mag", "Magnitude", names(mean_std_Data))
names(mean_std_Data) <- gsub("-mean()", "Mean", names(mean_std_Data), ignore.case = TRUE)
names(mean_std_Data) <- gsub("-std()", "STD", names(mean_std_Data), ignore.case = TRUE)
```

### Request 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

- 1st we have to group the data by activity and each subject

- 2nd we will calculate the mean per group for all columns

- Save TidyData.TXT file using this info

```
#create a new data.frame called groupedData, grouped by activity for each subject
groupedData <- group_by(mean_std_Data, subject_ID, activity_ID, activity_Name)
#calculate the mean for all columns and save the data into the new data.frame tidyData
tidyData<- summarise_all(groupedData, mean)
#create a TidyData.TXT file using the tidyData data.frame
write.table(tidyData, "TidyData.txt", row.name=FALSE)
```



#You should create one R script called run_analysis.R that does the following.
#1.	Merges the training and the test sets to create one data set.
#2.	Extracts only the measurements on the mean and standard deviation for each measurement.
#3.	Uses descriptive activity names to name the activities in the data set
#4.	Appropriately labels the data set with descriptive variable names.
#5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#
# Load required packages 
#
library(data.table)
library(dplyr)
library(reshape2)

#
# 1. Load the data files
# The files can be located in the folder./UCI HAR Dataset/. The following files shoudl be uploaded:
# test/subject_test.txt
# train/subject_train.txt
# test/X_test.txt
# train/X_train.txt
# test/y_test.txt
# train/y_train.txt
# features.txt 
# activity_labels.txt 

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# Request 1: Merges the training and the test sets to create one data set.
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

# Request 2: Extracts only the measurements on the mean and standard deviation for each measurement.
# As mentioned into features_info.txt file mean = mean and standard deviation = std.
# As we had included the "subject_ID" and "activity_ID" we also need to consider than to filter
#filter_cols   <- grepl("subject|mean|std|activity", names(allData))
# find subject|mean|std|activity at the column labels of allData data.frame
filter_cols   <- grepl("subject|mean|std|activity", names(allData))
# create a new data.fram with only the columns that have in their labels the values #"subject|mean|std|activity"
mean_std_Data  <- allData[,filter_cols]
# mean_std_Data only contains now the columns with the above filter

# Request 3: Uses descriptive activity names to name the activities in the data set
mean_std_Data$activity_Name = activity_labels[mean_std_Data$activity_ID,2]
rm(activity_labels, allData, filter_cols) #optional: just to free memory

# Request 4: Appropriately labels the data set with descriptive variable names.
# check features_info.txt
#replace/substitute the following values using gsub
names(mean_std_Data) <- gsub("^t", "Time", names(mean_std_Data)) #time domain signals (prefix 't' to denote time) 
names(mean_std_Data) <- gsub("^f", "Frequency", names(mean_std_Data)) #'f' to indicate frequency domain signals
names(mean_std_Data) <- gsub("Acc", "Accelerometer", names(mean_std_Data))
names(mean_std_Data) <- gsub("Gyro", "Gyroscope", names(mean_std_Data))
names(mean_std_Data) <- gsub("BodyBody", "Body", names(mean_std_Data))
names(mean_std_Data) <- gsub("Mag", "Magnitude", names(mean_std_Data))
names(mean_std_Data) <- gsub("-mean()", "Mean", names(mean_std_Data), ignore.case = TRUE)
names(mean_std_Data) <- gsub("-std()", "STD", names(mean_std_Data), ignore.case = TRUE)

# Request 5: From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.
groupedData <- group_by(mean_std_Data, subject_ID, activity_ID, activity_Name)
tidyData<- summarise_all(groupedData, mean)
write.table(tidyData, "TidyData.txt", row.name=FALSE)

      

# Author name: Juana Canul-Reich
# this script assumes the current working directory is ./UCI HAR Dataset 
# which holds two folders:
#folder train
#    with files X_train.txt, y_train.txt, subject_train.txt
#folder test
#    X_test.txt, y_test.txt, subject_test.txt     
# along with files: activity_labels.txt, features.txt, 

# reading files corresponding to training data
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

# reading files corresponding to test data
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

#reading file containing every single variable name: features.txt
features <- read.table("./features.txt", stringsAsFactors=FALSE)

#reading file containing activity name of each activity number: activity_labels.txt
activity_labels <- read.table("./activity_labels.txt", stringsAsFactors=FALSE)

#########################################################################
# 1. Merges the training and the test sets to create one data set.
# Putting data together
# X_train (7352 x 561) and X_test (2947 x 561) datasets are row-binded to become 
# one larger dataset (10299 x 561) variables
dataset <- rbind(X_train, X_test)

# subject train (7352 x 1) and subject test (2947 x 1) are row-binded to become
# one larger vector (10299 x 1)
subject <- rbind(subject_train, subject_test)

# Y_train (7352 x 1) and Y_test (2947 x 1) are row-binded to become 
# one larger vector (10299 x 1)
Y <- rbind(Y_train, Y_test)

# Now, we have the following data objects:
# dataset of size 10299 x 561, containing all measurements 
# subject of size 10299 x 1, indicating who performed the activity in number representation (1 to 30)
# Y       of size 10299 x 1, indicating the activity performed in number representation (1 to 6)

# Next step is to put all these data objects together into a dataset:
# to avoid conflicts on variable names in "dataset" (i.e. V1), the variable name in subject
# is changed from V1 to subject as follows
names(subject) <- "subject"

# the same is performed on dataset Y
names(Y) <- "activity"

# adding subject to dataset, size of object data is 10299 x 562
data <- cbind(dataset, subject)

# adding Y (activity) to data, size of new object data is 10299 x 563
data <- cbind(data, Y)

# Correct column names on "data" are those in features[,2]. 
# Getting right column names and retaining columns "subject" and "activity".
# Column names in features are listed in the same order as they appear in data
names(data) <- c(features[,2], "subject", "activity")
# so far, data contains train and test sets all merged,  
# correct column names, it is of size 10299 x 563 including subject and activity


#########################################################################
#3. Uses descriptive activity names to name the activities in the data set
#changing activity numbers into activity names
#creating a new column on data
data$activityname <- "unset"
#subsetting data to convert activity numbers into activity names. After change is
# made, result is assigned to new column just created
data$activityname[data$activity==1] <- "WALKING"
data$activityname[data$activity==2] <- "WALKING_UPSTAIRS"
data$activityname[data$activity==3] <- "WALKING_DOWNSTAIRS"
data$activityname[data$activity==4] <- "SITTING"
data$activityname[data$activity==5] <- "STANDING"
data$activityname[data$activity==6] <- "LAYING"

# removing column "activity" from data as column "activityname" has descriptive
# activity names not numbers 
data$activity = NULL 

#########################################################################
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
# PICKING the mean and standard deviations columns
# meanArray contains 46 values (46 column names related to a mean)
meanArray <- grep("mean", names(data), value=TRUE)
# stdArray contains 33 values (33 column names related to a std)
stdArray <- grep("std", names(data), value=TRUE)
# meanAndstd contains 79 column names total
meanAndstd <- c(meanArray, stdArray)

#subsetting data to contain only column names as specified by meanAndstd and
# including "subject" and "activityname"
#size of data is now 10299 x 81
data <- data[,c(meanAndstd, "subject", "activityname")]

#########################################################################
#4. Appropriately labels the data set with descriptive variable names.
#IMPROVING variable names
# removing "-" from column names
names(data) <- gsub("-", "", names(data))

# removing "()" from column names
names(data) <- gsub("[(|)]", "", names(data), perl=TRUE)

#########################################################################
#5. Creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
# First, "data" needs to get melted, later the new tidy data requested in point 5
# will be obtained by dcasting from this melted data. 
# To be able to use melt() function, library reshape2 needs to be loaded
# It enables functions melt and dcast
library(reshape2)
# melt() function will reshape our data object from 
# wide format into long format, so that there will be a row for each  
# other variable than the specified as id (subject + activity)
# melted datas "mdata" has 4 columns such as: 
# subject, activityname, variable, value
mdata <- melt(data, id=c("subject", "activityname"))

#next, we need to produce the new tidy data set as required
# function dcast() will bring back our data into a wide format
# as specified by formula (subject + activityname ~ variable)
# dcast will obtain the mean for each variable

# dcastdata is the new tidy data of size 180 * 81 (including two columns for subject
# and activity name plus 79 variables which show mean values)
dcastdata <- dcast(mdata, subject + activityname ~ variable, mean)

# writing dcastdata to a Tab Delimited Text File named tidydataset.txt
# it will be written to current working directory
# this is the file uploaded to Github
write.table(dcastdata, "./tidydataset.txt", sep="\t", row.names = FALSE)

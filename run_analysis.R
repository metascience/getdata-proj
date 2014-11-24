# run_analysis.R
# This file performs a series of tasks on the dataset "Human Activity
# Recognition Using Smartphones" from UCI. The tasks are aimed at producting
# a tidy dataset and then performing averages for activities and subects.
# 
# The steps are:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each
#    measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
#
# This script should be run from a directory where the subdirectory
# "UCI HAR Dataset" is alongside it. The dataset can be extracted from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Functions

# This function is used in Step 1 to get the dataset from the files and perform
# appropriate formatting, merging, and column naming.
# You can specify nrows to quickly test the function, otherwise all data is returned.
get_data_set <- function(nrows=-1) {
  # First get the 561 features, which identify the columns of the "X" data.
  features <- read.table("UCI HAR Dataset/features.txt")
  # To make neater table names, I will get rid of the parenthesis () in the
  # meaurement names, so that a variable like this: tBodyAcc-mean()-X
  # will produce a column name like this: tBodyAcc.mean.X
  featurenames <- gsub("()","",features[,2],fixed=TRUE)
  
  # Now get the observed data, and apply the 
  # Technically inserting the variable names here is part of Step 4,
  # however it makes it easier to handle the data throughout the process.
  train_set <- read.table("UCI HAR Dataset/train/X_train.txt",
                          nrows=nrows,
                          col.names=featurenames)
  test_set <- read.table("UCI HAR Dataset/test/X_test.txt",
                         nrows=nrows,
                         col.names=featurenames)
  
  # Get the subject who performed each observation and prepend as the first column.
  train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt",
                             nrows=nrows,
                             col.names="Subject")
  train_set <- cbind(train_subjects,train_set)
  test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt",
                            nrows=nrows,
                            col.names="Subject")
  test_set <- cbind(test_subjects,test_set)
  
  # Get the activity labels for each observation and prepend as the new first column.
  train_labels <- read.table("UCI HAR Dataset/train/y_train.txt",
                             nrows=nrows,
                             col.names="Activity.Label")
  train_set <- cbind(train_labels,train_set)
  test_labels <- read.table("UCI HAR Dataset/test/y_test.txt",
                            nrows=nrows,
                            col.names="Activity.Label")
  test_set <- cbind(test_labels,test_set)
  
  # Finally, merge and return all of the training observations and test
  #  observations as one large table by appending the rows. The training
  # set will come first.
  rbind(train_set,test_set)
}

# For Step 2, select only the measurements that are means or standard
# deviations of a signal. According to features_info.txt provided with
# the dataset, this includes columns where the signal name is followed
# by -mean() or -std(). We also keep the Activity.Label and subject
# columns.
select_mean_and_std <- function(data_set) {
  cols <- colnames(data_set)
  # Use the grepl function to detect the expected pattern in the name.
  # R translates the -, (, and ) as "." in the colnames.
  keep_cols <- grepl(".std.",cols,fixed=TRUE) | 
    grepl(".mean.",cols,fixed=TRUE) | 
    cols=="Activity.Label" | cols=="Subject"
  
  # Return just the selected columns
  data_set[,keep_cols]
}

# Step 3 - Get descriptive activity names from a file, and apply them
# to the dataset based on the numeric Activity Label that is already
# a part of the dataset. Example descriptions from the file are
# "WALKING" or "WALKING_UPSTAIRS"
add_activity_names <- function(data_set) {
  activity_labels = read.table("UCI HAR Dataset/activity_labels.txt",
                               col.names = c("Activity.Label", "Activity.Description"))
  data_set = merge(data_set, activity_labels, by="Activity.Label" )
}


## Execute the steps

# Step 1: Get and merge the data
data <-get_data_set()

# Step 2: Select Measurement Mean and Standard Deviation Columns
data <- select_mean_and_std(data)

# Step 3: Add descriptive Activity Names
data <-add_activity_names(data)

# Step 4: Add descriptive measurement names 
# This was alread done as part of step 1

# Step 5: Average each variable for each activity and subject


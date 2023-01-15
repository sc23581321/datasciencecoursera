library(tidyverse)

# download files
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = fileurl, destfile = "./data/getdata_projectdata.zip", method = "curl" )
datedownloaded <- date()

filelist <- unzip("./data/getdata_projectdata.zip", list = T)

unzip("./data/getdata_projectdata.zip", exdir = "./data")

# load the training datasets
trainX <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = F, quote = "", sep = "", dec = ".")
trainY <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = F, quote = "", sep = " ", dec = ".")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = F, quote = "", sep = " ", dec = ".")
trainX <- as_tibble(trainX)

# load the test datasets
testX <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = F, quote = "", sep = "", dec = ".")
testY <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = F, quote = "", sep = " ", dec = ".")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = F, quote = "", sep = " ", dec = ".")
testX <- as_tibble(testX)

# laod feature list
features <- read.table("./data/UCI HAR Dataset/features.txt", header = F, quote = "", sep = "", dec = ".")
colnames(features) <- c("fid", "feature")

# load activity list
activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = F, quote = "", sep = "", dec = ".")
colnames(activity) <- c("Label", "activity")

# change column names
colnames(subject_train)[1] <- "SubjectID"
colnames(subject_test)[1] <- "SubjectID"
colnames(trainY)[1] <- "Label"
colnames(testY)[1] <- "Label"

colnames(trainX) <- paste0("v_", features[,1]) # concate feature ID with string "v_" for readability
colnames(testX) <- paste0("v_", features[,1])  # same as above
trainX <- cbind(subject_train,trainY, trainX)  #add subject ID and activity label in the front
testX <- cbind(subject_test,testY, testX)  # add subject ID and activity label in the front

#####################
# Q1 Merges the training and the test sets to create one data set.
combined <- rbind(trainX, testX)
combined <- as_tibble(combined)

####################
## Q2 Extracts only the measurements on the mean and standard deviation for each measurement.

# filter the feature table to rows with mean and std
q2list <- features %>% filter(grepl('mean|std', feature))
q2list$vid <- paste0("v_", q2list$fid) # prepare the variable id

q2data <- combined %>% 
    select(SubjectID, Label, q2list$vid) %>% # select the subset columns
    as_tibble() 

###################
## Q3 Uses descriptive activity names to name the activities in the data set
q3data <- merge(q2data, activity, by = "Label") %>%   # add activity names
    select(SubjectID, Label, activity, starts_with("v_")) %>%  # reorder the columns
    as_tibble()

###################
## Q4 Appropriately labels the data set with descriptive variable names. 
q4data <- q3data
colnames(q4data)[4:82] <- q2list$feature[1:79]  # add descriptive variable names
q4data <- as_tibble(q4data)

###################
## Q5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
q5mean_data <- q4data %>% group_by(SubjectID, activity) %>% 
    summarise_at(vars("tBodyAcc-mean()-X":"fBodyBodyGyroJerkMag-meanFreq()"), mean, na.rm = T) %>% 
    as_tibble()
write.csv(q5mean_data, file = "./data/q5_mean_data_table.csv", row.names = F)





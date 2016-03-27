# 1. Merges the training and the test sets to create one data set.
# lets setwd:
#("C:/Users/Lenovo/Documents/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
# bring the 3 files in train folder X_train--y_train--subject_train
trainData <- read.table("./train/X_train.txt")
trainLabel <- read.table("./train/y_train.txt")
trainSubject <- read.table("./train/subject_train.txt")
# bring the 3 files in test folder X_test--y_test--subject_test
testData <- read.table("./test/X_test.txt")
testLabel <- read.table("./test/y_test.txt") 
testSubject <- read.table("./test/subject_test.txt")
# lets merge the data in one variable by pairs (X_train&X_test)-(y_train&y_test)&(subject_train&subject_test)
MData <- rbind(trainData, testData)
MLabel <- rbind(trainLabel, testLabel)
MSubject <- rbind(trainSubject, testSubject)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Read the features file
features <- read.table("./features.txt")
#get the mean and standard deviation indices
meanStdInd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
#merge it with MData
MData <- MData[, meanStdInd]
# Clean and handle data and names
names(MData) <- gsub("\\(\\)", "", features[meanStdInd, 2]) # remove "()"
names(MData) <- gsub("mean", "Mean", names(MData)) # m to M
names(MData) <- gsub("std", "Std", names(MData)) # s to S
names(MData) <- gsub("-", "", names(MData)) # remove "-" in column names 

# 3. Uses descriptive activity names to name the activities in the data set

# Read the activity_labels file
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
# get the values that are going to be used
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[MLabel[, 1], 2]
# rename the values, in this case change the numbers to one out of the 5 activity_labels
MLabel[, 1] <- activityLabel
names(MLabel) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names.

names(MSubject) <- "subject"
cleanedData <- cbind(MSubject, MLabel, MData)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

subLen <- length(table(MSubject))
actLen <- dim(activity)[1]
colLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subLen*actLen, ncol=colLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subLen) {
  for(j in 1:actLen) {
    result[row, 1] <- sort(unique(MSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:colLen] <- colMeans(cleanedData[bool1&bool2, 3:colLen])
    row <- row + 1
  }
}

write.table(result, "data_step5.txt",row.name=FALSE) # create the txt file to upload in the submission





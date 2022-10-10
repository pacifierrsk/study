library(dplyr)

# Reading the list of features and activities

feature_list <- read.table(file = "./UCIHARDataset/features.txt", col.names = c("s_no", "features"))
activity_list <- read.table(file ="./UCIHARDataset/activity_labels.txt", col.names = c("labels", "activity") )

subject_train <- read.table("./UCIHARDataset/train/subject_train.txt", col.names = "subject")
subject_test <- read.table("./UCIHARDataset/test/subject_test.txt", col.names = "subject")
X_test <- read.table("./UCIHARDataset/test/X_test.txt", col.names = feature_list$features)
y_test <- read.table("./UCIHARDataset/test/y_test.txt", col.names = "yLabels")
X_train <- read.table("./UCIHARDataset/train/X_train.txt", col.names = feature_list$features)
y_train <- read.table("./UCIHARDataset/train/y_train.txt", col.names = "yLabels")

# Merges the training and the test sets to create one data set

X_final <- rbind(X_train, X_test)
y_final <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

# merging all

final_data <- cbind(subject, y_final, X_final)


# Extracts only the measurements on the mean and standard deviation for each measurement.

cleaned_Data <- select(final_data,subject, yLabels, contains("mean"), contains("std"))
cleaned_Data$yLabels <- activity_list[cleaned_Data$yLabels, 2]


# Appropriately labels the data set with descriptive variable names
names(cleaned_Data)[2] = "Activity"

names(cleaned_Data)<-gsub("Acc", "accelerometer", names(cleaned_Data))
names(cleaned_Data)<-gsub("Mag", "Magnitude", names(cleaned_Data))
names(cleaned_Data)<-gsub("-std()", "Standerd Deviation", names(cleaned_Data), ignore.case = TRUE)
names(cleaned_Data)<-gsub("-freq()", "Frequency", names(cleaned_Data), ignore.case = TRUE)
names(cleaned_Data)<-gsub("angle", "Angle", names(cleaned_Data))
names(cleaned_Data)<-gsub("^t", "Time", names(cleaned_Data))
names(cleaned_Data)<-gsub("^f", "Frequency", names(cleaned_Data))
names(cleaned_Data)<-gsub("tBody", "TimeBody", names(cleaned_Data))
names(cleaned_Data)<-gsub("Gyro", "Gyroscope", names(cleaned_Data))
names(cleaned_Data)<-gsub("BodyBody", "Body", names(cleaned_Data))
names(cleaned_Data)<-gsub("-mean()", "Mean", names(cleaned_Data), ignore.case = TRUE)


# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

tidyData <- cleaned_Data %>%
        group_by(subject, Activity) %>%
        summarise_all(funs(mean))
write.table(tidyData, "tidyData.txt", row.name=FALSE)

summarise(tidyData)
x <- str(tidyData)

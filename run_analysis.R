## Coursera Getting and Cleaning Data Course Project
## Eliza 
## August 20, 2015

# This code works to create the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# 1. Merge the training and the test sets to create one data set.

# Set working directory to the location where the UCI HAR Dataset was unzipped
# Also make sure that all necessary packages are installed 
setwd('/Users/Eliza/datasciencecoursera/getting_and_cleaning_data/UCI HAR Dataset/')

# Import the data from the text files in the UCI database
features     <- read.table('./features.txt',header=FALSE)
activity_labels <- read.table('./activity_labels.txt',header=FALSE)
subject_train <- read.table('./train/subject_train.txt',header=FALSE)
x_train       <- read.table('./train/x_train.txt',header=FALSE)
y_train       <- read.table('./train/y_train.txt',header=FALSE)
subject_test <- read.table('./test/subject_test.txt',header=FALSE) 
x_test       <- read.table('./test/x_test.txt',header=FALSE)
y_test       <- read.table('./test/y_test.txt',header=FALSE) 

# Assigin all the column names. A full description of the data is available at the site where the data was obtained: 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones, names chosen accordingly
colnames(activity_labels)  <- c('activity_id','activity_label')
colnames(subject_train)  <- "subject_id"
colnames(x_train)        <- features[,2] 
colnames(y_train)        <- "activity_id"
colnames(subject_test) <- "subject_id"
colnames(x_test)       <- features[,2] 
colnames(y_test)       <- "activity_id"

# Merge the three types of training and testing datasets, then merge those together into one dataset. 
training_data <- cbind(cbind(y_train,subject_train),x_train)
test_data <- cbind(cbind(y_test,subject_test),x_test)
final_data <- rbind(training_data,test_data)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

mean_std_features <- grep("-(mean|std)\\(\\)", features[,2])

# Subset the dataset by the desired columns, including subject, activity, means and stdevs
mean_std_data <- final_data[, mean_std_features]
names(mean_std_data) <- features[mean_std_features, 2]
subject_activity <- final_data[,1:2]
final_data <- cbind(subject_activity,mean_std_data)

# 3. Use descriptive activity names to name the activities in the data set

# Merge the dataset with the correct labels
final_data <- merge(final_data,activity_labels,by='activity_id',all.x=TRUE)

# 4. Appropriately label the data set with descriptive activity names. 

# Create a names vector in order to more appropriately name them
data_names <- names(final_data)

# Tidy up the data names
data_names <- gsub("Mag","Magnitude",data_names)
data_names <- gsub("-mean","Mean",data_names)
data_names <- gsub("-std","StDev",data_names)
data_names <- gsub("Acc","Accelerometer",data_names)
data_names <- gsub("Gyro","Gyroscope",data_names)
data_names <- gsub("BodyBody","Body",data_names)

# rename the dataset columns
names(final_data) <- data_names

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Remove the labels 
finalDataNoLabel <- final_data[,names(final_data) != 'activity_labels']

# Create tidy data of the average of each variable for each activity and each subject
tidy_data <- aggregate(finalDataNoLabel[,names(finalDataNoLabel) != c('activity_id','subject_id')],by=list(activity_id=finalDataNoLabel$activity_id,subject_id = finalDataNoLabel$subject_id),mean)

# Reinclude the description of the activity name from the labels
tidy_data <- merge(tidy_data,activity_labels,by='activity_id',all.x=TRUE)
tidy_data$activity_label.x <- NULL

# Export the tidy data into a txt file in the directory
write.table(tidy_data, './tidy.txt',row.name=FALSE,sep='\t')

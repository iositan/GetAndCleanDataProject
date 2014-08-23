library(downloader)
rm(list = ls())

# 1.Meges the training and the test sets to create one data set ----
# 1.a) Download data files ----
file_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
dest_Url <- "UCI_HAR_Dataset.zip"
download(file_Url,dest_Url, mode="wb")
# unzip 
unzip(dest_Url)


# 1.b) Read data ----
data_folder <- "./UCI HAR Dataset"
#train_data_folder <- paste(data_folder,"train",sep="/")
#test_data_folder <- paste(data_folder,"test", sep="/")

# helper function
read_file <- function(which_folder, var, what) {
        a <-paste(which_folder, what, paste(var,"_", what,".txt",sep=""),sep="/")
        print(sprintf("reading file %s", a))
        X <- read.table(a)
} 

# X data
train_x       <- read_file(data_folder,"X","train")
test_x        <- read_file(data_folder,"X","test")
# y data
train_y       <- read_file(data_folder,"y","train")
test_y        <- read_file(data_folder,"y", "test")



# 1.c) Merges the training and the test sets to create one data set. ----
merged_x <- rbind(train_x, test_x)
rm(train_x, test_x)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 2.a) Attach names to columns
features       <-read.table(paste(data_folder,"features.txt",sep="/"))
colnames(merged_x) <- c(as.character(features[,2]))
# 2.b) Create the filters
mean_std_filter <- grepl("mean|std",colnames(merged_x))
# 2.c)
filtered_X <- merged_x[,mean_std_filter]


# 3. Uses descriptive activity names to name the activities in the data set. ----
merged_y <- rbind(train_y,test_y)
rm(train_y, test_y)
colnames(merged_y) <- c("Activity")

merged_ds <- cbind(filtered_X , merged_y)

activity_labels<-read.table(paste(data_folder,"activity_labels.txt", sep="/"),stringsAsFactors = FALSE)
# helper function
replaceMe <- function(x) {
        for ( i in 1:nrow(activity_labels)) {
                x[x == i] <- activity_labels[i,2]
        }
        x
}
merged_ds$"Activity" <- replaceMe(merged_ds$"Activity")

# 4. Appropriately labels the data set with descriptive activity names. -----
# read subject also (see point 5)
subject_train <- read_file(data_folder,"subject", "train")
subject_test  <- read_file(data_folder,"subject","test")

subject_all<-rbind(subject_train,subject_test)
names(subject_all) <- "Subject"
rm(subject_train,subject_test)
complete_ds<-cbind(subject_all,merged_ds)


# 5. Creates a second, independent tidy data set with the average of 
#   each variable for 
#   each activity and 
#   each subject.
library(doBy)
listOfVariableColumns <- names(complete_ds)[!(names(complete_ds) %in% c("Subject","Activity"))]
new_complete_ds <- complete_ds
# factorize
new_complete_ds$Activity <- factor(new_complete_ds$Activity)
new_complete_ds$Subject <- factor(new_complete_ds$Subject)
# create a list of names for columns
# store the vector with the column number for selected columns above
storedColNum <- which((names(new_complete_ds) %in% listOfVariableColumns))
coln <- paste("v",storedColNum,sep="")
# replace the name of the columns

replaceColName <- function(df, storedColNum , coln) {
        for ( i in 1:length(storedColNum)) {
                names(df)[storedColNum[i]] <- coln[i]
        }
        df
}
new_complete_ds <- replaceColName(new_complete_ds,storedColNum , coln)
tidyData <- summaryBy( . ~ Subject+Activity, data = new_complete_ds, FUN=c(mean))

colnames(tidyData)[3:ncol(tidyData)] <- paste("mean of ",listOfVariableColumns)

write.table(tidyData, file = "Tidy Data Set.txt", row.name=FALSE)

# Load the packages needed to do the analysis ane create the
# clean dataset

library(plyr)
library(dplyr)

# Check is the un-clean/original data is present in working directory
# if not, then download and unzip it

if(!file.exists("./UCI HAR Dataset")){
	dataURL<-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	zipName = "UCI_HAR_dataset.zip"
	download.file(dataURL, destfile=zipName)
	unzip(zipName)
}

# Read in the original data and merge the training and test data sets

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

# merge train and test datasets

xMerged <- rbind(x_train, x_test)
yMerged <- rbind(y_train, y_test)
subMerged <- rbind(sub_train, sub_test)

# read the features (column headings) and pick the ones for mean() or std()

features <- read.table("UCI HAR Dataset/features.txt")
colsWithMean<-filter(features,like(V2,"mean\\(\\)"))
colsWithSTD<-filter(features,like(V2,"std\\(\\)"))
colsToSelect<-sort(c(colsWithMean$V1,colsWithSTD$V1))


myData<- xMerged[,colsToSelect]


# Apply the ColumnNames
colnames(myData) <- features[colsToSelect,2]
colnames(yMerged) <- "ActivityName"
colnames(subMerged) <- "Subject"

# Appropriately label the dataset with descriptive variable names in myData

tempVector<-colnames(myData)
for (i in 1:length(tempVector)) {	
	tempVector[i] = gsub("\\()","",tempVector[i])
	tempVector[i] = gsub("-std$","StdDev",tempVector[i])
	tempVector[i] = gsub("^(t)","time",tempVector[i])
	tempVector[i] = gsub("^(f)","frequency",tempVector[i])
	tempVector[i] = gsub("([Gg]ravity)","Gravity",tempVector[i])
	tempVector[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",tempVector[i])
	tempVector[i] = gsub("[Gg]yro","Gyro",tempVector[i])
	tempVector[i] = gsub("AccMag","AccMagnitude",tempVector[i])
	tempVector[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",tempVector[i])
	tempVector[i] = gsub("JerkMag","JerkMagnitude",tempVector[i])
	tempVector[i] = gsub("GyroMag","GyroMagnitude",tempVector[i])
}

colnames(myData) <- tempVector

# Give meaningful names to Activities

yMerged$ActivityName[yMerged$ActivityName == 1] = "WALKING"
yMerged$ActivityName[yMerged$ActivityName == 2] = "WALKING_UPSTAIRS"
yMerged$ActivityName[yMerged$ActivityName == 3] = "WALKING_DOWNSTAIRS"
yMerged$ActivityName[yMerged$ActivityName == 4] = "SITTING"
yMerged$ActivityName[yMerged$ActivityName == 5] = "STANDING"
yMerged$ActivityName[yMerged$ActivityName == 6] = "LAYING"

# Combine the data into one data frame

myCombinedData <- cbind(subMerged,yMerged,myData)

# Create the tidy dataset W\with the average of each variable for each activity
# and each subject.

calcMeanFor<-myCombinedData[,3:dim(myCombinedData)[2]]
tidyData<-aggregate(calcMeanFor,list(myCombinedData$Subject,myCombinedData$ActivityName),mean)

# re-assign Subject and ActivityName column headings

names(tidyData)[1] <- "Subject"
names(tidyData)[2] <- "ActivityName"

# Write tidy data out to file

tidyDataTxtFile <- "./tidyData.txt"
write.table(tidyData,tidyDataTxtFile,row.names=FALSE)


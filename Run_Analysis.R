a# Getting and Cleaning Data - Course Project
# run_analysis.R submission by github.com/sampsonsimpson
#_____________________________________________

#clear workspace memory
rm(list=ls())

#load dplyr and tidyr packages
library(dplyr)
library(tidyr)

#Download and Unzip project files
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(url, destfile = 'samsung.zip', method='curl')
unzip('samsung.zip')


#load features table
features <- read.table('./UCI HAR Dataset/features.txt')
#create vector for only the features with 'Mean' and 'Std' 
featuresLite <- grep('-[Mm]ean()|-[Ss]td()', features[,2])
#get just those feature names for 'Mean' and 'Std' filtering on vector above
featureNames <- as.character(features[featuresLite,2])

#clean up feature names and make them descriptive
featureNames <- gsub('*-meanFreq\\()*', ' MeanFreq ', featureNames)
featureNames <- gsub('*-mean\\()*', ' Mean ', featureNames)
featureNames <- gsub('*-std\\()*', ' StdDev ', featureNames)

#read in activity labels as new table
activityLabels <- read.table('./UCI HAR Dataset/activity_labels.txt')
colnames(activityLabels) <- c('activityID','activity')

#Read in the Training set data but get only the 'Mean' and 'Std' data
#  according to our featuresLite logical vector created above
subjectTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
xtrain <- read.table('./UCI HAR Dataset/train/X_train.txt')[featuresLite]
ytrain <- read.table('./UCI HAR Dataset/train/Y_train.txt')

#Define column names for the training data
colnames(subjectTrain) <- "subjectID"
colnames(xtrain) <- featureNames
colnames(ytrain) <- "activityID"

#combine the subject, y, and x training sets into one
trainSet <- cbind(subjectTrain,ytrain,xtrain)

#Read in the Test set data but get only the 'Mean' and 'Std' data
#  according to our featuresLite vector created above
subjectTest <- read.table('./UCI HAR Dataset/test/subject_test.txt')
xtest <- read.table('./UCI HAR Dataset/test/X_test.txt')[featuresLite]
ytest <- read.table('./UCI HAR Dataset/test/Y_test.txt')

colnames(subjectTest) <- "subjectID"
colnames(xtest) <- featureNames
colnames(ytest) <- "activityID"

#combine the subject, y, and x test sets into one
testSet <- cbind(subjectTest,ytest,xtest)

#combine the training set and test set into one full set
fullSet <- rbind(trainSet,testSet)

#merge the fullSet dataset with the activityLabels table 
fullSet <- merge(activityLabels,fullSet,'activityID')

#gather columns into a long format table to create clean dataset
cleanSet <- gather(fullSet,ncol=4:82)

# separating the feature column into the three delimited parts
#    and labeling the new cols appropriately: 'feature','measure','XYZ'
# grouping by all relevant columns
# summarising by the group above for the average of all values
# spreading the dataset by the newly created 'measure' column
tidySet <- cleanSet %>%
    separate(key, into=c('feature','measure','XYZ'), sep = ' ') %>%
    group_by(feature, activityID, activity, subjectID, XYZ, measure) %>%
    summarise(Value = mean(value)) %>%
    spread(measure, Value)

#clean XYZ column
tidySet$XYZ <- gsub('-','',tidySet$XYZ)
#convert to data frame    
tidySet <- as.data.frame(tidySet)
#save tidySet as Rdata file
save(tidySet, file='tidySet.Rdata')
write.table(tidySet, file='tidySet.txt')

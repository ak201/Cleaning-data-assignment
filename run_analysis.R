setwd('C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/');
getwd()

##1.Merges the training and the test sets to create one data set##

## Reading the datasets downloaded in the pc##

features <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/features.txt", header = FALSE)
activityLabel <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/activity_labels.txt", header = FALSE)


## loading train datasets##
subjectTrain <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
xTrain <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/train/X_train.txt", header = FALSE)
yTrain <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/train/Y_train.txt", header = FALSE)

##Assigning column names####

colnames(activityLabel)= c('activityid','activity type')
colnames(subjectTrain)= "subjectid"
colnames(xTrain)= features[,2]
colnames(yTrain)="activityid"

##merging##
trainfinalData = cbind(subjectTrain,xTrain,yTrain)


##Now loading test datasets##
subjectTest <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xTest <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/test/X_test.txt", header = FALSE)
yTest <- read.table("C:/Users/amank/Downloads/cleaning_assignment/UCI HAR Dataset/test/Y_test.txt", header = FALSE)

##assigning names##
colnames(subjectTest) = "subjectid"
colnames(xTest) = features[,2]
colnames(yTest)= "activityid"
 
## merging##
testFinalData = cbind(subjectTest,xTest,yTest)

####combining both datasets####
realDataset = rbind(trainfinalData,testFinalData)

colNames= colnames(realDataset)

##2.Extracts only the measurements on the mean and standard deviation for each measurement.##

## using grepl and not grep as grep gives the column names and we do not want in this case and hence uding grepl as it helps to find the values and display the number of TRUE and FALSE from the dataset ##

measurement <-(grepl("activityid",colNames) | grepl("subjectid",colNames) | grepl("-mean",colNames)) &
  realDataset <- realDataset[measurement==TRUE]          


##3.Uses descriptive activity names to name the activities in the data set##

##updating the dataset by using the activity Labels names on activity id##

realDataset = merge(realDataset,activityLabel,by='activityid',all.x = TRUE)
colNames = colnames(realDataset)


##4.Appropriately labels the data set with descriptive variable names.## 
##Cleaning and labeling data
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])                       ## gsub for substituting the unclean variables and data##
  colNames[i] = gsub("-std$","Standard Deviation",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  
}

colnames(realDataset)= colNames

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

secondDataset = realDataset[,names(realDataset) !='activityLabel']


##average of each variable for each activity using the aggregate function##

finalDataset =aggregate(secondDataset[,names(secondDataset) != c('activityid','subjectid')],by=list(activityid=secondDataset$activityid,subjectid=secondDataset$subjectid),mean)

finalDataset = merge(finalDataset,activityLabel,by='activityid',all.x=TRUE)

##tidy dataset
write.table(finalDataset,'./finalDataset.txt',row.names = TRUE,sep = '\t')
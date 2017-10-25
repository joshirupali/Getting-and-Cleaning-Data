rm(list = ls())

getwd()
setwd('E:/R/Getting and Cleaning Data/UCI HAR Dataset')

# 1. Merge the training and the test sets to create one data set.
features<-read.table('./features.txt',header = FALSE)
activityType<-read.table('./activity_labels.txt',header = FALSE)
subjecttrain<-read.table('./train/subject_train.txt',header = FALSE)
xTrain<-read.table(file.choose(),header = FALSE)
yTrain<-read.table(file.choose(),header = FALSE)


colnames(activityType)  = c('activityId','activityType');
colnames(subjecttrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain) = "activityId"

list.files(path = getwd())

trainingdata=cbind(yTrain,subjecttrain,xTrain)

subjectest<-read.table('./test/subject_test.txt',header = FALSE)
xTest<-read.table(file.choose(),header = FALSE)
yTest<-read.table(file.choose(),header = FALSE)

colnames(subjectest)  = "subjectId";
colnames(xTest)        = features[,2]; 
colnames(yTest) = "activityId"

testdata<-cbind(yTest,subjectest,xTest)

finalData<-rbind(trainingdata,testdata)

colNames = colnames(finalData);
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];


# 3. Use descriptive activity names to name the activities in the data set
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)

colNames = colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names.
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(finalData) = colNames



# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')




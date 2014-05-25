library(data.table)
library(Hmisc)

cleanName <- function(n) {
    n <- sub('tBody','timeDomainBody',n)
    n <- sub('fBody','frequencyDomainBody',n)    
    n <- sub('tGravity','timeDomainGravity',n)
    n <- sub('fGravity','frequencyDomainGravity',n)
    n <- sub('Acc','Accelerometer',n)
    n <- sub('Gyro','Gyroscope',n)
    n <- sub('-mean','Mean',n)
    n <- sub('-std','Std',n)
    n <- sub('-X','X',n)
    n <- sub('-Y','Y',n)
    n <- sub('-Z','Z',n)
    n <- gsub('\\(|\\)|\\-|\\,','',n)
    n
}

# Step 1.
# load features
message('Loading Feature Labels...')
features <- read.table('UCI HAR Dataset/features.txt',
                       col.names = c('index','label'))
message('Modifying Feature Labels...')
features <- lapply(features$label,cleanName)


# load activity labels
message('Loading Activity Labels...')
labels <- read.table('UCI HAR Dataset/activity_labels.txt',
                       col.names = c('index','label'))

# Loading Training Data
message('Loading Training Data...')
message('-> Subject')
train_subject <- read.table(
    'UCI HAR Dataset/train/subject_train.txt',
    col.names = c('subjectId'))

# Fread has issue with the 'empty' column that I don't know how to fix
message('-> X')
train_data <- read.table(
    'UCI HAR Dataset/train/X_train.txt',
    col.names = features,
    sep = "",
    colClasses = c(rep("numeric",561)),
    header = FALSE,
    nrows = 7352)
train_data <- data.table(train_data)

message('-> Labels')
train_labels <- read.table(
    'UCI HAR Dataset/train/Y_train.txt',
    col.names = c('activityId'))

# Loading Test Data
message('Loading Test Data...')
message('-> Subject')
test_subject <- read.table(
    'UCI HAR Dataset/test/subject_test.txt',
    col.names = c('subjectId'))
message('-> X')
test_data <- read.table(
    'UCI HAR Dataset/test/X_test.txt',
    col.names = features,
    sep = "",
    colClasses = c(rep("numeric",561)),
    header = FALSE,
    nrows = 7352)
test_data <- data.table(test_data)

message('-> Labels')
test_labels <- read.table(
    'UCI HAR Dataset/test/Y_test.txt',
    col.names = c('activityId'))

message('-> Done Loading')


message('Cleaning up data')
# Features not needed anymore
rm(features)
# trim down data by returning all columns that do not match std or mean
# Part 2
train_data[,grep('Std|Mean',colnames(train_data),invert=TRUE):=NULL]
test_data[,grep('Std|Mean',colnames(test_data),invert=TRUE):=NULL]

# merge our frames
test_data_merged <- cbind(test_subject,test_labels,test_data)
train_data_merged <- cbind(train_subject,train_labels,train_data)
# remove unused
rm('test_data','test_labels','test_subject','train_data','train_labels','train_subject')
# merge Part 1
dataset <- data.table(rbind(train_data_merged,test_data_merged))
# remove
rm('train_data_merged','test_data_merged')
# alter Part 3
dataset[,'activityName':=sapply(dataset$activityId,function(o){as.character(labels[[o,2]])})]
dataset[,'activityId':=NULL]
#remove
rm('labels')
# Create new set Part 5
meanDataset <- aggregate(dataset,list(dataset$subjectId,dataset$activityName),mean)
meanDataset <- data.table(meanDataset)
#clean
meanDataset[,c('activityName','Group.1'):=NULL]
setnames(meanDataset,"Group.2","activityName")
#write data set
write.csv(meanDataset,'tidy.csv')

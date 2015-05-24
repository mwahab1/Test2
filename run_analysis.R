
# This R file was created on a Windows machine running Windows 7, R 3.2 
# This requires the dplyr package

require(dplyr)

# The instructions say the data will be in the current directory. 
# This script  assumes the whole  zipfile has been unzipped and test and train 
# directories exist in the current directory. 

# Check to make sure all required data files exist before loading any data 
# If any required file dors not exist- stop the program.

if(!file.exists("./test/X_test.txt")){stop("X_test.txt file not found")}
if(!file.exists("./test/y_test.txt")){stop("y_test.txt file not found")}

if(!file.exists("./test/subject_test.txt")){stop("subject_test.txt file not found")}
if(!file.exists("./train/X_train.txt")){stop("X_train.txt file not found")}
if(!file.exists("./train/y_train.txt")){stop("y_train.txt file not found")}
if(!file.exists("./train/subject_train.txt")){stop("subject_train.txt file not found")}
if(!file.exists("features.txt")){stop("features.txt file not found")}
if(!file.exists("activity_labels.txt")){stop("activity_labels.txt file not found")}

# 
##Load data files. 



#Load measurements data - test data set


df_X_test <- read.table(file = "./test/X_test.txt", header=FALSE,sep="")

#Load corresponding activity file . Call the column "acitivity".
df_y_test <- read.table(file = "./test/y_test.txt", 
                        header=FALSE,sep="" , col.names=c("activity")) 

#Load subjects for the test data set. Call the column "subject".
df_subject_test <- read.table(file = "./test/subject_test.txt", 
                              header=FALSE,sep="" , col.names=c("subject"))

#Load measurements data - training data set
df_X_train <- read.table(file = "./train/X_train.txt", 
                         header=FALSE,sep="")

#Load training activity file- training data set. Call the column "activity"
df_y_train <- read.table(file = "./train/y_train.txt", 
                         header=FALSE,sep="" , col.names=c("activity"))

#Load training subject file- training data set. Call the colum "subject". 

df_subject_train <- read.table(file = "./train/subject_train.txt", 
                               header=FALSE,sep="", col.names=c("subject"))

#Read in features file

df_features <- read.table(file = "features.txt", 
                          header=FALSE,sep="",stringsAsFactors= FALSE)

#Read in Activity Lables file

df_activities <- read.table(file = "activity_labels.txt", 
                            header=FALSE,sep="",  stringsAsFactors= FALSE)


#Add column names for the measurement data sets from the features.txt file read 
#into the the data frame df_features, earlier. 


colnames(df_X_train) <- df_features[,2]
colnames(df_X_test) <- df_features[,2]
#=============================

# We need only columns that are mean/standard deviation measurements. Since 
# variables like angle(Z,gravityMean) may or may not be necessary, keep these. 
# Depending on future usage of this data, these columns maybe needed. 
# Eg. Say average angle maybe a more relevant measure of angular movement.      


# Get the list of columns from features list that we want to keep
# Any column that has mean/Mean/std in the feature name

mean_std_list<- df_features[grep("mean|Mean|std", df_features$V2),]
featurecolumns <-mean_std_list[,1]

# Subset measurement data to include only columns that have mean/std etc
# This removes unnecessary columns.

df_X_train <- df_X_train[,c(featurecolumns)]
df_X_test <- df_X_test[,c(featurecolumns)]  




# Combine the subbject ID data frame, the activity dataframe and the measures
# data frames into one combined data frame each for training and test data
# Simple column bind will get us this based on the dataset descriptions.

df_train <- cbind(df_subject_train, df_y_train, df_X_train)

df_test <- cbind(df_subject_test, df_y_test, df_X_test)


# Combine the two data sets...simple row bind.

df_dataset <- rbind(df_train, df_test)

#replace the values 1-6 in activity column with text labels from activity_labels
# 1 becomes Walking, 2-> Walking Upstairs etc

df_dataset[,2] <- factor(df_dataset$activity,labels=df_activities[,2])

# Create a new list of descriptive column names


new_column_names<-c(
      "subject"
      ,"activity"
      ,"TimeBodyAccelerationmeanXAxis"
      ,"TimeBodyAccelerationmeanYAxis"
      ,"TimeBodyAccelerationmeanZAxis"
      ,"TimeBodyAccelerationstandarddeviationXAxis"
      ,"TimeBodyAccelerationstandarddeviationYAxis"
      ,"TimeBodyAccelerationstandarddeviationZAxis"
      ,"TimeGravityAccelerationmeanXAxis"
      ,"TimeGravityAccelerationmeanYAxis"
      ,"TimeGravityAccelerationmeanZAxis"
      ,"TimeGravityAccelerationstandarddeviationXAxis"
      ,"TimeGravityAccelerationstandarddeviationYAxis"
      ,"TimeGravityAccelerationstandarddeviationZAxis"
      ,"TimeBodyAccelerationJerkmeanXAxis"
      ,"TimeBodyAccelerationJerkmeanYAxis"
      ,"TimeBodyAccelerationJerkmeanZAxis"
      ,"TimeBodyAccelerationJerkstandarddeviationXAxis"
      ,"TimeBodyAccelerationJerkstandarddeviationYAxis"
      ,"TimeBodyAccelerationJerkstandarddeviationZAxis"
      ,"TimeBodyGyroscopemeanXAxis"
      ,"TimeBodyGyroscopemeanYAxis"
      ,"TimeBodyGyroscopemeanZAxis"
      ,"TimeBodyGyroscopestandarddeviationXAxis"
      ,"TimeBodyGyroscopestandarddeviationYAxis"
      ,"TimeBodyGyroscopestandarddeviationZAxis"
      ,"TimeBodyGyroscopeJerkmeanXAxis"
      ,"TimeBodyGyroscopeJerkmeanYAxis"
      ,"TimeBodyGyroscopeJerkmeanZAxis"
      ,"TimeBodyGyroscopeJerkstandarddeviationXAxis"
      ,"TimeBodyGyroscopeJerkstandarddeviationYAxis"
      ,"TimeBodyGyroscopeJerkstandarddeviationZAxis"
      ,"TimeBodyAccelerationMagnitudemean"
      ,"TimeBodyAccelerationMagnitudestandarddeviation"
      ,"TimeGravityAccelerationMagnitudemean"
      ,"TimeGravityAccelerationMagnitudestandarddeviation"
      ,"TimeBodyAccelerationJerkMagnitudemean"
      ,"TimeBodyAccelerationJerkMagnitudestandarddeviation"
      ,"TimeBodyGyroscopeMagnitudemean"
      ,"TimeBodyGyroscopeMagnitudestandarddeviation"
      ,"TimeBodyGyroscopeJerkMagnitudemean"
      ,"TimeBodyGyroscopeJerkMagnitudestandarddeviation"
      ,"FrequencyDomainBodyAccelerationmeanXAxis"
      ,"FrequencyDomainBodyAccelerationmeanYAxis"
      ,"FrequencyDomainBodyAccelerationmeanZAxis"
      ,"FrequencyDomainBodyAccelerationstandarddeviationXAxis"
      ,"FrequencyDomainBodyAccelerationstandarddeviationYAxis"
      ,"FrequencyDomainBodyAccelerationstandarddeviationZAxis"
      ,"FrequencyDomainBodyAccelerationmeanFrequencyXAxis"
      ,"FrequencyDomainBodyAccelerationmeanFrequencyYAxis"
      ,"FrequencyDomainBodyAccelerationmeanFrequencyZAxis"
      ,"FrequencyDomainBodyAccelerationJerkmeanXAxis"
      ,"FrequencyDomainBodyAccelerationJerkmeanYAxis"
      ,"FrequencyDomainBodyAccelerationJerkmeanZAxis"
      ,"FrequencyDomainBodyAccelerationJerkstandarddeviationXAxis"
      ,"FrequencyDomainBodyAccelerationJerkstandarddeviationYAxis"
      ,"FrequencyDomainBodyAccelerationJerkstandarddeviationZAxis"
      ,"FrequencyDomainBodyAccelerationJerkmeanFrequencyXAxis"
      ,"FrequencyDomainBodyAccelerationJerkmeanFrequencyYAxis"
      ,"FrequencyDomainBodyAccelerationJerkmeanFrequencyZAxis"
      ,"FrequencyDomainBodyGyroscopemeanXAxis"
      ,"FrequencyDomainBodyGyroscopemeanYAxis"
      ,"FrequencyDomainBodyGyroscopemeanZAxis"
      ,"FrequencyDomainBodyGyroscopestandarddeviationXAxis"
      ,"FrequencyDomainBodyGyroscopestandarddeviationYAxis"
      ,"FrequencyDomainBodyGyroscopestandarddeviationZAxis"
      ,"FrequencyDomainBodyGyroscopemeanFrequencyXAxis"
      ,"FrequencyDomainBodyGyroscopemeanFrequencyYAxis"
      ,"FrequencyDomainBodyGyroscopemeanFrequencyZAxis"
      ,"FrequencyDomainBodyAccelerationMagnitudemean"
      ,"FrequencyDomainBodyAccelerationMagnitudestandarddeviation"
      ,"FrequencyDomainBodyAccelerationMagnitudemeanFrequency"
      ,"FrequencyDomainBodyAccelerationJerkMagnitudemean"
      ,"FrequencyDomainBodyAccelerationJerkMagnitudestandarddeviation"
      ,"FrequencyDomainBodyAccelerationJerkMagnitudemeanFrequency"
      ,"FrequencyDomainBodyGyroscopeMagnitudemean"
      ,"FrequencyDomainBodyGyroscopeMagnitudestandarddeviation"
      ,"FrequencyDomainBodyGyroscopeMagnitudemeanFrequency"
      ,"FrequencyDomainBodyGyroscopeJerkMagnitudemean"
      ,"FrequencyDomainBodyGyroscopeJerkMagnitudestandarddeviation"
      ,"FrequencyDomainBodyGyroscopeJerkMagnitudemeanFrequency"
      ,"angleofTimeBodyAccelerationMeanandGravityMean"
      ,"angleofTimeBodyAccelerationJerkMeanandGravityMean"
      ,"angleofTimeBodyGyroscopeMeanandGravityMean"
      ,"angleofTimeBodyGyroscopeJerkMeanandGravityMean"
      ,"angleofXaxisandGravityMean"
      ,"angleofYaxisandGravityMean"
      ,"angleofZaxisandGravityMean"
)

# Rename the columns in out dataset using the new list
colnames(df_dataset) <- new_column_names

# Create Our Tidy Dataset of by calculating the means of each columns 
# grouped by activity and subject  

df_tidy <- 
      group_by(df_dataset, subject, activity) %>% summarise_each(funs(mean))


write.table(df_tidy, file="Tidy_Samsung.txt", quote=FALSE,sep=" ", 
            row.names=FALSE,col.names=TRUE)

### Sample code to read in data 
### df_readdata_txt<- read.table("Tidy_Samsung.txt", header=TRUE)

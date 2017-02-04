#Run_Analysis

##Clear Environment and Load Packages
rm(list=ls())
require(dplyr)
require(tidyr)

#Step 1: "Merges the training and test sets to create one data set.
##Loading Data

##Assigning Training Data to Variables

trainy<-read.table(file=".\\train\\y_train.txt",header=F,
                   col.names = "Labels",colClasses = "character")

trainsubject<-read.table(file = ".\\train\\subject_train.txt",header=F,
                         col.names = "Subject")

trainx<-read.table(file=".\\train\\X_train.txt",header=F)

##Loading Testing Data to Variables

testy<-read.table(file=".\\test\\y_test.txt",header=F,
                  col.names = "Labels")

testsubject<-read.table(file = ".\\test\\subject_test.txt",header=F,
                        col.names = "Subject")

testx<-read.table(file=".\\test\\X_test.txt",header=F)

##Merging Training and Testing Data
testdata<-cbind(testx,set="test",testsubject,testy)
traindata<-cbind(trainx,set="train",trainsubject,trainy)
fulldata<-rbind(testdata,traindata)

#Step 2. "Extracts only the measurements on the mean and standard deviationfor each measurement."

##Determining which measurements are on mean and std. dev.

###Loading feature labels
featurelabels<-read.table(".\\features.txt",header=F)

###Generating T/F vector for contains "std" or "mean", plus TRUEs for other columns
meansandstds<-c(grepl("std",x=featurelabels[,2])|grepl("mean",x = featurelabels[,2]),T,T,T)

###Subsets fulldata by columns which are TRUE for subsetting T/F vector
subdata<-fulldata[,meansandstds]

#Step 3. "Uses descriptive activity names to name the activities in the data set"

###Loading Activity Labels
activities<-read.table(".\\activity_labels.txt")

###Replacing numeric Activity Labels with Descriptive Activity Names
fulldata$Labels<-as.character(factor(x=fulldata$Labels,levels=1:6,labels=as.character(activities[,2])))
subdata$Labels<-as.character(factor(x=subdata$Labels,levels=1:6,labels=as.character(activities[,2])))

#Step 4. "Appropriately labels the data set with descriptive variable names."

##Due to length of some variable names exceeding max length, abbreviations must be used.

featurelabels[,2]<-as.character(featurelabels[,2])
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "Body",replacement="B",ignore.case = T)
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "Gravity",replacement="Gr",ignore.case = T)
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "Gyro",replacement="Gy",ignore.case = T)
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "Jerk", replacement = "Jk",ignore.case = T)
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "bandsEnergy",replacement = "bE",ignore.case = T)
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "correlation",replacement = "corr",ignore.case = T)
featurelabels[,2]<-gsub(x = featurelabels[,2],pattern = "kurtosis",replacement = "Kurt",ignore.case = T)
##Once feature labels have been abbreviated, appply to full and subdata using grepl T/F vector approach.

names(fulldata)<-c(featurelabels[,2],"set","Subject","Labels")

names(subdata)<-c(featurelabels[grepl("std",x=featurelabels[,2])|grepl("mean",x = featurelabels[,2]),2],"set","Subject","Labels")

#Step 5. "From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."

subdatatbl<-tbl_df(data = subdata)
bysubjact<-group_by(.data = subdatatbl,Subject,Labels)
summed<-summarise_all(bysubjact,.funs = mean)
avgdata<-as.data.frame(x=summed)

#Final Step: EXPORT!

write.csv(x=subdata,file="Mean and Standard Deviation Data.csv")
write.table(x=avgdata,file="Averages by Subject and Activity Data.txt",row.names = F)

#Appendix: Appending Features File with Shorthand Version
featurelabelsfull<-(cbind(read.table(".\\features.txt",header=F),featurelabels[,2]))
names(featurelabelsfull)<-c("Index","Full","Shorthand")

write.csv(x = featurelabelsfull,file = ".\\features.txt",row.names = F)

run_analysis<-function(){
  
  library(plyr)
  library(reshape2)
  
  
  # Train data 
  train_subjects_data<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/train/subject_train.txt")
  
  train_activities_data<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/train/Y_train.txt")
  
  train_data<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/train/X_train.txt")
  
  # Get the names of the variables 
  
  features<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/features.txt")  
  # Rename the columns
  names(train_data)<-features[,2]
  
  # only columns with mean() or std()
  
  train_data<-train_data[,grep("mean\\(\\)|std\\(\\)", names(train_data), value=TRUE)]
  
  
  # add activity and subject columns to the train data
  train_data$subject<-train_subjects_data[,1]
  train_data$activity<-train_activities_data[,1]
  
  
# Test data 
  
  test_subjects_data<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/test/subject_test.txt")
  
  test_activities_data<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/test/y_test.txt")
  
  test_data<-read.table("~/getdata_projectfiles_UCI/UCI HAR Dataset/test/X_test.txt")

# Clean the names


# Rename the columns
names(test_data)<-features[,2]

# only columns with mean() or std()

test_data<-test_data[,grep("mean\\(\\)|std\\(\\)", names(test_data), value=TRUE)]

  
# add activity and subject columns to the test data
   test_data$subject<-test_subjects_data[,1]
   test_data$activity<-test_activities_data[,1]

# Combine the train and test data
  merged_data<-rbind(train_data, test_data)
  
 
  


  
# activities label

   merged_data$activity<-sub( "^1", "WALKING2",merged_data[,68])
   merged_data$activity<-sub( "^2", "WALKING_UPSTAIRS3", merged_data[,68])
   merged_data$activity<-sub( "^3", "WALKING_DOWNSTAIRS4", merged_data[,68])
   merged_data$activity<-sub( "^4", "SITTING5", merged_data[,68])
   merged_data$activity<-sub( "^5", "STANDING6", merged_data[,68])
   merged_data$activity<-sub( "^6", "LAYING", merged_data[,68])



   names(merged_data)<-gsub("-","_",names(merged_data))
   names(merged_data)<-gsub("\\(\\)","",names(merged_data))
   names(merged_data)<-gsub("BodyBody","Body",names(merged_data))

  melted_data <- melt(merged_data, id.vars=c("subject", "activity"))
 tidy_data<-dcast(melted_data, subject+activity~variable, mean)

  write.table(tidy_data, "tidydata.txt", row.names=FALSE)






  
  
}
## run_analysis.R

library(reshape2)

# step1
#   Merges the training and the test sets to create one data set.
#   argument:   [path] - data set path (ex: /work/Getting-and-Cleaning-Data/data)
#   return:     [step1_val] - train & test data
step1 <- function(path) {
    # backup and replace working dir
    backup_wd <- getwd()
    setwd(path)

    xtrain <- read.table("train/X_train.txt")
    ytrain <- read.table("train/y_train.txt")
    subtrain <- read.table("train/subject_train.txt")

    xtest <- read.table("test/X_test.txt")
    ytest <- read.table("test/y_test.txt")
    subtest <- read.table("test/subject_test.txt")

    # restore working dir
    setwd(backup_wd)

    wholedata <- rbind(cbind(xtrain,ytrain,subtrain),cbind(xtest,ytest,subtest))

    return(wholedata)
}


# step2
#   Extracts only the measurements on the mean and standard deviation for each measurement.
#   argument:   [path] - data set path (ex: /work/Getting-and-Cleaning-Data/data)
#               [step1_val] - result step1 data
#   return:     [step2_val] - Extracts data set
step2 <- function(path, step1_val) {
    # backup and replace working dir
    backup_wd <- getwd()
    setwd(path)
    features <- read.table("features.txt")
    # restore working dir
    setwd(backup_wd)

    # retrive the index that contains the "mean" or "std" in a variable
    retrive_indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

    # leaves the Y and Subject to index
    retrivedata <- step1_val[, append(retrive_indices, c(length(step1_val)-1,length(step1_val)))]

    # name on the label
    namelist <- gsub("\\(\\)", "", features[retrive_indices, 2])
    names(retrivedata) <- append(namelist,c("label","subject"))

    return(retrivedata)
}

# step3
#   Uses descriptive activity names to name the activities in the data set
#   argument:   [path] - data set path (ex: /work/Getting-and-Cleaning-Data/data)
#               [step2_val] - result step2 data
#   return:     [step3_val] - name the activities in the data set
step3 <- function(path, step2_val) {
    # backup and replace working dir
    backup_wd <- getwd()
    setwd(path)
    activity <- read.table("activity_labels.txt")
    # restore working dir
    setwd(backup_wd)

    # create activityLabel and bind data
    activityLabel <- activity[step2_val[, length(step2_val)-1], 2]
    activityjoindata <- cbind(step2_val, activityLabel)
    names(activityjoindata)[length(activityjoindata)] <- "activity"

    return(activityjoindata)
}

# step4
#   Appropriately labels the data set with descriptive variable names.
#   argument:   [step3_val] - result step3 data
#   return:     [step4_val] - name on the label
step4 <- function(step3_val) {
    step4_val <- step3_val
    t4name <- names(step4_val)
    t4name <- gsub("^t","denote time ",t4name)
    t4name <- gsub("^f","indicate frequency domain signals ",t4name)
    t4name <- gsub("Acc","Acceleration",t4name)
    t4name <- gsub("Gyro","Gyroscope",t4name)
    t4name <- gsub("Mag"," Magnitude",t4name)
    names(step4_val) <- t4name
    return(step4_val)
}

# step5
#   From the data set in step 4, creates a second,
#   independent tidy data set with the average of each variable for each activity and each subject.
#   argument:   [step4_val] - result step4 data
#   return:     [step5_val] - result data set
step5 <- function(step4_val) {
    v_melt <- melt(step4_val, id = c("subject","activity"))
    v_mean <- dcast(v_melt, subject + activity ~ variable, mean)
    return(v_mean)
}

# step_1to5
#   cleanup data and write txt file
#   argument:   [path] - data set path (ex: /work/Getting-and-Cleaning-Data/data)
#   return:     [step5_val] - result step5 data
#
step_1to5 <- function(path) {
    t1 <- step1(path)
    t2 <- step2(path,t1)
    t3 <- step3(path,t2)
    t4 <- step4(t3)
    t5 <- step5(t4)

    backup_wd <- getwd()
    setwd(path)
    write.table(t5, "outputdata_step5.txt", row.name=FALSE)
    # restore working dir
    setwd(backup_wd)

    return(t5)
}

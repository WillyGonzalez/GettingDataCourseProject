## README FILE

Author: Willy Gonzalez

# Includes all the scripts performed, along with the comments on how and why I performed each step



run_analysis <- function (){
    
    #############################################################################
    ############################### PART 1 ######################################
    #############################################################################
    # Set the working directory
    wd<-"~/Development/R/3. Getting and cleaning data/Course Project"
    setwd(wd)
    prepath="/UCI HAR Dataset/"
    
    # Generate all the paths for the files
    actpath<-paste(wd, prepath, "activity_labels.txt", sep="")
    featpath<-paste(wd, prepath, "features.txt", sep="")
    
    xtrainpath<-paste(wd, prepath, "train/X_train.txt", sep="")
    strainpath<-paste(wd, prepath, "train/subject_train.txt", sep="")
    ytrainpath<-paste(wd, prepath, "train/Y_train.txt", sep="")
    
    xtestpath<-paste(wd, prepath, "test/X_test.txt", sep="")
    stestpath<-paste(wd, prepath, "test/subject_test.txt", sep="")
    ytestpath<-paste(wd, prepath, "test/Y_test.txt", sep="")
    
    
    act<- read.table(actpath, quote="\"", stringsAsFactors=F)
    feat<-read.table(featpath, quote="\"", stringsAsFactors=F)
    
    xtrain <- read.table(xtrainpath, quote="\"", stringsAsFactors=F)
    strain <- read.table(strainpath, quote="\"", stringsAsFactors=F)
    ytrain <- read.table(ytrainpath, quote="\"", stringsAsFactors=F)
    
    xtest <- read.table(xtestpath, quote="\"", stringsAsFactors=F)
    stest <- read.table(stestpath, quote="\"", stringsAsFactors=F)
    ytest <- read.table(ytestpath, quote="\"", stringsAsFactors=F)
    
    for (i in 1:6){
        ytrain[ytrain$V1 == i,] <- act[i,2]
    }
    
    for (i in 1:6){
        ytest[ytest$V1 == i,] <- act[i,2]
    }
    
    # Combine train and test datasets
    x<-rbind(xtrain,xtest)
    s<-rbind(strain,stest)
    y<-rbind(ytrain,ytest)
    
    # Add column names
    names(x)<-feat$V2
    names(s)<-"Subject"
    names(y)<-"Activity"
    
    # Combine the features, subject and activity dataframes
    tempData<-cbind(x,s)
    combData<-cbind(tempData,y)
    
    
    #############################################################################
    ############################### PART 2 ######################################
    #############################################################################
    
    #Find the locations of features with strings "mean" and "std"
    meanloc<-grep("mean()",feat$V2)
    stdloc<-grep("std()",feat$V2)
    
    #Subset and take only those columns
    msData<-combData[,c(meanloc,stdloc,562,563)]
    
    #############################################################################
    ############################### PART 3 ######################################
    #############################################################################
    
    #Already done with the following code
    #names(x)<-feat$V2
    #names(s)<-"Subject"
    #names(y)<-"Activity"
    
    #############################################################################
    ############################### PART 4 ######################################
    #############################################################################
    
    # Remove the parenthesis on the names for further readability
    
    names(msData)<-sub("\\(\\)","",names(msData))
    
    
    #############################################################################
    ############################### PART 5 ######################################
    #############################################################################
    
    library(plyr)
    attach(msData)
    aggData <-aggregate(msData[,1:79], by=list(Subject, Activity), FUN=mean, na.rm=TRUE)
    names(aggData)[1]<-"Subject"
    names(aggData)[2]<-"Activity"
    
    write.table(aggData,"tidyDataSet.txt",row.name=F)
    
    return(aggData)
}
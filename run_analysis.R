## Hello , welcome to my data cleaning script
## Please be patient, takes TIME TO RUN
## Prepare the work environment

## clear the work environment
rm(list = ls())  
## if no data folder exists, create it
if(!file.exists("./data")){
  dir.create("./data")
  } #end if no data folder exists
## if no data file exists, download it
if(!file.exists("./data/Dataset.zip")){
  ## download the data file
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "./data/Dataset.zip")
} #end if no data file exists
## if no unzipped files wxist, unzip the files
if(!file.exists("./data/UCI HAR Dataset")){
  ## Unzip the files
  unzip("./data/Dataset.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = "./data", unzip = "internal", setTimes = FALSE)
} # end if no unzipped files exist

## Load the data into R

## Load the header
HeadCol1 <- read.table("/home/sam/data/UCI HAR Dataset/features.txt")

## Load the test tables
X_testTable <- read.table("/home/sam/data/UCI HAR Dataset/test/X_test.txt")
Y_testTable <- read.table("/home/sam/data/UCI HAR Dataset/test/y_test.txt")
subject_testTable <- read.table("/home/sam/data/UCI HAR Dataset/test/subject_test.txt")
## make the headers
names(X_testTable) <- HeadCol1$V2
names(Y_testTable) <- "activity"
names(subject_testTable) <- "subject"
## Combine the tables
XY_testTable <- cbind(Y_testTable,X_testTable)
XYs_testTable <- cbind(subject_testTable,XY_testTable)

## Load the train tables
X_trainTable <- read.table("/home/sam/data/UCI HAR Dataset/train/X_train.txt")
Y_trainTable <- read.table("/home/sam/data/UCI HAR Dataset/train/y_train.txt")
subject_trainTable <- read.table("/home/sam/data/UCI HAR Dataset/train/subject_train.txt")
## make the headers
names(X_trainTable) <- HeadCol1$V2
names(Y_trainTable) <- "activity"
names(subject_trainTable) <- "subject"
## Combine the tables
XY_trainTable <- cbind(Y_trainTable,X_trainTable)
XYs_trainTable <- cbind(subject_trainTable,XY_trainTable)

## Combine test and train
CombinedTable <- rbind(XYs_testTable, XYs_trainTable)

## Remove all non- mean or standard deviation columns
colcount <- ncol(CombinedTable)
## Step backwards because deleting columns
for(i in colcount:3){ # for each column in Combined table except first two
  if(!grepl("(-mean)|(std)",colnames(CombinedTable)[i])){ # if condition
    CombinedTable <- CombinedTable[-i]
  } # end ifcondition
} # end for each column in Combined table

## Remove all columns that have Freq
colcount <- ncol(CombinedTable)
## Step backwards because deleting columns
for(i in colcount:3){ # for each column in Combined table except first two
  if(grepl("Freq",colnames(CombinedTable)[i])){ # if condition
    CombinedTable <- CombinedTable[-i]
  } # end ifcondition
} # end for each column in Combined table


## change column names to descriptive
colcount <- ncol(CombinedTable)
for(i in 1:colcount){ #for each column in CombinedTable
  colnames(CombinedTable)[i] <- gsub("^t", "time ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("^f", "fourier ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("[(][)]", "", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("std", "standard deviation", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("Acc", " acceleration ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("Gyro", " gyroscope ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("Body", " body ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("Mag", " magnet ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("Gravity", " gravity ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("Jerk", " jerk ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("[-]", " ", colnames(CombinedTable)[i])
  colnames(CombinedTable)[i] <- gsub("  ", " ", colnames(CombinedTable)[i])
} # end for each column in CombinedTable

## change activities to descriptive
rowcount <- nrow(CombinedTable)
for(i in 1:rowcount){# for each row in CombinedTable
  CombinedTable$activity[i] <- gsub("1", "walking", CombinedTable$activity[i])
  CombinedTable$activity[i] <- gsub("2", "walking upstairs", CombinedTable$activity[i])
  CombinedTable$activity[i] <- gsub("3", "walking downstairs", CombinedTable$activity[i])
  CombinedTable$activity[i] <- gsub("4", "sitting", CombinedTable$activity[i])
  CombinedTable$activity[i] <- gsub("5", "standing", CombinedTable$activity[i])
  CombinedTable$activity[i] <- gsub("6", "laying", CombinedTable$activity[i])
} # end  for each row in CombinedTable

### THIS IS THE FIRST TABLE

#CombinedTable

# dimension the second table
TableTwo <- matrix(c(0), nrow = 180,ncol = colcount) # same columns as CombinedTable, rows = subjects * activities
TableTwo <- as.data.frame(TableTwo) # because data types
names(TableTwo) <- names(CombinedTable) # same columns as CombinedTable
## Populate first two columns
counter <- 1
TempVector <- c()
for(i in 1:30){# for each subject
  for(j in 1:6){# for each activity
    TempVector <- c(TempVector, counter)
    } # end for each activity
counter <- counter +1
}# end for each subject
TableTwo$subject <- TempVector

TempVector <- c()
for(i in 1:30){# for each subject
    TempVector <- c(TempVector, 1:6)
}# end for each subject
TempVector <- gsub("1", "walking", TempVector)
TempVector <- gsub("2", "walking upstairs", TempVector)
TempVector <- gsub("3", "walking downstairs", TempVector)
TempVector <- gsub("4", "sitting", TempVector)
TempVector <- gsub("5", "standing", TempVector)
TempVector <- gsub("6", "laying", TempVector)
TableTwo$activity <- TempVector

## Make a copy of TableTwo for divisor when calculating mean
DivisorTable <- TableTwo

## Poplulate the rest of the columns
rowcount2 <- nrow(TableTwo)
counter <- 0
for(i in 1:rowcount){# for each row in CombinedTable
  for(j in 1:rowcount2){#for each row in TableTwo
    if(CombinedTable$subject[i] == TableTwo$subject[j]){ # if subject = subject
      if(CombinedTable$activity[i] == TableTwo$activity[j]){ # if activity = activity
        TableTwo[j,3:colcount] <-  TableTwo[j,3:colcount] + CombinedTable[i,3:colcount]
        DivisorTable[j,3:colcount] <- DivisorTable[j,3:colcount] +1
      }# end if activity = activity
    }# end if subject = subject
    
    
  }# end for each row in TableTwo
}# end for each row in CombinedTable

## find the mean
TableTwo[,3:colcount] <- TableTwo[,3:colcount] / DivisorTable[,3:colcount]

## Print the Tables
head(CombinedTable)
head(TableTwo)



  
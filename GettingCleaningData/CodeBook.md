---
title: "Codebook.md"
author: "Mike Belli"
date: "2023-12-31"
output: html_document
---
This markdown file is part of Coursera's "Getting and Cleaning Data" class.

The code here is also contained in the script "run_analysis.R". The goal is to 
take the test data (X_test.txt, subject_test.txt, y_test.txt) and the train
data (X_train.txt, subject_train.txt, y_train.txt), perform necessary 
manipulations, and create a tidy data set of averages for each subject. 

Specifically, the instructions are to create one R script called run_analysis.R 
that does the following:
1) Merges the training and the test sets to create one data set.
2) Extracts only the measurements on the mean and standard deviation for each measurement. 
3) Uses descriptive activity names to name the activities in the data set
4) Appropriately labels the data set with descriptive variable names. 
5) From the data set in step 4, creates a second, independent tidy data set 
  with the average of each variable for each activity and each subject.

Here is a description of the data, from the creators of the data:
•	Participants: 30 volunteers, aged 19-48 years.
•	Activities: Six types (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING).
•	Equipment: Samsung Galaxy S II smartphone worn on the waist.
•	Data Capture: 3-axial linear acceleration and 3-axial angular velocity, recorded at 50Hz.
•	Video Recording: Experiments video-recorded for manual data labeling.
•	Dataset Partitioning: Randomly into 70% training data and 30% test data.
•	Pre-processing: Noise filters applied; data sampled in 2.56-second sliding windows (128 readings/window) with 50% overlap.
•	Signal Separation: Sensor acceleration signal divided into body acceleration and gravity using a Butterworth low-pass filter (0.3 Hz cutoff).
•	Feature Extraction: Variables calculated from time and frequency domain for each window.
•	For each record in the dataset: 
    o	Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
    o	Triaxial Angular velocity from the gyroscope. 
    o	A 561-feature vector with time and frequency domain variables.
    o	Its activity label. - An identifier of the subject who carried out the experiment.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
mywd <- getwd()
myfolder <- paste0(mywd,"/")
```

## Here we get the data
## First, I read the data
## Then, I combine the data into a test and train data

```
test_x <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/test/X_test.txt"),stringsAsFactors = FALSE)
test_sub <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/test/subject_test.txt"),stringsAsFactors = FALSE) 
test_y <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/test/y_test.txt"),stringsAsFactors = FALSE)
names(test_sub)[names(test_sub)=="V1"] <- "sub"
names(test_y)[names(test_y)=="V1"] <- "y"
test <- cbind(test_sub,test_y,test_x)
train_x <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/train/X_train.txt"),stringsAsFactors = FALSE)
train_sub <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/train/subject_train.txt"),stringsAsFactors = FALSE) 
train_y <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/train/y_train.txt"),stringsAsFactors = FALSE)
names(train_sub)[names(train_sub)=="V1"] <- "sub"
names(train_y)[names(train_y)=="V1"] <- "y"
train <- cbind(train_sub,train_y,train_x)
train$dataset <- "train"
test$dataset <- "test"
```

## 1) merge training and test sets to create one data set
## I also find the variables that are means and standard
## deviations, and group those into "key_vars" along with
## the subject variable, y


```
df <- rbind(train,test)
# df is the merged data set with train and test

act_label <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/features.txt"),stringsAsFactors = FALSE)
names(act_label)[names(act_label)=="V1"] <- "number"
names(act_label)[names(act_label)=="V2"] <- "label"
mean_vars <- act_label$label[grep("mean()",act_label$label,fixed=TRUE)]
std_vars <- act_label$label[grep("std()",act_label$label,fixed=TRUE)]
key_vars <- c("y",mean_vars,std_vars)
```

# 3) use descriptive activity names
# 4) labels the data set with descriptive variable names

```
check <- df
for (c in names(check)){
  for (b in 1:length(act_label$label)){
    names(check)[names(check)==paste0("V",b)] <- act_label$label[b]
  }
}
#ls(check)
df2 <- check
```


# 2) extracts only measurements on the mean and standard deviation
# 5) from the data, create a second, independent tidy data set with the average of each variable for each activity and each subject
# tidy data has:
# 1) each variable has a column
# 2) each observation is a row
# 3) each observation type has its own table. 
# Here, we only have one type of data, the measurements. If we had subject-specific data, that would
# be it's own table.

## I make the data frame long, then slightly wider and average each mean measurement for each subject.
## I end up with a tidy data set called df_tidy which only contains one row for each subject, and
## has the average of each measurement for which there was a mean in the data.

```
df_data1 <- df2[key_vars]

# drop the standard deviations, and keep only the averages
df_data1 <-df_data1[c("y",mean_vars)]
temp_df <- df_data1

# create a data frame where each row has the observation (mean) for a specific measurement, for a subject
library(dplyr)
library(tidyr)
library(stringr)

# Gather the columns
long_df <- temp_df %>%
  gather(key = "variable", value = "value", -y)
long_df <- long_df %>%
  group_by(y,variable) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Pivot the data to wide format using pivot_wider()
df_tidy <- long_df %>%
  pivot_wider(id_cols = c(y), names_from = variable, values_from = value)

# Rename columns
df_tidy <- df_tidy %>%
  rename_with(~ str_replace_all(., pattern = "-mean\\(\\)-", replacement = "_"),
              contains("-mean()-"))
```

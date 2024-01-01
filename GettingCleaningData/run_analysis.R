#Run_analysis.R
mywd <- getwd()
myfolder <- paste0(mywd,"/")



# read in test and train, then merge them
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

# 1) merge training and test sets to create one data set
df <- rbind(train,test)
# df is the merged data set with train and test

act_label <- read.table(paste0(myfolder,"Learning/Coursera_R/UCI HAR Dataset/features.txt"),stringsAsFactors = FALSE)
names(act_label)[names(act_label)=="V1"] <- "number"
names(act_label)[names(act_label)=="V2"] <- "label"


mean_vars <- act_label$label[grep("mean()",act_label$label,fixed=TRUE)]
std_vars <- act_label$label[grep("std()",act_label$label,fixed=TRUE)]
key_vars <- c("y",mean_vars,std_vars)

#head(act_label)

# 3) use descriptive activity names
# 4) labels the data set with descriptive variable names
check <- df
for (c in names(check)){
  for (b in 1:length(act_label$label)){
    names(check)[names(check)==paste0("V",b)] <- act_label$label[b]
  }
}
#ls(check)
df2 <- check

# 2) extracts only measurements on the mean and standard deviation

df_data1 <- df2[key_vars]


# 5) from the data, create a second, independent tidy data set with the average of each variable for each activity and each subject
# tidy data has:
# 1) each variable has a column
# 2) each observation is a row
# 3) each observation type has its own table. 
#
# Here, we only have one type of data, the measurements. If we had subject-specific data, that would
# be it's own table.

# drop the standard deviations, and keep only the averages
df_data1 <-df_data1[c("y",mean_vars)]
temp_df <- df_data1
#ls(temp_df)
#temp_df <- temp_df[c("y","tBodyGyro-mean()-X","tBodyGyro-mean()-Y")]
#ls(temp_df)


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

#5) From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

df_tidy <- df_tidy %>%
  rename_with(~ str_replace(., "-mean\\(\\)$", ""))
  


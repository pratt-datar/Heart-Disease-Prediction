############################
# Data_Import_Clean
############################

# preamble

# main source code
### Imports
library(tidyverse)
#library(Hmisc)
library(corrplot)
### Data read
df_hun <- read.table("processed.hungarian.data",sep=",")
### substituting unknown values with NAs and dropping unused level
df_hun[df_hun == "?"] <- NA
df_hun <- droplevels(df_hun)
# rename column names 
colnames(df_hun) <- c("age"
                      ,"sex"
                      ,"cp"
                      ,"trestbps"
                      ,"chol"
                      ,"fbs"
                      ,"restecg"
                      ,"thalach"
                      ,"exang"
                      ,"oldpeak"
                      ,"slope"
                      ,"ca"
                     ,"thal"
                     , "num")
# changing variable data types
df_hun$num <- as.factor(df_hun$num)
df_hun$trestbps <- as.integer(df_hun$trestbps)
df_hun$chol <- as.integer(df_hun$chol)
df_hun$thalach <- as.integer(df_hun$thalach)
df_hun$oldpeak <- as.integer(df_hun$oldpeak)
### Descriptive statistics
head(df_hun,3)
### finding colums with more than 50% missing values
na_col = vector()
for (i in colnames(df_hun)){
    
    if (length(which(is.na(df_hun[,i]))) > (dim(df_hun)[1]/2))
        {
        na_col <- append(na_col,i)
    }
    
}
na_col
### removing mostly NA columns
df_hun <- df_hun %>% select(-na_col)
### fixing NA values in other columns
# removing 1 common NA value for trestbps, oldpeak, and thalach columns
delete_list_tte <- which(is.na(df_hun$trestbps) & is.na(df_hun$exang) & is.na(df_hun$thalach))
df_hun <- df_hun[-delete_list_tte,]
# removing 9 factor missing values for fbs and restecg
delete_list_fr <- which(is.na(df_hun$fb) | is.na(df_hun$restecg))
df_hun <- df_hun[-delete_list_fr,]
# replacing 23 missing values in chol with mean values
df_hun <- df_hun %>% mutate(chol = ifelse(is.na(chol),round(mean(chol, na.rm = TRUE),0),chol))
summary(df_hun)

# postamble
port_7e9564b13dfc4f61a47f6ec9af6ad7d1 <- df_hun


############################
# Data_Model
############################

# preamble
df_hun <- port_7e9564b13dfc4f61a47f6ec9af6ad7d1

# main source code
### Imports
library(tidyverse)
library(caret)
colnames(df_hun)
### Splitting data into train and test dataset
set.seed(100) # set the seed for random number
index <- sample(1:6961, 6961) # generate a list of random number
# separate the data into train and test set
trainList <- createDataPartition(index,p=.8,list=FALSE)
train <- df_hun[trainList, ]
test <- df_hun[-trainList, ]
### Logistic model (AGE + SEX + TRESTBPS)
model1 <- glm(num ~ age + sex + trestbps,
              family=binomial,
              data=train)
summary(model1)
pred1 <- predict(model1, newdata = test, type = "response")
y_pred_num <- ifelse(pred1 > 0.5, 1, 0) 
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$num
# Confusion Matrix
table(y_pred, y_act)
# accuracy rate
mean(y_pred == y_act)

# postamble



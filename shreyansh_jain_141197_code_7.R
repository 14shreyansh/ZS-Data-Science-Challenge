#Import complete data set

data<-read.csv('data.csv')

#Divide data into training data and test data by considering
#only NA values of is_goal for test data and non NA values for training data

train_data<-subset(data,(!is.na(data$is_goal)))
test_data<-subset(data,(is.na(data$is_goal)))


# Data Preprocessing on training data set

#Indexing from 0 to 1
train_data$Column1<-train_data$Column1 + 1

#After analysing data, removing few features that do not create impact on is_goal
train_data<-train_data[,-c(8,15,16,22,23,25,26,27)]

#Filling missing values for home.away variable and giving 1 to home and 0 to away
#if vs. or lat is 42.98293 then it is home match otherwise it is away match
train_data$home.away<-ifelse(grepl(42.982923,train_data$lat.lng) | grepl("vs",train_data$home.away),1,0)

#Filling missing value of remaining_min variable
#giving mean value to the missing values
train_data$remaining_min[is.na(train_data$remaining_min)]<-round(mean(train_data$remaining_min,na.rm = TRUE))

#Filling missing value of remaining_min2 variable
#giving mean value to the missing values
train_data$remaining_min2[is.na(train_data$remaining_min2)]<-round(mean(train_data$remaining_min2,na.rm = TRUE))

#Filling missing value of remaining_sec variable
#giving mean value to the missing values
train_data$remaining_sec[is.na(train_data$remaining_sec)]<-round(mean(train_data$remaining_sec,na.rm = TRUE))

#Filling missing value of distance_of_shot6 variable
#giving mean value to the missing values
train_data$distance_of_shot6[is.na(train_data$distance_of_shot6)]<-round(mean(train_data$distance_of_shot6,na.rm = TRUE))

#Filling missing value of distance_of_shot variable
#giving mean value to the missing values
train_data$distance_of_shot[is.na(train_data$distance_of_shot)]<-round(mean(train_data$distance_of_shot,na.rm = TRUE))

#Filling missing value of power_of_shot variable
#giving mean value to the missing values
train_data$power_of_shot[is.na(train_data$power_of_shot)]<-round(mean(train_data$power_of_shot,na.rm = TRUE))

#function to calculate mode of a variable
mode <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x,ux)); ux[tab == max(tab)]
}

#find mode of knockout_match and assign it to missing values of this column
mode(train_data$knockout_match)
train_data$knockout_match[is.na(train_data$knockout_match)]<-0

#assigning NA to all blank spaces
train_data[train_data==""]<-NA

#find mode of area_of_shot and assign it to missing values of this column
mode(train_data$area_of_shot)
train_data$area_of_shot[is.na(train_data$area_of_shot)]<-"Center(C)"


#find mode of shot_basics and assign it to missing values of this column
mode(train_data$shot_basics)
train_data$shot_basics[is.na(train_data$shot_basics)]<-"Mid Range"

#find mode of range_of_shot and assign it to missing values of this column
mode(train_data$range_of_shot)
train_data$range_of_shot[is.na(train_data$range_of_shot)]<-"Less Than 8 ft."

#find mode of type_of_shot and assign it to missing values of this column
mode(train_data$type_of_shot)
train_data$type_of_shot[is.na(train_data$type_of_shot)]<-"shot - 39"

#find mode of area_of_combined_shot and assign it to missing values of this column
mode(train_data$type_of_combined_shot)
train_data$type_of_combined_shot[is.na(train_data$type_of_combined_shot)]<-"shot - 3"

#converting variables to numeric data type for calculating correlation matrix
train_data$area_of_shot<-as.numeric(train_data$area_of_shot)

train_data$is_goal<-as.numeric(train_data$is_goal)

train_data$shot_basics<-as.numeric(train_data$shot_basics)

train_data$range_of_shot<-as.numeric(train_data$range_of_shot)

train_data$type_of_combined_shot<-as.numeric(train_data$type_of_combined_shot)

train_data$type_of_shot<-as.numeric(train_data$type_of_shot)

#find mode of location_x and assign it to missing values of this column
mode(train_data$location_x)
train_data$location_x[is.na(train_data$location_x)]<-0

#find mode of location_y and assign it to missing values of this column
mode(train_data$location_y)
train_data$location_y[is.na(train_data$location_y)]<-0

#removing match_event_id and shot_id_number.
train_data<-train_data[,-c(2,15)]

#column1 replaces shot_id_number
names(train_data)[names(train_data)=="Column1"] <- "shot_id_number"

#round remaining_min2 and distance_of_shot6
train_data$remaining_min2<- round(train_data$remaining_min2)
train_data$distance_of_shot6<- round(train_data$distance_of_shot6)

#seperating latitute and longitute 
train_data<-within(train_data,{
  lat<-substr(train_data$lat.lng,1,9)
  lon<-substr(train_data$lat.lng,11,22)
  })

#converting them to numeric and removing combined column
train_data$lon<-as.numeric(train_data$lon)
train_data$lat<-as.numeric(train_data$lat)
train_data<-train_data[,-14]

#replacing missing values with latest value
library(zoo)
train_data$lon<- na.locf(train_data$lon)
train_data$lat<- na.locf(train_data$lat)


#Same data preprocessing on test dataset

test_data$Column1<-test_data$Column1 + 1

test_data<-test_data[,-c(8,15,16,22,23,25,26,27)]

test_data$home.away<-ifelse(grepl(42.982923,test_data$lat.lng) | grepl("vs",test_data$home.away),1,0)

test_data$remaining_min[is.na(test_data$remaining_min)]<-round(mean(test_data$remaining_min,na.rm = TRUE))

test_data$remaining_min2[is.na(test_data$remaining_min2)]<-round(mean(test_data$remaining_min2,na.rm = TRUE))

test_data$remaining_sec[is.na(test_data$remaining_sec)]<-round(mean(test_data$remaining_sec,na.rm = TRUE))

test_data$distance_of_shot[is.na(test_data$distance_of_shot)]<-round(mean(test_data$distance_of_shot,na.rm = TRUE))

test_data$distance_of_shot6[is.na(test_data$distance_of_shot6)]<-round(mean(test_data$distance_of_shot6,na.rm = TRUE))

test_data$power_of_shot[is.na(test_data$power_of_shot)]<-round(mean(test_data$power_of_shot,na.rm = TRUE))

mode(test_data$knockout_match)

test_data$knockout_match[is.na(test_data$knockout_match)]<-0

test_data[test_data==""]<-NA

mode(test_data$area_of_shot)

test_data$area_of_shot[is.na(test_data$area_of_shot)]<-"Center(C)"

mode(test_data$shot_basics)
test_data$shot_basics[is.na(test_data$shot_basics)]<-"Mid Range"

mode(test_data$range_of_shot)
test_data$range_of_shot[is.na(test_data$range_of_shot)]<-"Less Than 8 ft."

mode(test_data$type_of_shot)
test_data$type_of_shot[is.na(test_data$type_of_shot)]<-"shot - 39"

mode(test_data$type_of_combined_shot)
test_data$type_of_combined_shot[is.na(test_data$type_of_combined_shot)]<-"shot - 3"

test_data$area_of_shot<-as.numeric(test_data$area_of_shot)

test_data$is_goal<-as.numeric(test_data$is_goal)

test_data$shot_basics<-as.numeric(test_data$shot_basics)

test_data$range_of_shot<-as.numeric(test_data$range_of_shot)

test_data$type_of_combined_shot<-as.numeric(test_data$type_of_combined_shot)

test_data$type_of_shot<-as.numeric(test_data$type_of_shot)

mode(test_data$location_x)
test_data$location_x[is.na(test_data$location_x)]<-0

mode(test_data$location_y)
test_data$location_y[is.na(test_data$location_y)]<-0

test_data<-test_data[,-c(2,15)]

names(test_data)[names(test_data)=="Column1"] <- "shot_id_number"

test_data$remaining_min2<- round(test_data$remaining_min2)
test_data$distance_of_shot6<- round(test_data$distance_of_shot6)

test_data<-within(test_data,{
  lat<-substr(test_data$lat.lng,1,9)
  lon<-substr(test_data$lat.lng,11,22)
})

test_data$lon<-as.numeric(test_data$lon)
test_data$lat<-as.numeric(test_data$lat)
test_data<-test_data[,-14]

library(zoo)
test_data$lon<- na.locf(test_data$lon)
test_data$lat<- na.locf(test_data$lat)

#Scale data to fit for PCA
train_data[,-c(1,9)]<-scale(train_data[,-c(1,9)])
test_data[,-c(1,9)]<-scale(test_data[,-c(1,9)])

#Applying PCA for dimension reduction
library(caret)
library(e1071)
pca = preProcess(x = train_data[,-c(1,9)], method = 'pca', pcaComp = 2)
training_set = predict(pca, train_data)
test_set = predict(pca, test_data)

#Applying Generalized Linear Model on Training Set
logitmod<- glm(is_goal~PC1 + PC2,
               family ="binomial",data=training_set)

summary(logitmod)

#Predicting data for Test_set
pred<- predict(logitmod, newdata = test_set, type = "response")
prediction<-as.data.frame(pred)

#Converting result in desired format and saving it in CSV File

write.csv(prediction,"shreyansh_jain_pca.csv")
sample<-read.csv('sample_submission.csv')
prediction<-read.csv('shreyansh_jain_pca.csv')
names(prediction)[names(prediction)=="X"] <- "shot_id_number"

final<-merge(sample,prediction,by="shot_id_number")
final<-final[,-c(2)]
names(final)[names(final)=="pred"] <- "is_goal"

write.csv(final,"shreyansh_jain_141197_prediction_6.csv")
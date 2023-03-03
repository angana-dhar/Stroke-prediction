#import the library 
library(dplyr)
library("tidyverse")
library("ggplot2")
library("gridExtra")
library("fastdummies")
library("gplots")
library("gains")
library("caret")
library("ROCR")
library("randomForest")
library("rpart")
library("rpart.plot")
library("caret")
library("e1071")
library("ranger")
library("DMwR2")
library("smotefamily")
library("ROSE")
library("pROC")


#head of stroke
head(healthcare_dataset_stroke_data)

#Names of dataset
names(healthcare_dataset_stroke_data)


#dropping the id column
healthcare_dataset_stroke_data<-subset(healthcare_dataset_stroke_data,select=(-id))
head(healthcare_dataset_stroke_data)



healthcare_dataset_stroke_data <- healthcare_dataset_stroke_data%>%filter(gender!="other")


#EDA 
#View of dataset
View(healthcare_dataset_stroke_data)


#Checking the structure of dataset 
str(healthcare_dataset_stroke_data)

#BMI has NA values 
healthcare_dataset_stroke_data$bmi<-as.integer(healthcare_dataset_stroke_data$bmi)

#Changing the datatype of the gender, ever_married, Smoking status to factors 
healthcare_dataset_stroke_data$gender<-as.factor(healthcare_dataset_stroke_data$gender)
healthcare_dataset_stroke_data$ever_married<-as.factor(healthcare_dataset_stroke_data$ever_married)
healthcare_dataset_stroke_data$work_type<-as.factor(healthcare_dataset_stroke_data$work_type)
healthcare_dataset_stroke_data$Residence_type<-as.factor(healthcare_dataset_stroke_data$Residence_type)
healthcare_dataset_stroke_data$smoking_status<-as.factor(healthcare_dataset_stroke_data$smoking_status)
healthcare_dataset_stroke_data$stroke<-as.factor(healthcare_dataset_stroke_data$stroke)
healthcare_dataset_stroke_data$heart_disease<-as.factor(healthcare_dataset_stroke_data$heart_disease)
healthcare_dataset_stroke_data$hypertension<-as.factor(healthcare_dataset_stroke_data$hypertension)

#checking if there is any null value in the dataset 
any(is.na(healthcare_dataset_stroke_data))

View(healthcare_dataset_stroke_data)

#number of na's in bmi 
length(which(is.na(healthcare_dataset_stroke_data$bmi)))


#Doing median imputation for fixing null value
healthcare_dataset_stroke_data_1<-healthcare_dataset_stroke_data
healthcare_dataset_stroke_data_mean <- healthcare_dataset_stroke_data
nrow(healthcare_dataset_stroke_data_mean)
#removingthe missing values of the bmi
healthcare_dataset_stroke_data2<-na.omit(healthcare_dataset_stroke_data_1)

#Checking for na after the values are dropped
any(is.na(healthcare_dataset_stroke_data2))
nrow(healthcare_dataset_stroke_data2)


View()


#calculating the median
med<-median(healthcare_dataset_stroke_data2$bmi);med
Mean<-mean(healthcare_dataset_stroke_data2$bmi);Mean

#imputing the mean value 
healthcare_dataset_stroke_data_mean$bmi[is.na(healthcare_dataset_stroke_data_mean$bmi)]<-Mean

#Imputing the median value 
healthcare_dataset_stroke_data_1$bmi[is.na(healthcare_dataset_stroke_data_1$bmi)]<-med

#checking the value for bmi 
any(is.na(healthcare_dataset_stroke_data_1$bmi))


#Checking the structure of the dataframe
str(healthcare_dataset_stroke_data_1)

unique(healthcare_dataset_stroke_data_1$work_type)


#healthcare_dataset_stroke_data_1 => median imputed value 
#healthcare_dataset_stroke_data2 => missing values dropped 

#Performing the Exploratory Data Analysis
#Creating a bar plot fot gender VS stroke
ggplot(healthcare_dataset_stroke_data_1,aes(x=gender,fill=stroke))+geom_bar(position="dodge")

#creating a bar plot for hypertension vs stroke
ggplot(healthcare_dataset_stroke_data_1,aes(x=hypertension,fill=stroke))+geom_bar(position="dodge")


#Creating the bar plot for heart disease
ggplot(healthcare_dataset_stroke_data_1,aes(x=heart_disease,fill=stroke))+geom_bar(position="dodge")

#Creating the bar plot for worktype 
ggplot(healthcare_dataset_stroke_data_1,aes(x=work_type,fill=stroke))+geom_bar(position="dodge")


#Creating a barplot for the marital status of the people
ggplot(healthcare_dataset_stroke_data_1,aes(x=ever_married,fill=stroke))+geom_bar(position="dodge")



#Creating a bar plot for the smoking status and the stroke 
ggplot(healthcare_dataset_stroke_data_1,aes(x=smoking_status,fill=stroke))+geom_bar(position = "dodge")

#So now comparing the within the population of people who got stroke 

library("dplyr")


#Comparing the proportions within the gender among the people who got stroke

data_proportion_gender<-healthcare_dataset_stroke_data_1 %>% group_by(gender) %>% summarise(prop=sum(stroke==1)/length(gender))

G1<-ggplot(data_proportion_gender,aes(x=gender,y=prop,fill=gender))+geom_col()
G1


#comparing the proportions within the different married type who got stroke 
data_proportion_married<- healthcare_dataset_stroke_data_1%>%
  group_by(ever_married)%>%
  summarise(prop=sum(stroke==1)/length(ever_married))
G2<-ggplot(data_proportion_married,aes(x=ever_married,y=prop,fill=ever_married))+geom_col()
G2

#comparing the proportion of people who is having heart disease or not
data_proportion_heart_disease<-healthcare_dataset_stroke_data_1%>%
  group_by(heart_disease)%>%
  summarise(prop=sum(stroke==1)/length(heart_disease))
G3<-ggplot(data_proportion_heart_disease,aes(x=heart_disease,y=prop,fill=heart_disease))+geom_col()
G3



#Comparing the proportions of people who have hypertension  who got stroke 
data_proportion_hypertension<- healthcare_dataset_stroke_data_1%>%
  group_by(hypertension)%>%
  summarise(prop=sum(stroke==1)/length(hypertension))

G4<-ggplot(data_proportion_hypertension,aes(x=hypertension,y=prop,fill=hypertension))+geom_col()
G4




#Comparing the proportions of people who have different worktype who got a stroke 
data_proportion_work_type<- healthcare_dataset_stroke_data_1%>%
  group_by(work_type)%>%
  summarise(prop=sum(stroke==1)/length(work_type))

G5<-ggplot(data_proportion_work_type,aes(x=work_type,y=prop,fill=work_type))+geom_col()
G5



#Comparing the proportion of people who have different resident type 
data_proportion_Residence_type<- healthcare_dataset_stroke_data_1%>%
  group_by(Residence_type)%>%
  summarise(prop=sum(stroke==1)/length(Residence_type))

G6<-ggplot(data_proportion_Residence_type,aes(x=Residence_type,y=prop,fill=Residence_type))+geom_col()
G6

#comparing people with different smoking status type who got stroke 

data_proportion_smoking_status<- healthcare_dataset_stroke_data_1%>%
  group_by(smoking_status)%>%
  summarise(prop=sum(stroke==1)/length(smoking_status))


G7<-ggplot(data_proportion_smoking_status,aes(x=smoking_status,y=prop,fill=smoking_status))+geom_col()
G7


#Plotting together in the form of a grid 
grid.arrange(grobs=list(G1,G2,G3,G4,G5,G6,G7),ncol=3, top = "Proportion of Strokes for Each Factor")



#Gender and the residence type does not have much difference in occurrence of strokes. Those with hypertension, heart diseases and those who have been married have higher proportion of strokes 

#Children and the people who have never worked have very low occurrence of stroke. People who are self_employed have a greater proportion of getting the stroke.

#People who are currently smoking also has a greater percentage of getting the stroke when compared to the rest of the population

#Removing other from the dataframe 
#healthcare_dataset_stroke_data_1<- healthcare_dataset_stroke_data_1%>%
#  filter(gender!="Other")

#healthcare_dataset_stroke_data2<- healthcare_dataset_stroke_data2%>%
#  filter(gender!="Other")

#healthcare_dataset_stroke_data_mean<- healthcare_dataset_stroke_data__mean%>%
#  filter(gender!="Other")



#Comparing the boxplot for different factors  with age 
B1<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=gender,y=age,color=stroke))+geom_boxplot()
B1
B2<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=hypertension,y=age,color=stroke))+geom_boxplot()
B2

B3<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=ever_married,y=age,color=stroke))+geom_boxplot()
B3

B4<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=heart_disease,y=age,color=stroke))+geom_boxplot()
B4

B5<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=smoking_status,y=age,color=stroke))+geom_boxplot()
B5

B6<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=work_type,y=age,color=stroke))+geom_boxplot()
B6

B7<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=Residence_type,y=age,color=stroke))+geom_boxplot()
B7
#plotting the boxplots in the form of a grid 
grid.arrange(grobs=list(B1,B2,B3,B4,B5,B6,B7),ncol=3,top="Box plots for Stroke and Age across various factors")

#From the above plot we can infer that most of the people who got stroke are the people who are older
#people who are self employed are also older than rest of the population 
#People who had stroke and smokes are younger than those who never smoked

#Comparing the strokes for glucose level across various factors 
Q1<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=gender,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q1

Q2<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=hypertension,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q2

Q3<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=ever_married,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q3

Q4<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=heart_disease,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q4

Q5<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=smoking_status,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q5

Q6<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=work_type,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q6

Q7<-healthcare_dataset_stroke_data_1%>%
  ggplot(aes(x=Residence_type,y=avg_glucose_level,color=stroke))+geom_boxplot()
Q7

#plotting the boxplots in the form of a grid 
grid.arrange(grobs=list(Q1,Q2,Q3,Q4,Q5,Q6,Q7),ncol=3,top="Box plots for Average Glucose level across various factors")


# We can see that the glucose level is right skewed 
#The Inter Quartile Range is higher for people who gets stroke 

#Comparing the strokes for BMI across various factors 
W1<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=gender,y=bmi,color=stroke))+geom_boxplot()
W1

W2<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=hypertension,y=bmi,color=stroke))+geom_boxplot()
W2

W3<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=ever_married,y=bmi,color=stroke))+geom_boxplot()
W3

W4<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=heart_disease,y=bmi,color=stroke))+geom_boxplot()
W4

W5<-healthcare_dataset_stroke_data_1%>%
  ggplot(aes(x=smoking_status,y=bmi,color=stroke))+geom_boxplot()
W5

W6<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=work_type,y=bmi,color=stroke))+geom_boxplot()
W6

W7<-healthcare_dataset_stroke_data_1 %>%
  ggplot(aes(x=Residence_type,y=bmi,color=stroke))+geom_boxplot()
W7
#Creating the grid for all the plots 
grid.arrange(grobs=list(W1,W2,W3,W4,W5,W6,W7),ncol=3,top="Boxplots for Stroke and BMI accross various factors")
#There is no much difference between the person who got stroke and person who didn't get stroke. Even the BMI is right skewed.

#plotting the density plot and the histogram for the continuous parameters
E1<- ggplot(healthcare_dataset_stroke_data_1,aes(x=age,fill=stroke))+geom_density(alpha=0.5)
E1 
E2<- ggplot(healthcare_dataset_stroke_data_1,aes(x=avg_glucose_level,fill=stroke))+geom_density(alpha=0.5)
E2
E3<- ggplot(healthcare_dataset_stroke_data_1,aes(x=bmi,fill=stroke))+geom_density(alpha=0.5)
E3
E4<-ggplot(healthcare_dataset_stroke_data_1,aes(x=age,fill=stroke))+geom_histogram()
E4
E5<-ggplot(healthcare_dataset_stroke_data_1,aes(x=avg_glucose_level,fill=stroke))+geom_histogram()
E5
E6<-ggplot(healthcare_dataset_stroke_data_1,aes(x=bmi,fill=stroke))+geom_histogram()
E6

grid.arrange(grobs=list(E1,E2,E3,E4,E5,E6),nrow=3, top=" Distribution of continuous variable")

#In case we need to do regression in the future, we should set multi-level features into dummy variable and save it in a dataframe.
#For multi-level variables, get dummy
stroke_dummy <- dummy_cols(healthcare_dataset_stroke_data_1,select_columns = c("gender","work_type","smoking_status"),remove_first_dummy = TRUE, remove_selected_columns = TRUE)
stroke_dummy %>% head()

#For continuous variables, we could also use scatter plot to display their relationship.
cont.plot <- ggplot(data = healthcare_dataset_stroke_data_1, aes(x= age, y = bmi, color = stroke))+geom_point()
cont.plot
#As we can see, stroke patients are more likely to appear at older people and people with high bmi.

# Logistic Regression For Prediction
#From the EDA, we finds out that no certain feature has a strong correlation with stroke. To quantify how a feature affect the outcome, we need to conduct a regression. Since stroke is a binary variable, it’s better to use logistic regression.

#Dataset Building and Model Training
#First, we need to build the training dataset. We are going to use the stroke_dummy for regression. To test the regression result, we split the dataset into training set (70%) and testing set (30%).

# Training set creation
Training <- createDataPartition(y = stroke_dummy$stroke , p = 0.7, list = FALSE)
training <- stroke_dummy[Training,]
testing <- stroke_dummy[-Training,]

#We could examine the dimension of the training set and the testing set
dim(training)

dim(testing)


model <- glm(stroke ~.,family=binomial(link='logit'), data=training)
summary(model)

#From the regression result, we could conclude that:

#Age, hypertension and work_type_self-employed are statiscally significant in the regression results, with p-value smaller than 0.005. Age has the lowest p-value.
#Age and hypertension are postively correlated with stroke, which corresponds to our observation in EDA.
#Work_type_Self-employed is negatively correlated with stroke. It means that becoming self-employed would reduce the risk of getting stroke. Probably self-employed people would better enjoy the life and have a healthy lifestyle.


#Now we can run the anova() function on the model to see the deviance of the regression model.
anova(model, test="Chisq")

#predict the stroke variable based on the testing dataset
model.prob = predict(model, testing, type="response")

#predict the stroke variable based on the testing dataset
model.prob = predict(model, testing, type="response")

#To visualize the prediction result, we could use the confusionMatrix function from package caret. We set the prediction result to be 0.5, indicating that if the predicted stroke is larger than 0.5, we believe that this patient are likely to get stroke.
# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(as.numeric(model.prob>0.5)), reference = as.factor(testing$stroke))

#From the confusion matrix report, we could find the accuracy to be 0.95, which is relatively high. However, if we take a closer look, we would find that most prediction would just predict the outcome to be 0. It’s because th outcome ‘stroke’ is so imbalanced that the model believes that almost all the outcomes are 0. It’s hard to distinguish the one with the stroke and the one without it.


#What if we change the threshold? We found a helpful solution on the StacksOverflow(https://stats.stackexchange.com/questions/199978/optimizing-probability-thresholds-in-a-glm-model-in-caret) to plot the relationship between threshold and accuracy.
threhold <- seq(0.3,0.6,0.01)
accuracy <- NULL
for (i in seq(along = threhold)){
  prediction <- ifelse(model$fitted.values >= threhold[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(training$stroke ==prediction))/length(prediction)*100)
}
plot(threhold, accuracy, pch =19,type='b',col= "steelblue",main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


#The result is not pleasing. We cannot raise the accuracy just by changing the cutoff threshold.

#Maybe a different model? We could do a random forest classifier to test the outcome.

stroke.rf <- randomForest(stroke ~ ., data = healthcare_dataset_stroke_data_1, importance = TRUE,proximity = TRUE)
print(stroke.rf)

#For the random forest classifier, it reaches an accuracy of 95.03%, but the confusion matrix still looks alike with the logistic regression. So the imbalance of the dataset would greatly affect the prediction result.




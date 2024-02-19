# Stroke Prediction Project

## Overview

This project aims to predict stroke occurrences using advanced data analysis techniques and machine learning algorithms. It leverages the R programming language and various libraries for data manipulation, visualization, and model building.

## Libraries Used

- dplyr
- tidyverse
- ggplot2
- gridExtra
- fastdummies
- gplots
- gains
- caret
- ROCR
- randomForest
- rpart
- rpart.plot
- e1071
- ranger
- DMwR2
- smotefamily
- ROSE
- pROC

## Exploratory Data Analysis (EDA)

The project starts with exploratory data analysis, examining various factors such as gender, age, hypertension, heart disease, smoking status, work type, and residence type in relation to stroke occurrences.

## Data Preprocessing

Data preprocessing involves handling missing values, converting data types, and preparing the dataset for modeling. Techniques such as median imputation and dummy variable creation are utilized.

## Model Building

Two main models are built for stroke prediction:
1. Logistic Regression: Utilizes binary classification to predict stroke occurrences based on various features.
2. Random Forest Classifier: Implements an ensemble learning method for predicting stroke occurrences.

## Evaluation

Model performance is evaluated using confusion matrices and accuracy metrics. Additionally, the impact of changing threshold levels on model accuracy is explored.

## Results

Both logistic regression and random forest classifier models achieve high accuracy rates. However, the imbalance in the dataset affects prediction outcomes, leading to challenges in distinguishing between positive and negative stroke occurrences.


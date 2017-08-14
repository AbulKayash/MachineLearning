Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior.

The goal of your project is to predict the manner in which they did the exercise.

Load The Libraries
------------------

``` r
library(knitr)
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(rpart)
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(ggplot2)
library(rattle)
```

    ## Rattle: A free graphical interface for data mining with R.
    ## Version 4.1.0 Copyright (c) 2006-2015 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

Load & CLean Data
-----------------

``` r
trainData <- read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!",""), header=TRUE)
benchMark <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""), header=TRUE)

# remove id column as they are unique for each row thus not a useful predictor
trainData <- trainData [,-1] 
benchMark <- benchMark [,-1]


# remove null columns
trainData <- trainData[, unlist(lapply(trainData, function(x) !any(is.na(x))))]
benchMark <- benchMark[, unlist(lapply(benchMark, function(x) !any(is.na(x))))]

dim (trainData)
```

    ## [1] 19622    59

``` r
dim (benchMark)
```

    ## [1] 20 59

Prepare Training & Partion Set
------------------------------

``` r
# Set seed to make results reproducible
set.seed(5315)

inTrain <- createDataPartition(y=trainData$classe , p=0.7, list=FALSE)
training <- trainData[inTrain,]
testing <- trainData[-inTrain,]

dim(testing)
```

    ## [1] 5885   59

``` r
dim(training)
```

    ## [1] 13737    59

Build Prediction Model
----------------------

``` r
#Build 
controlDTree <- trainControl(method="cv", 5)
modelDTree <- train(classe ~ ., data=training, method="rf", trControl=controlDTree, ntree=150)

#Test the model.
confusionMatrix(predict(modelDTree, testing), testing$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1674    2    0    0    0
    ##          B    0 1137    3    0    0
    ##          C    0    0 1022    0    0
    ##          D    0    0    1  963    0
    ##          E    0    0    0    1 1082
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9988          
    ##                  95% CI : (0.9976, 0.9995)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9985          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9982   0.9961   0.9990   1.0000
    ## Specificity            0.9995   0.9994   1.0000   0.9998   0.9998
    ## Pos Pred Value         0.9988   0.9974   1.0000   0.9990   0.9991
    ## Neg Pred Value         1.0000   0.9996   0.9992   0.9998   1.0000
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.2845   0.1932   0.1737   0.1636   0.1839
    ## Detection Prevalence   0.2848   0.1937   0.1737   0.1638   0.1840
    ## Balanced Accuracy      0.9998   0.9988   0.9981   0.9994   0.9999

``` r
#Very high accuracy rate 99.88%, thus we have a very good model to predict the outcome of the training set.    

predict(modelDTree, benchMark)
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

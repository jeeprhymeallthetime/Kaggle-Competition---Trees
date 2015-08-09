library(randomForest)
library(caret)
library(rpart)
library(e1071)
setwd("~/GitHub/trees")
tree <- read.csv(file="train.csv", head=TRUE, sep=",")
tree$Cover_Type <- factor(tree$Cover_Type)
set.seed(1354)
index <- createDataPartition(tree$Cover_Type,p=.5,list=FALSE)
training <- tree[index, ]
testing <- tree[-index, ]
fol <- formula(Cover_Type ~  Elevation + Horizontal_Distance_To_Roadways + Horizontal_Distance_To_Fire_Points + Wilderness_Area1 + Wilderness_Area2 + Wilderness_Area3 + Wilderness_Area4 + Soil_Type1 + Soil_Type2 + Soil_Type3 + Soil_Type4 + Soil_Type5 + Soil_Type6 + Soil_Type7 + Soil_Type8 + Soil_Type9 + Soil_Type10 + Soil_Type11 + Soil_Type12 + Soil_Type13 + Soil_Type14 + Soil_Type15 + Soil_Type16 + Soil_Type17 + Soil_Type18 + Soil_Type19 + Soil_Type20 + Soil_Type21 + Soil_Type22 + Soil_Type23 + Soil_Type24 + Soil_Type25 + Soil_Type26 + Soil_Type27 + Soil_Type28 + Soil_Type29 + Soil_Type30 + Soil_Type31 + Soil_Type32 + Soil_Type33 + Soil_Type34 + Soil_Type35 + Soil_Type36 + Soil_Type37 + Soil_Type38 + Soil_Type39 + Soil_Type40)
accf <- pdf==testing$Cover_Type
summary(accf)
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))
modelf <- randomForest(fol, data=training)
importance(modelf)

###Determined which of the most important predicting variables are.  They are now isolated and used for the next model


fol <- formula(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Wilderness_Area4 + Soil_Type3 + Soil_Type10 + Soil_Type38 + Soil_Type39)
model <- rpart(fol, method="class", data=training)
modelf <- randomForest(fol, data=training)
models <- svm(fol, data=training)
pd <- predict(model, testing, type="class")
pdf <- predict(modelf, testing, type="class")
pds <- predict(models, testing, type="class")
acc <- pd==testing$Cover_Type
accf <- pdf==testing$Cover_Type
accs <- pds==testing$Cover_Type
summary(acc)
as.numeric(summary(acc)[3])/(as.numeric(summary(acc)[3])+as.numeric(summary(acc)[2]))
summary(accf)
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))
summary(accs)
as.numeric(summary(accs)[3])/(as.numeric(summary(accs)[3])+as.numeric(summary(accs)[2]))
importance(modelf)


#After this modeling was completed, the soil types were relabeled.  
#Instead of 40 variables that were boolean, they were all transformed into one variable from 1-40.
#Similarily, Wilderness Area was changed from 4 booleans to one predictor with 4 variables
#relabel.py performs the relabeling, runR.R produces the models 






library(randomForest)
library(caret)
library(rpart)
library(e1071)
set.seed(1354)
setwd("~/GitHub/trees")


tree <- read.csv(file="relabeltrain.csv", head=TRUE, sep=",")
tree$Cover_Type <- factor(tree$Cover_Type)
tree$Soil_Type <- factor(tree$Soil_Type)
tree$Wilderness_Area <- factor(tree$Wilderness_Area)
index <- createDataPartition(tree$Cover_Type,p=.8,list=FALSE)
training <- tree[index, ]
testing <- tree[-index, ]

fol <- formula(Cover_Type ~ Elevation + Aspect + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Wilderness_Area + Soil_Type)
modelf <- randomForest(fol, data=training)
pdf <- predict(modelf, testing, type="response")
accf <- pdf==testing$Cover_Type
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))

fol <- formula(Cover_Type ~ Elevation + Aspect + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +Hillshade_9am + Horizontal_Distance_To_Fire_Points + Wilderness_Area + Soil_Type)
modelf <- randomForest(fol, data=training)
pdf <- predict(modelf, testing, type="class")
accf <- pdf==testing$Cover_Type
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))

fol <- formula(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Wilderness_Area + Soil_Type)
modelf <- randomForest(fol, data=training)
pdf <- predict(modelf, testing, type="response")
accf <- pdf==testing$Cover_Type
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))

fol <- formula(Cover_Type ~ Elevation + Aspect + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +Hillshade_9am + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Wilderness_Area + Soil_Type)
modelf <- randomForest(fol, data=training)
pdf <- predict(modelf, testing, type="response")
accf <- pdf==testing$Cover_Type
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))

fol <- formula(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points)
modelf <- randomForest(fol, data=training)
pdf <- predict(modelf, testing, type="response")
accf <- pdf==testing$Cover_Type
as.numeric(summary(accf)[3])/(as.numeric(summary(accf)[3])+as.numeric(summary(accf)[2]))

#Results here show that the best model fit here does not need to include either Wilderness Area of Soil Type
#However, when submitted to the Kaggle competition to test agains their own test set, the formula that included Soil Type and Wilderness Area 
#ended up being the most successful model


models <- svm(fol, data=training)
pds <- predict(models, testing)
accs <- pds==testing$Cover_Type
summary(accs)
as.numeric(summary(accs)[3])/(as.numeric(summary(accs)[3])+as.numeric(summary(accs)[2]))
importance(modelf)



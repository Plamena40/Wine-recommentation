library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyverse)
library(caret)
library(pROC)
library(ROCR)
library(ROSE)
library(ResourceSelection)
library(PresenceAbsence)
library(shinythemes)
library(bslib)
library(MLeval)

df <- read.table("data/wine.data", sep = ",", header = FALSE)
names(df) <- c("Class", "Alcohol", "Malicacid", "Ash", "Alcalinity_of_ash", 
               "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols",
               "Proanthocyanins", "Color_intensity", "Hue", "0D280_0D315_of_diluted_wines",
               "Proline")

df$Class <- factor(make.names(df$Class))
df$Wine <- ifelse(df$Class == "X1", "Barolo",
                  ifelse(df$Class == "X2", "Grignolino",
                         ifelse(df$Class == "X3", "Barbera", NA)))

df$Flavanoids_cat <- ifelse(df$Flavanoids > median(df$Flavanoids), 1, 0)
df$Color_intensity_cat <- ifelse(df$Color_intensity > median(df$Color_intensity), 1, 0)


set.seed(8918)
tuneGrid <- expand.grid(
  alpha = seq(0, 0.15, by = 0.05),      
  lambda = 0)

ctrl <- trainControl(
  method = "LOOCV", #number=50,
  classProbs = TRUE, 
  summaryFunction = multiClassSummary,
  verboseIter = TRUE, 
  savePredictions = TRUE, 
  returnResamp = "final")

mod1 <- train(
  Wine ~ Alcohol + Color_intensity_cat + Flavanoids_cat,
  data = df, 
  method = "glmnet",
  trControl = ctrl, 
  metric = "Accuracy",
  tuneGrid = tuneGrid,
  preProcess = c("center", "scale"))

save(mod1, file = "/Wine recommentation/mod1.RData")

evalm(mod1)
print(mod1)



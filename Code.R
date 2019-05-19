install.packages("gbm")
install.packages("here")
install.packages("forecast")


options(scipen=999, digits=6)
library(forecast)
library(here)
library(Lahman)
library(lubridate)
library(forecast)
library(dplyr)
library(tidyr)
library(zoo)
library(glmnet)
library(tseries)
library(rpart)

todays_date_formatted <- format(Sys.Date(), '%Y%m%d')
dir.create(here::here('output', todays_date_formatted), showWarnings = FALSE)


library(rpart.plot)
library(glmnet)
library(forecast)
library(MASS)
library(randomForest)
library(gbm)

data_url <- 'C:/Users/moshm/Desktop/Edinburgh_study/semester2/Industrial Analytics/Project_Industrial/data/sarah-gets-a-diamond-raw-data.csv'
raw_diamond_dat <- read.csv(data_url)

diamond <- raw_diamond_dat

diamond <- diamond %>%
  mutate(Clarity = as.factor(Clarity), 
         Dataset = as.factor(Dataset), 
         Cut = as.factor(ifelse(Cut == "Signature-Ideal",
                                "SignatureIdeal", 
                                as.character(Cut))),
         Cut = as.factor(ifelse(Cut == "Very Good", 
                                "VeryGood", 
                                as.character(Cut))),
         LPrice = log(Price),
         LCarat = log(Carat.Weight),
         recipCarat = 1 / Carat.Weight,
         Caratbelow1 = as.numeric(Carat.Weight < 1),
         Caratequal1 = as.numeric(Carat.Weight == 1),
         Caratbelow1.5 = as.numeric((Carat.Weight > 1) & (Carat.Weight < 1.5)),
         Caratequal1.5 = as.numeric(Carat.Weight == 1.5),
         Caratbelow2 = as.numeric((Carat.Weight > 1.5) & (Carat.Weight < 2)),
         Caratabove2 = as.numeric(Carat.Weight >= 2))

summary(diamond)

dummies <- model.matrix(~ 0 + Cut + Color + Clarity + Polish + Symmetry + Report + 
                          Cut:Color + Cut:Clarity + Cut:Polish + Cut:Symmetry + Cut:Report +
                          Color:Clarity + Color:Polish + Color:Symmetry + Color:Report+
                          Polish:Symmetry + Polish:Report + Symmetry:Report, 
                        data = diamond)

diamond.full <- as.data.frame(cbind(diamond, dummies))

diamond.train <- diamond[diamond$Dataset == "Train",]
diamond.test <- diamond[diamond$Dataset == "Test",]

diamond.full.train <- diamond.full[diamond.full$Dataset == "Train",]
diamond.full.test <- diamond.full[diamond.full$Dataset == "Test",]

nTrain <- dim(diamond.train)[1]
(nSmallTrain <- round(nrow(diamond.train) * 0.75))

(nValid <- nTrain - nSmallTrain)

rowIndicesSmallerTrain <- sample(1:nTrain, size = nSmallTrain, replace = FALSE)

diamond.smaller.train <- diamond.train[rowIndicesSmallerTrain, ]
diamond.validation <- diamond.train[-rowIndicesSmallerTrain, ]

diamond.full.smaller.train <- diamond.full.train[rowIndicesSmallerTrain, ]
diamond.full.validation <- diamond.full.train[-rowIndicesSmallerTrain, ]

#First Scatter Plot
plot(x=diamond$Carat.Weight, y=diamond$Price, 
     main="Price vs. Carat", 
     ylab="Price", xlab="Carat")

#Second Scatter Plot
plot(x=diamond$Carat.Weight, y=diamond$LPrice, 
     main="Log Price vs. Carat", 
     ylab="Log Price", xlab="Carat")

#Third Scatter Plot
plot(x=diamond$LCarat, y=diamond$LPrice, 
     main="Log Price vs. Log Carat", 
     ylab="Log Price", xlab="Log Carat")

lm_formula <- "LPrice ~ LCarat+recipCarat+Caratbelow1+Caratequal1+Caratbelow1.5+Caratequal1.5+Caratbelow2+Caratabove2+CutFair+CutGood+CutIdeal+CutSignatureIdeal+CutVeryGood+ColorE+ColorF+ColorG+ColorH+ColorI+ClarityIF+ClaritySI1+ClarityVS1+ClarityVS2+ClarityVVS1+ClarityVVS2+PolishG+PolishID+PolishVG+SymmetryG+SymmetryID+SymmetryVG+ReportGIA+CutGood:ColorE+CutIdeal:ColorE+CutSignatureIdeal:ColorE+CutVeryGood:ColorE+CutGood:ColorF+CutIdeal:ColorF+CutSignatureIdeal:ColorF+CutVeryGood:ColorF+CutGood:ColorG+CutIdeal:ColorG+CutSignatureIdeal:ColorG+CutVeryGood:ColorG+CutGood:ColorH+CutIdeal:ColorH+CutSignatureIdeal:ColorH+CutVeryGood:ColorH+CutGood:ColorI+CutIdeal:ColorI+CutSignatureIdeal:ColorI+CutVeryGood:ColorI+CutGood:ClarityIF+CutIdeal:ClarityIF+CutSignatureIdeal:ClarityIF+CutVeryGood:ClarityIF+CutGood:ClaritySI1+CutIdeal:ClaritySI1+CutSignatureIdeal:ClaritySI1+CutVeryGood:ClaritySI1+CutGood:ClarityVS1+CutIdeal:ClarityVS1+CutSignatureIdeal:ClarityVS1+CutVeryGood:ClarityVS1+CutGood:ClarityVS2+CutIdeal:ClarityVS2+CutSignatureIdeal:ClarityVS2+CutVeryGood:ClarityVS2+CutGood:ClarityVVS1+CutIdeal:ClarityVVS1+CutSignatureIdeal:ClarityVVS1+CutVeryGood:ClarityVVS1+CutGood:ClarityVVS2+CutIdeal:ClarityVVS2+CutSignatureIdeal:ClarityVVS2+CutVeryGood:ClarityVVS2+CutGood:PolishG+CutIdeal:PolishG+CutSignatureIdeal:PolishG+CutVeryGood:PolishG+CutGood:PolishID+CutIdeal:PolishID+CutSignatureIdeal:PolishID+CutVeryGood:PolishID+CutGood:PolishVG+CutIdeal:PolishVG+CutSignatureIdeal:PolishVG+CutVeryGood:PolishVG+CutGood:SymmetryG+CutIdeal:SymmetryG+CutSignatureIdeal:SymmetryG+CutVeryGood:SymmetryG+CutGood:SymmetryID+CutIdeal:SymmetryID+CutSignatureIdeal:SymmetryID+CutVeryGood:SymmetryID+CutGood:SymmetryVG+CutIdeal:SymmetryVG+CutSignatureIdeal:SymmetryVG+CutVeryGood:SymmetryVG+CutGood:ReportGIA+CutIdeal:ReportGIA+CutSignatureIdeal:ReportGIA+CutVeryGood:ReportGIA+ColorE:ClarityIF+ColorF:ClarityIF+ColorG:ClarityIF+ColorH:ClarityIF+ColorI:ClarityIF+ColorE:ClaritySI1+ColorF:ClaritySI1+ColorG:ClaritySI1+ColorH:ClaritySI1+ColorI:ClaritySI1+ColorE:ClarityVS1+ColorF:ClarityVS1+ColorG:ClarityVS1+ColorH:ClarityVS1+ColorI:ClarityVS1+ColorE:ClarityVS2+ColorF:ClarityVS2+ColorG:ClarityVS2+ColorH:ClarityVS2+ColorI:ClarityVS2+ColorE:ClarityVVS1+ColorF:ClarityVVS1+ColorG:ClarityVVS1+ColorH:ClarityVVS1+ColorI:ClarityVVS1+ColorE:ClarityVVS2+ColorF:ClarityVVS2+ColorG:ClarityVVS2+ColorH:ClarityVVS2+ColorI:ClarityVVS2+ColorE:PolishG+ColorF:PolishG+ColorG:PolishG+ColorH:PolishG+ColorI:PolishG+ColorE:PolishID+ColorF:PolishID+ColorG:PolishID+ColorH:PolishID+ColorI:PolishID+ColorE:PolishVG+ColorF:PolishVG+ColorG:PolishVG+ColorH:PolishVG+ColorI:PolishVG+ColorE:SymmetryG+ColorF:SymmetryG+ColorG:SymmetryG+ColorH:SymmetryG+ColorI:SymmetryG+ColorE:SymmetryID+ColorF:SymmetryID+ColorG:SymmetryID+ColorH:SymmetryID+ColorI:SymmetryID+ColorE:SymmetryVG+ColorF:SymmetryVG+ColorG:SymmetryVG+ColorH:SymmetryVG+ColorI:SymmetryVG+ColorE:ReportGIA+ColorF:ReportGIA+ColorG:ReportGIA+ColorH:ReportGIA+ColorI:ReportGIA+PolishG:SymmetryG+PolishID:SymmetryG+PolishVG:SymmetryG+PolishG:SymmetryID+PolishID:SymmetryID+PolishVG:SymmetryID+PolishG:SymmetryVG+PolishID:SymmetryVG+PolishVG:SymmetryVG+PolishG:ReportGIA+PolishID:ReportGIA+PolishVG:ReportGIA+SymmetryG:ReportGIA+SymmetryID:ReportGIA+SymmetryVG:ReportGIA
          + LCarat:Cut + LCarat:Color + LCarat:Calarity + LCarat:Polish + LCarat:Symmetry + LCarat:Report
+ Caratbelow1:Cut + Caratbelow1:Color + Caratbelow1:Calarity + 
Caratbelow1:Polish + Caratbelow1:Symmetry + Caratbelow1:Report"

lm <- lm(as.formula(lm_formula), data = diamond.full.smaller.train)
summary(lm)

lm.pred.valid <- predict(lm, diamond.full.validation)
accuracy(exp(lm.pred.valid), diamond.full.validation$Price)

formula <- "LPrice ~ LCarat+recipCarat+Caratbelow1+Caratequal1+Caratbelow1.5+Caratequal1.5+Caratbelow2+Caratabove2+CutFair+CutGood+CutIdeal+CutSignatureIdeal+CutVeryGood+ColorE+ColorF+ColorG+ColorH+ColorI+ClarityIF+ClaritySI1+ClarityVS1+ClarityVS2+ClarityVVS1+ClarityVVS2+PolishG+PolishID+PolishVG+SymmetryG+SymmetryID+SymmetryVG+ReportGIA+CutGood:ColorE+CutIdeal:ColorE+CutSignatureIdeal:ColorE+CutVeryGood:ColorE+CutGood:ColorF+CutIdeal:ColorF+CutSignatureIdeal:ColorF+CutVeryGood:ColorF+CutGood:ColorG+CutIdeal:ColorG+CutSignatureIdeal:ColorG+CutVeryGood:ColorG+CutGood:ColorH+CutIdeal:ColorH+CutSignatureIdeal:ColorH+CutVeryGood:ColorH+CutGood:ColorI+CutIdeal:ColorI+CutSignatureIdeal:ColorI+CutVeryGood:ColorI+CutGood:ClarityIF+CutIdeal:ClarityIF+CutSignatureIdeal:ClarityIF+CutVeryGood:ClarityIF+CutGood:ClaritySI1+CutIdeal:ClaritySI1+CutSignatureIdeal:ClaritySI1+CutVeryGood:ClaritySI1+CutGood:ClarityVS1+CutIdeal:ClarityVS1+CutSignatureIdeal:ClarityVS1+CutVeryGood:ClarityVS1+CutGood:ClarityVS2+CutIdeal:ClarityVS2+CutSignatureIdeal:ClarityVS2+CutVeryGood:ClarityVS2+CutGood:ClarityVVS1+CutIdeal:ClarityVVS1+CutSignatureIdeal:ClarityVVS1+CutVeryGood:ClarityVVS1+CutGood:ClarityVVS2+CutIdeal:ClarityVVS2+CutSignatureIdeal:ClarityVVS2+CutVeryGood:ClarityVVS2+CutGood:PolishG+CutIdeal:PolishG+CutSignatureIdeal:PolishG+CutVeryGood:PolishG+CutGood:PolishID+CutIdeal:PolishID+CutSignatureIdeal:PolishID+CutVeryGood:PolishID+CutGood:PolishVG+CutIdeal:PolishVG+CutSignatureIdeal:PolishVG+CutVeryGood:PolishVG+CutGood:SymmetryG+CutIdeal:SymmetryG+CutSignatureIdeal:SymmetryG+CutVeryGood:SymmetryG+CutGood:SymmetryID+CutIdeal:SymmetryID+CutSignatureIdeal:SymmetryID+CutVeryGood:SymmetryID+CutGood:SymmetryVG+CutIdeal:SymmetryVG+CutSignatureIdeal:SymmetryVG+CutVeryGood:SymmetryVG+CutGood:ReportGIA+CutIdeal:ReportGIA+CutSignatureIdeal:ReportGIA+CutVeryGood:ReportGIA+ColorE:ClarityIF+ColorF:ClarityIF+ColorG:ClarityIF+ColorH:ClarityIF+ColorI:ClarityIF+ColorE:ClaritySI1+ColorF:ClaritySI1+ColorG:ClaritySI1+ColorH:ClaritySI1+ColorI:ClaritySI1+ColorE:ClarityVS1+ColorF:ClarityVS1+ColorG:ClarityVS1+ColorH:ClarityVS1+ColorI:ClarityVS1+ColorE:ClarityVS2+ColorF:ClarityVS2+ColorG:ClarityVS2+ColorH:ClarityVS2+ColorI:ClarityVS2+ColorE:ClarityVVS1+ColorF:ClarityVVS1+ColorG:ClarityVVS1+ColorH:ClarityVVS1+ColorI:ClarityVVS1+ColorE:ClarityVVS2+ColorF:ClarityVVS2+ColorG:ClarityVVS2+ColorH:ClarityVVS2+ColorI:ClarityVVS2+ColorE:PolishG+ColorF:PolishG+ColorG:PolishG+ColorH:PolishG+ColorI:PolishG+ColorE:PolishID+ColorF:PolishID+ColorG:PolishID+ColorH:PolishID+ColorI:PolishID+ColorE:PolishVG+ColorF:PolishVG+ColorG:PolishVG+ColorH:PolishVG+ColorI:PolishVG+ColorE:SymmetryG+ColorF:SymmetryG+ColorG:SymmetryG+ColorH:SymmetryG+ColorI:SymmetryG+ColorE:SymmetryID+ColorF:SymmetryID+ColorG:SymmetryID+ColorH:SymmetryID+ColorI:SymmetryID+ColorE:SymmetryVG+ColorF:SymmetryVG+ColorG:SymmetryVG+ColorH:SymmetryVG+ColorI:SymmetryVG+ColorE:ReportGIA+ColorF:ReportGIA+ColorG:ReportGIA+ColorH:ReportGIA+ColorI:ReportGIA+PolishG:SymmetryG+PolishID:SymmetryG+PolishVG:SymmetryG+PolishG:SymmetryID+PolishID:SymmetryID+PolishVG:SymmetryID+PolishG:SymmetryVG+PolishID:SymmetryVG+PolishVG:SymmetryVG+PolishG:ReportGIA+PolishID:ReportGIA+PolishVG:ReportGIA+SymmetryG:ReportGIA+SymmetryID:ReportGIA+SymmetryVG:ReportGIA
          + LCarat:Cut + LCarat:Color + LCarat:Calarity + LCarat:Polish + LCarat:Symmetry + LCarat:Report
+ Caratbelow1:Cut + Caratbelow1:Color + Caratbelow1:Calarity + 
Caratbelow1:Polish + Caratbelow1:Symmetry + Caratbelow1:Report"

lm <- lm(formula, data = diamond.full.train)
summary(lm)


lm.pred <- predict(lm, diamond.full.test)
accuracy(exp(lm.pred), diamond.full.test$Price)


lm.step <- step(lm, direction = "backward", trace=0, step=10)
lm.step.pred <- predict(lm.step, diamond.full.test)
accuracy(exp(lm.step.pred), diamond.full.test$Price)

#Lasso Regression

xtrain <- as.matrix(diamond.full.smaller.train[, -c(1:11)])
ytrain <- as.vector(diamond.full.smaller.train$LPrice)
xtest <- as.matrix(diamond.full.validation[, -c(1:11)])
lm.regularized.cv <- cv.glmnet(xtrain, ytrain, 
                               nfolds = 10, family = "gaussian", alpha=1)

lm.regularized.cv$lambda.min

(minLogLambda <- log(lm.regularized.cv$lambda.min))

coef(lm.regularized.cv, s = "lambda.min") 

lm.regularized <- glmnet(xtrain, ytrain, family = "gaussian", 
                         lambda=lm.regularized.cv$lambda.min)
plot(lm.regularized, xvar = "lambda", label = TRUE)

lm.regularized.cv.pred.valid <- predict(lm.regularized.cv, newx = xtest, s = "lambda.min") 
lm.regularized.pred.valid <- predict(lm.regularized, newx = xtest, s = "lambda.min")

accuracy(exp(as.ts(lm.regularized.cv.pred.valid)), as.ts(diamond.full.validation$Price))

accuracy(exp(as.ts(lm.regularized.pred.valid)), as.ts(diamond.full.validation$Price))

xtrain <- as.matrix(diamond.full.train[, -c(1:11)])
ytrain <- as.vector(diamond.full.train$LPrice)
xtest <- as.matrix(diamond.full.test[, -c(1:11)])
lm.regularized.cv <- cv.glmnet(xtrain, ytrain, 
                               nfolds = 10, family = "gaussian", alpha=1)

(minLogLambda <- log(lm.regularized.cv$lambda.min))

coef(lm.regularized.cv, s = "lambda.min") 

lm.regularized <- glmnet(xtrain, ytrain, family = "gaussian", 
                         lambda=lm.regularized.cv$lambda.min)
plot(lm.regularized, xvar = "lambda", label = TRUE)

lm.regularized.cv.pred <- predict(lm.regularized.cv, newx = xtest, s = "lambda.min") 
lm.regularized.pred <- predict(lm.regularized, newx = xtest, s = "lambda.min") 
head(lm.regularized.cv.pred)

head(lm.regularized.pred)

head(as.numeric(exp(lm.regularized.cv.pred)))

head(as.numeric(diamond.full.validation$Price))

accuracy(as.numeric(exp(lm.regularized.cv.pred)), as.numeric(diamond.full.test$Price))

accuracy(as.numeric(exp(lm.regularized.pred)), as.numeric(diamond.full.test$Price))


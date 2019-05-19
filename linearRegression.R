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

rm(list= ls())
data_url <- 'C:/Users/moshm/Desktop/Edinburgh_study/semester2/Industrial Analytics/Project_Industrial/data/sarah-gets-a-diamond-raw-data.csv'

sample<-read.csv(data_url)
str(sample)

sample <- sample %>%
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

lm1 <- lm(Price ~ .,data = sample)

summary(lm1)	
summary(lm1)$r.sq
summary(lm1)$adj.r.sq

coef(lm1)

sample$predictedPrice <- fitted(lm1) # to add predicted values to the dataset
sample$residuals <- residuals(lm1,type="working") # to add residuals to the dataset
coef(lm1) 

par(mfrow=c(2,2)) # to facet all graphs on one page
plot(lm1)

confint(lm1)

lm_interact_1 <- lm(Price ~ Cut + Color + Clarity + Polish + Carat.Weight , data=sample)
summary(lm_interact_1)


#Split data in train and test
ind <- sample.int(n=nrow(sample),size=nrow(sample)*0.50)
train <- sample[ind,]
test <- sample[-ind,]

lm_model <- lm(Price ~ .,data=sample)

#Using the model built in B., make a prediction using the test set
preds <- predict(lm_model,test)

(RSS = sum((test$Price - preds)**2))
(TSS = sum((test$Price - mean(test$Price))**2))
(Rsq = 1 - RSS/TSS)

#-------------------------------------------------------------------------------
# Project: POVERTY VARIABLE SELECTION VALIDATION
# Purpose: Test LASSO model
#-------------------------------------------------------------------------------

# Get libraries

library(caret)
library(dplyr)
library(foreign)
library(ggplot2)
library(glmnet)
library(haven)
library(here)
library(tidyverse)
library(randomForest)

# Clear environment
rm(list = ls())

# Get data
hh_df <- readRDS(file.path('data', 'merged data', 'hh_df.rds'))
hh_df_urban <- readRDS(file.path('data', 'merged data', 'hh_df_urban.rds'))
hh_df_rural <- readRDS(file.path('data', 'merged data', 'hh_df_rural.rds'))

# set seed for reproducibility
set.seed(0393)

# Calculate the size of each of the data sets
size <- floor(nrow(hh_df)/2)

# Generate a random sample of half the households
indexes <- sample(1:nrow(hh_df), size = size)

# Create model specification
# Assign assets as covariates for variable selection
covars <- names(hh_df[,15:46])

select_vars <- c("motor_car",
                 "refrigerator",
                 "drinking_water",
                 "camera",
                 "dining_room",
                 "walls_home"         )

yvar <- "upperpoor"
form <- as.formula(paste(yvar, paste(covars, collapse = " + "), sep = " ~ "))


# Assign the data to the correct sets
training <- hh_df[indexes,] 

testing <- hh_df[-indexes,]


# LASSO WITH CROSS-VALIDATION
# -------------------------------------------------

# Dummy code categorical variables
x.train <- model.matrix(form, data = training)[,-1]
y.train <- as.numeric(training$upperpoor)

# Determine lambda through cross-validation
cv.lasso <- cv.glmnet(x.train, y.train, alpha = 1, family = "binomial")

# Plot to see lambda that minimizes MSE
plot(cv.lasso)

# Fit model on the training data using the cross-validated value of lambda
lasso.model <- glmnet(x.train, y.train, 
                      family = "binomial", 
                      alpha = 1, 
                      lambda = cv.lasso$lambda.min,
                      standardize.response = TRUE)

coef(lasso.model)

# Predict on the test data

x.test <- model.matrix(form, data = testing)[,-1]
y.test <- as.numeric(testing$upperpoor)

assess.glmnet(lasso.model,
              newx = x.test,
              newy = y.test) # Error rate of 24 percent, good but not great

plot(roc.glmnet(lasso.model,
                newx = x.test,
                newy = y.test), type = 'l') 

# Rank variable importance

imp <- cv.lasso$glmnet.fit %>% 
  varImp(lambda = cv.lasso$lambda.min, scale = F) 

imp.sorted <- data.frame(overall = imp$Overall,
                  names = rownames(imp))

imp.sorted[order(imp.sorted$overall, decreasing = T),]


ggplot(imp.sorted, 
       aes(x = reorder(names, overall), y = overall)) + 
  geom_point() +
  geom_segment(aes(xx = names, xend = names, y = 0, yend = overall)) +
  coord_flip()

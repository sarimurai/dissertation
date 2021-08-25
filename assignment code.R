### Part 1###

# set working directry
setwd("C:/Users/User/Desktop/Leeds Univ Classes/【Semester2】 210517- classes/Big data and consumer analytics/assignment")

## 1. Packages and Data
# download packages
install.packages("tidyverse")
install.packages("rgdal")
install.packages("randomForest")
install.packages("ranger")
install.packages("caret")
install.packages("scales")

library(tidyverse)
library(rgdal)
library(randomForest)
library(ranger)
library(caret)
library(scales)

# data reading
Sys.setlocale("LC_ALL", "C")

listings = as_tibble(read.csv("listings.csv", stringsAsFactors = F))


#str(listings)

#listings2 = as_tibble(read.csv("listings.csv", stringsAsFactors = F))
#table(listings2$room_type)
#table(listings2$property_type)

#table(is.na(data_anal$log_price))
#table(is.na(data_anal$accommodates))
#table(is.na(data_anal$beds))
#table(is.na(data_anal$bathrooms))
#table(is.na(data_anal$cleaning_fee))
#table(is.na(data_anal$property_type_House))
#table(is.na(data_anal$property_type_Other))
#table(is.na(data_anal$room_type_Private_room))
#table(is.na(data_anal$room_type_Shared_room))


# Descriptive statistics of important variables.
#install.packages("psych")
#library(psych)
#listings %>% stargazer(data_anal,type="text",summary = TRUE)


## 2. An initial OLS Regression model
# convert prices to numbers
dollar_to_number = function(x) {
  x = gsub("[\\$]", "", x)
  x = gsub(",", "", x)
  x = as.numeric(x)
  x}

listings$price = dollar_to_number(listings$price)
listings$cleaning_fee = dollar_to_number(listings$cleaning_fee)

# examine output
# listings %>%
#select(price, accommodates, bathrooms, cleaning_fee,property_type, room_type) %>%
#drop_na()

# examine na
table(is.na(listings$price))
table(is.na(listings$accommodates))
table(is.na(listings$beds))
table(is.na(listings$bathrooms))
table(is.na(listings$cleaning_fee))
table(is.na(listings$property_type))
table(is.na(listings$room_type))

# fit a model
#initial.ols = lm(price~accommodates+bathrooms+cleaning_fee+
#                   factor(property_type)+factor(room_type),
#                 data = listings[!is.na(listings$price),])
#summary(initial.ols)


## 3.Data Cleaning, transformation and pre-processing
# convert price to numbers
#listings$price = dollar_to_number(listings$price)
hist(listings$price, col = "salmon", breaks = 150)

# skewed price distribution needs to be transformed(log)
hist(log(listings$price), col = "cornflowerblue")

# get rid of any records that have a rental price of zero (generating infinity logs!)
listings = listings[listings$price >0,]
listings$log_price = log(listings$price)
summary(listings$log_price)

# reduce the number of "property_types"
index = listings$property_type == "Apartment" | listings$property_type == "House"
index
listings$property_type[!index] = "Other"

# convert listing_fee into binary data
listings$cleaning_fee = dollar_to_number(listings$cleaning_fee)
listings$cleaning_fee = (listings$cleaning_fee > 0 )+0
listings$cleaning_fee[is.na(listings$cleaning_fee)] = 0

# others
listings$property_type_House = (listings$property_type == "House")+0
listings$property_type_Other = (listings$property_type == "Other")+0
listings$room_type_Private_room = (listings$room_type == "Private room")+0
listings$room_type_Shared_room = (listings$room_type == "Shared room")+0

# fill gaps(empty values on NAs) by median value
#hist(listings$bathrooms)
#hist(listings$beds)

listings$bathrooms[is.na(listings$bathrooms)] = median(listings$bathrooms, na.rm = T)
listings$beds[is.na(listings$beds)] = median(listings$beds, na.rm = T)

# new data table for analysis
data_anal =
  listings %>% select(log_price, accommodates, beds, bathrooms, cleaning_fee,
                      property_type_House, property_type_Other, room_type_Private_room,
                      room_type_Shared_room)
str(data_anal)
summary(data_anal)

# Std.Dev. for descritive statistics
sd(data_anal$log_price)
sd(data_anal$accommodates)
sd(data_anal$beds)
sd(data_anal$bathrooms)
sd(data_anal$cleaning_fee)
sd(data_anal$property_type_House)
sd(data_anal$property_type_Other)
sd(data_anal$room_type_Private_room)
sd(data_anal$room_type_Shared_room)

#table(is.na(listings$property_type_House))
#table(is.na(listings$property_type_Other))
#table(is.na(listings$room_type_Private_room))
#table(is.na(listings$room_type_Shared_room))


### Part 2###

## 1. Random Forests- overview and background

## 2. Random Forests in R: a worked example
# The Random Forest model will be constructed using the georgia data
# read data of gergia
#load("georgia.RData")
#georgia

# converts the target variable to 1000s of dollars
#georgia$MedInc = georgia$MedInc/1000

#  create the training and validation data splits
#set.seed(123) # reproducibility
#train.index = createDataPartition(georgia$MedInc, p = 0.7, list = F)
set.seed(123) # reproducibility
train.index_M = createDataPartition(data_anal$log_price, p = 0.7, list = F)


summary(data_anal$log_price[train.index_M])
summary(data_anal$log_price[-train.index_M])

train_x_M = data_anal[train.index_M,]
test_x_M = data_anal[-train.index_M,]

#normalization
Z_train_x_M =
  train_x_M %>% select(-log_price) %>%
  mutate_if(is_logical,as.character) %>%
  mutate_if(is_double,scale) %>% data.frame()

Z_test_x_M =
  test_x_M %>% select(-log_price) %>%
  mutate_if(is_logical,as.character) %>%
  mutate_if(is_double,scale) %>% data.frame()

Z_train_x_M$log_price = train_x_M$log_price
Z_test_x_M$log_price = test_x_M$log_price


#  create an initial model using the RF implementation
#reg.mod = MedInc~PctRural+PctBach+PctEld+PctFB+PctPov+PctBlack
reg.mod = log_price~accommodates + beds + bathrooms +
  cleaning_fee + property_type_House +
  property_type_Other + room_type_Private_room +
  room_type_Shared_room

# use non-normalized data
rf1 <- randomForest(
  formula = reg.mod, ntree= 5000,
  data = train_x_M
)

# number of trees with lowest error
which.min(rf1$mse)
plot(rf1)

which.min(rf1$mse)
plot(rf1)



# tuning grid
params <- expand.grid(
  # the max value should be equal to number of predictors
  mtry = c(2:8),
  # the node sizes
  node_size = seq(3, 15, by = 2),
  # the within training data sample split
  samp_size = c(.65, 0.7, 0.8, 0.9, 1)
)
# have a look!
dim(params)
head(params)
tail(params)


# define a vector to save the results of each iteration of the loop
rf.grid = vector()
# now run the loop
for(i in 1:nrow(params)) {
  # create the model
  rf.i <- ranger(
    formula = reg.mod,
    data = train_x_M,
    num.trees = 5000,
    mtry = params$mtry[i],
    min.node.size = params$node_size[i],
    sample.fraction = params$samp_size[i],
    seed = 123
  )
  # add OOB error to rf.grid
  rf.grid <- c(rf.grid, sqrt(rf.i$prediction.error))
  # print to see progress
  if (i%%10 == 0) cat(i, "\t")
}

# add the result to the params grid
params$OOB = rf.grid

params[which.min(params$OOB),]

# These can be assigned to best_vals and passed to a final model:
best_vals = unlist(params[which.min(params$OOB),])
best_vals

rfFit = ranger(
  formula = reg.mod,
  data = train_x_M,
  num.trees = 5000,
  mtry = best_vals[1],
  min.node.size = best_vals[2],
  sample.fraction = best_vals[3],
  seed = 123,
  importance = "impurity"
)

# The final model can be evaluated by using it to predict median income values for the test data:
pred.rf = predict(rfFit, data = test_x_M)$predictions

#predict(rfFit, data = Z_test_x_M)

pred.rf

# model accuracy evaluated
postResample(pred = pred.rf, obs = test_x_M$log_price)

# the variable importance
data.frame(name = names(rfFit$variable.importance),
           value = rescale(rfFit$variable.importance, c(0,100))) %>%
  arrange(desc(value)) %>%
  ggplot(aes(reorder(name, value), value)) +
  geom_col(fill = "red") + coord_flip() + xlab("") +
  theme(axis.text.y = element_text(size = 10))


data.frame(name = names(rfFit$variable.importance),
           value = rfFit$variable.importance) %>%
  arrange(desc(value)) %>%
  ggplot(aes(reorder(name, value), value)) +
  geom_col(fill = "red") + coord_flip() + xlab("") +
  theme(axis.text.y = element_text(size = 10))

rfFit$variable.importance



# predicted and observed data can be compared graphically
#data.frame(Predicted = pred.rf, Observed = ★Z_test_x_M$log_price) %>%
ggplot(aes(x = Observed, y = Predicted))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "lm", col = "blue")+
  geom_smooth(method = "loess", col = "red")


## 3. Summary of the task
# save from Part 1
#save(data_anal, file = "data_anal.RData")
#write.csv(data_anal, file = "data_anal.csv")
#write.csv(listings, file = "listings.csv")

# load into your assignment R session
#load("data_anal.RData")



##############application for potential data###################

# recreate the refined OLS model for use in your assignment 
## 4. A refined OLS model

# data_anal　normalization
#z_data_anal <- data.frame(data_anal[,-1] %>% scale())
#z_data_anal$log_price = data_anal$log_price


#apply(z_data_anal, 2, mean)
#apply(z_data_anal, 2, sd)

reg.mod =
  as.formula(log_price ~ accommodates + beds + bathrooms +
               cleaning_fee + property_type_House +
               property_type_Other + room_type_Private_room +
               room_type_Shared_room)
m = lm(reg.mod, data = data_anal)
summary(m)

# relative importance of each predictor variable
varImp(m, scale = T)

#imp_OLS <- data.frame(19.616172,1.269273, 5.001370, 4.813812,
8.890831, 3.448799, 29.883115, 10.004297)

#colnames(imp_OLS) <- c("accommodates","beds","bathrooms","cleaning_fee", 
"property_type_House", "property_type_Other", 
"room_type_Private_room", "room_type_Shared_room")
#names(imp_OLS)
#imp_OLS/29.883115*100


# compare imortance RF vs OLS
#imp_OLS <- data.frame(varImp(m, scale = T))
#colnames(imp_OLS) <- c("importance")

#imp_RF <- data.frame(rescale(rfFit$variable.importance, c(0,100)))
#colnames(imp_RF) <- c("importance")


# the model predictions compared with the actual, observed logged price values
ggplot(data.frame(Observed = data_anal$log_price,
                  Predicted = m$fitted.values),
       aes(x = Observed, y = Predicted))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "lm", col = "red")

# the model can be applied to the test data
# read the test data
load("potential_rentals.RData")
potential_rentals

#potential_rentals normalization
#Z_potential_rentals <- data.frame(potential_rentals[,-1] %>% scale())

#potential_rentals[,-1]
#Z_potential_rentals

# prediction
pred = exp(predict(m, newdata = potential_rentals))
pred

# round the figure to the nearest $5
tab = data.frame(ID = potential_rentals$ID,
                 `OLS Price` = paste0("$", round(pred/5)*5))
tab


# If your final RF model is called rfFit 
#then you will be able to create a table of all of the result
pred_ols = exp(predict(m, newdata = potential_rentals))



pred_rf = exp(predict(rfFit, data = potential_rentals)$predictions)


data.frame(ID = potential_rentals$ID,
           `OLS Price` = paste0("$", round(pred_ols/5)*5),
           `RF Price` = paste0("$", round(pred_rf/5)*5))














library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)
library(rpart)
library(rpart.plot)
library(readxl)
library(tidyverse)
library(scales)
#install.packages("mice")
#install.packages("DT")
library(mice)
library(DT)

df1 =read.csv("players_18.csv")
df2=read.csv("players_19.csv")
df3=read.csv("players_20.csv")

df1$year="2018"
df2$year="2019"
df3$year="2020"

#merging data

merge_df= rbind(df1,df2,df3)
head(merge_df)

new_df1 = select(merge_df,-player_url,-long_name,-real_face, -body_type, -player_traits,-player_tags,-work_rate, -team_jersey_number,-release_clause_eur,-contract_valid_until,-nation_position, -nation_jersey_number, -gk_diving, -gk_handling,-gk_kicking,-gk_reflexes,-gk_speed, -gk_positioning, -ls, -st, -rs, -lw, -lf, -cf, -rf, -rw, -lam, -cam, -ram, -lm, -lcm, -cm, -rcm, -rm,-lwb,-ldm, -cdm,-rdm,-rwb,-lb, -lcb, -cb, -rcb, -rb)
new_df1
head(new_df1)
str(new_df1)

na_values_count <- colSums(is.na(new_df1))
na_values_count

# convert to numeric 
new_df1 <- new_df1 %>% mutate(goalkeeping_diving=as.numeric(goalkeeping_diving),
                              goalkeeping_handling=as.numeric(goalkeeping_handling),
                              goalkeeping_kicking=as.numeric(goalkeeping_kicking),
                              goalkeeping_positioning=as.numeric(goalkeeping_positioning),
                              goalkeeping_reflexes=as.numeric(goalkeeping_reflexes), 
                              attacking_crossing=as.numeric(attacking_crossing),
                              attacking_finishing=as.numeric(attacking_finishing),
                              attacking_heading_accuracy=as.numeric(attacking_heading_accuracy),
                              attacking_short_passing=as.numeric(attacking_short_passing),
                              attacking_volleys=as.numeric(attacking_volleys),
                              defending_marking=as.numeric(defending_marking),
                              defending_standing_tackle=as.numeric(defending_standing_tackle),
                              defending_sliding_tackle=as.numeric(defending_sliding_tackle),
                              movement_balance=as.numeric(movement_balance),
                              movement_reactions =as.numeric(movement_reactions),
                              movement_agility =as.numeric(movement_agility),
                              movement_sprint_speed =as.numeric(movement_sprint_speed),
                              movement_acceleration=as.numeric(movement_acceleration),
                              skill_dribbling=as.numeric(skill_dribbling),
                              skill_curve=as.numeric(skill_curve),
                              skill_fk_accuracy=as.numeric(skill_fk_accuracy),
                              skill_long_passing=as.numeric(skill_long_passing),
                              skill_ball_control=as.numeric(skill_ball_control))
head(new_df1)
str(new_df1)

#code to merge columns
new_df1$goal_keeping <-  (new_df1$goalkeeping_diving+ new_df1$goalkeeping_handling+ new_df1$goalkeeping_kicking+ new_df1$goalkeeping_positioning+ new_df1$goalkeeping_reflexes)/5
new_df1$attacking <- (new_df1$attacking_crossing+ new_df1$attacking_finishing+new_df1$attacking_heading_accuracy+ new_df1$attacking_short_passing+new_df1$attacking_volleys)/5
new_df1$defending <- (new_df1$defending_marking+ new_df1$defending_standing_tackle+ new_df1$defending_sliding_tackle)/3
new_df1$movement <- (new_df1$movement_acceleration+new_df1$movement_sprint_speed+new_df1$movement_agility+ new_df1$movement_reactions+new_df1$movement_balance)/5
new_df1$skilling<-(new_df1$skill_ball_control+new_df1$skill_curve+new_df1$skill_dribbling+new_df1$skill_fk_accuracy+new_df1$skill_long_passing+new_df1$skill_moves)/6
new_df1$wages <- floor(as.numeric(new_df1$wage_eur)/1000) 
new_df1$market_value <- floor(as.numeric(new_df1$value_eur)/1000)
str(new_df1)


fifa_data <- na.omit(new_df1)
any(is.na(new_df1))
head(new_df1)
str(new_df1)

new_fifa <- fifa_data %>%
  mutate(factor_overall.score = case_when(
    overall >= 90 ~ "90+",
    overall >= 80 ~ "80-89",
    overall >= 70 ~ "70-79",
    overall >= 60 ~ "60-69",
    overall >= 50 ~ "50-59",
    overall >= 40 ~ "40-49",
    TRUE ~ "score under 40"
  ))
head(new_fifa)

##############

new_fifa$nationality <- factor(new_fifa$nationality)
levels(new_fifa$nationality)
new_fifa$nationality_numeric <- as.numeric(new_fifa$nationality)
head(new_fifa$nationality_numeric)

new_fifa$club <- factor(new_fifa$club)
levels(new_fifa$nationality)
new_fifa$club_numeric <- as.numeric(new_fifa$club)
head(new_fifa$club_numeric)

#train-test split

library(caTools)
set.seed(111)

set_sample <-  sample.split(new_fifa$wages,SplitRatio = 0.8)
train_set <- subset(new_fifa,set_sample==TRUE)
test_set <- subset(new_fifa,set_sample==FALSE)
nrow(train_set)
nrow(test_set)

#linear models

lm1 <- lm(wages~ overall + potential + age + height_cm + club_numeric + nationality_numeric+ weight_kg + market_value + goal_keeping + attacking + movement + defending + skilling + international_reputation + potential, data=train_set)
summary(lm1)
AIC1 = AIC(lm1)
#Adjusted R-squared:  0.763 
  
  #cook's d
cook<-cooks.distance(lm1)
cooks.distance(lm1)[which.max(cooks.distance(lm1))]
plot(lm1,which=4)  
par(mfrow = c(2, 2))
plot(lm1)

n <- length(cook)
cutoff <- 4/n
plot(cook, pch = 20, main = "Cook's Distance Plot before removal")
abline(h = cutoff, col = "red")

train_new <- subset(train_set, cook <= cutoff)

lm_cook <- lm(wages~ overall + potential + age + height_cm + club_numeric + nationality_numeric+ weight_kg + market_value + goal_keeping + attacking + movement + defending + skilling + international_reputation + potential, data=train_new)
summary(lm_cook)
cook_AIC <-  AIC(lm_cook)
cook2 <-  cooks.distance(lm_cook)

sample_n <-  nrow(train_new)
plot(cook2, pch = 20, main = "Cook's Distance Plot after removal")
abline(h = cutoff, col = "red")

# 164623.3

#lm3 is the non numeric version
lm3 <- lm(wages~ overall + potential + age + height_cm + club + nationality + weight_kg + market_value + goal_keeping + attacking + movement + defending + skilling + international_reputation + potential, data=train_set)
summary(lm3)

AIC3 = AIC(lm3)
#R-squared:  0.7631,	Adjusted R-squared:  0.76, AIC = 221366.5

#removing insignificant variables weight, height and movement

lm2 <- lm(wages~ overall + potential + age  + market_value + goal_keeping + attacking + defending + skilling + international_reputation + potential, data=train_set)
summary(lm2)
AIC2 = AIC(lm2)
AIC2
res <-  resid(lm2)
#R-squared:  ,0.7631,	Adjusted R-squared:  0.763,	AIC:  221403.3

#checking assumptions

install.packages("ggfortify")
library(ggfortify)
linear_assumptions <-  autoplot(lm1)
linear_assumptions

library(car)
vif_mod <- vif(lm(wages ~ overall + potential + age  + market_value + goal_keeping + attacking + defending + skilling + international_reputation + potential, data=train_set))
vif_mod
#p1(resvsfit) Used to check the linear relationship assumptions. A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.
#p2(qq) normality of residuals dont follow a straight line, cannot assume normality
#p3(scale- location) shows heteroscedasticity problem use log or square root transformation
#p4(resvslev) highligts top 3 influential points

#A4 cook'd plot
A4 <-  plot(lm2, 4)
A4_2 <- plot(lm2, 5)
#residuals vs leverage, #  there are outliers but they are not influential to the regression results.


#log transformation 
# adding constant to avoid zero in wages
train_set$wages_mod <- train_set$wages + 0.02
train_set$market_mod <- train_set$market_value + 0.02

test_set$wages_mod <- test_set$wages + 0.02
test_set$market_mod <- test_set$market_value + 0.02
test_set$log_value <- log(test_set$market_mod)
test_set$log_wages <- log(test_set$wages_mod)

#before transformation 
hist(new_fifa$wages)
hist(new_fifa$market_value)

#after transformation
#train_set$log_wages  <-  log(train_set$wages_mod)
train_set$log_value <- log(train_set$market_mod)
#hist(train_set$log_wages)

# log of market value is enough

hist(train_set$log_value)

#positive skewness has reduced

#using log transformed log_value AIC 241032.1
str(train_set)

log_model <- lm(wages~ overall + potential + age  + club_numeric + nationality_numeric + log_value + goal_keeping + attacking + defending + skilling + movement + international_reputation + potential, data=train_new)
summary(log_model)
AIC4 = AIC(log_model)
AIC4

log_model2 <- lm(wages~ overall + potential + age  +club + nationality + log_value + goal_keeping + attacking + defending + skilling + movement + international_reputation + potential, data=train_new)
summary(log_model2)

AIC_logm2 = AIC(log_model2)
AIC_logm2

#log_y_model <- lm(log_wages~ overall + potential + age  + log_value + goal_keeping + attacking + defending + skilling + international_reputation + potential, data=train_set)
#summary(log_model)
#AICn = AIC(log_y_model)
#AICn


#fit glm with log-link coz our model has influential points and heterskadasticity

install.packages("pscl")

library(pscl)

glm <- glm(wages_mod ~ overall + potential + age + log_value + club_numeric + nationality_numeric+ goal_keeping + defending + attacking + defending + skilling + international_reputation + potential, data = train_set, family = Gamma(link="log"))
glm

# AIC: 137900

## Remove the constant from the fitted values and residuals

glm$fitted.values <- exp(glm1$fitted.values) - 0.02
glm$residuals <- log(train_set$wages + 0.02) - glm1$fitted.values
summary(glm)

#AIC

#AIC_mod <- step(glm1, direction = "backward", k = 2)
#AIC_mod1 <- step(glm1, direction = "both", k = 2)
#AIC_mod2 <- step(glm1, direction = "both", k = 2)

#based on potential
AIC_mod4 <- step(glm(wages_mod ~ potential, data=train_set, family = Gamma(link="log")),
                 scope = wages ~  overall + log_value + potential + age + height_cm + club_numeric + nationality_numeric + weight_kg + market_value + goal_keeping + attacking + movement + defending + skilling + international_reputation, 
                 direction = "both")
# AIC=137360.7

#based on market value AIC=120913.6
AIC_mod <- step(glm(wages_mod ~  market_value, data=train_new, family = Gamma(link="log")),
                scope = wages ~  overall + log_value + potential + age + height_cm + club_numeric + nationality_numeric+ weight_kg + market_value + goal_keeping + attacking + movement + defending + skilling + international_reputation, 
                direction = "both")

step_mod <-  glm(wages_mod ~ market_value + overall + log_value + international_reputation + 
  age + potential + attacking + movement + club_numeric + weight_kg + 
  nationality_numeric + defending + goal_keeping, data = train_new, family = Gamma(link="log"))
step_mod

# AIC =137300.4


#based on AIC, the final model retain all the variables we got from lm model. 


install.packages("MASS")
library(MASS)

# robust reg

robust <- rlm( wages ~  overall + market_value + potential + age + height_cm + club_numeric + nationality_numeric+ weight_kg + goal_keeping + attacking + movement + defending + skilling + international_reputation, data=train_new, maxit=50)
robust
AIC(robust)

robust2 <- rlm( wages ~  overall + log_value + potential + age + height_cm + club_numeric + nationality + weight_kg  + goal_keeping + attacking + movement + defending + skilling + international_reputation, data=train_new, maxit=50)
robust2
AIC(robust2)

ggplot(test_set, aes(x=predict(robust, test_set), y=wages)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='robust Model')





AIC5 <- AIC(robust)

mape(predict(robust,test_set,type="response"),test_set$wages)*100



pred <- predict(robust, test_set)
resid <- test_set$wages - pred

# Calculate the total sum of squares
tss <- sum((test_set$wages - mean(test_set$wages))^2)

# Calculate the residual sum of squares
rss <- sum(resid^2)

# Calculate the R-squared value
rsq <- 1 - (rss/tss)

# Print the R-squared value 0.7573761
> 
print(rsq)

AIC4 <- AIC(robust)
AIC4

mape(predict(lm_cook,test_set,type="response"),test_set$wages)*100
mape(predict(lm_cook,test_set,type="response"),test_set$wages)*100

ggplot(test_set, aes(x=predict(robust, test_set), y=wages)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Robust Model')

#AIC =223104.4

# Random Forest

#install.packages("randomForest")
library(randomForest)
library (rsq)
random_forest_model1 <-  randomForest(wages_mod ~ overall + log_value + potential + age + height_cm + club_numeric + nationality_numeric+ weight_kg +  goal_keeping + attacking + movement + defending + skilling + international_reputation, data=train_new, ntree=25)
summary(random_forest_model1)

random_forest_model <-  randomForest(wages_mod ~ overall + log_value + potential + age + height_cm + club_numeric + nationality_numeric+ weight_kg +  goal_keeping + attacking + movement + defending + skilling + international_reputation, data=train_set, ntree=25)
summary(random_forest_model)
AIC(random_forest_model)





random_forest_pred <- mape(predict(random_forest_model, test_set), test_set$wages)*100
random_forest_pred 

ggplot(test_set, aes(x=predict(random_forest_model, test_set), y=wages)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Random Forest Model')

install.packages("Metrics")

library(Metrics)

models <- list(lm1, lm_cook, glm, log_model, step_mod, robust, random_forest_model)
names <- c("linear model", "lm(cook's D) ", "glm", "log_model", "AIC", "robust", "rfm")
mape_results <- c()

for (i in seq_along(models)) {
  mape_result <- mape(predict(models[[i]], test_set), test_set$wages)*100
  mape_results <- c(mape_results, mape_result)
}

names(mape_results) <- names
mape_results

mape_results_df <- data.frame(Model = names(mape_results), MAPE = mape_results)

ggplot(mape_results_df, aes(x = Model, y = MAPE)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) + # Add a line to connect the points
  labs(title = "mean absolute percentage error",
       x = "Models",
       y = "MAPE scores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





################################

#prediction

#install.packages("Metrics")

library(Metrics)

models <- list(lm1,log_model, glm, step_mod, robust)
names <- c("lm1", "log_model", "glm", "AIC_mod", "robust")
mape_results <- c()

for (i in seq_along(models)) {
  mape_result <- mape(predict(models[[i]], test_set), test_set$wages)*100
  mape_results <- c(mape_results, mape_result)
}

names(mape_results) <- names
mape_results

mape_results_df <- data.frame(Model = names(mape_results), MAPE = mape_results)

ggplot(mape_results_df, aes(x = Model, y = MAPE)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) + # Add a line to connect the points
  labs(title = "MAPE Results for Models",
       x = "Model",
       y = "MAPE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(test_set, aes(x=predict(robust, test_set), y=wages)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with Robust Model')


###LASSO REGRESSION#####
#define response variable
y <- train_set$wages
x <- data.matrix(train_set[, c('age', 'height_cm', 'nationality_numeric', 'club_numeric','overall','potential','market_value','international_reputation','shooting',
                               'passing','dribbling','attacking','goal_keeping','pace')])
install.packages("glmnet")
library(glmnet)
library(Matrix)
#lambda is the Tuning Parameter that controls the bias-variance tradeoff and 
#we estimate its best value via cross-validation. 
#It is a Regularization Method to reduce Overfitting.
#
#perform k-fold cross-validation to find optimal lambda value


cv_model <- cv.glmnet(x, y, alpha = 1)



#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
#produce plot of test MSE by lambda value
plot(cv_model)
#The value of lambda will be chosen by cross-validation.
#The plot shows cross-validated mean squared error. As lambda decreases, the mean squared error decreases. 
#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)



##Note that this is a key difference between ridge regression and lasso regression. Ridge regression shrinks all coefficients towards zero, 
##but lasso regression 
##has the potential to remove predictors from the model by shrinking the coefficients completely to zero.
#define new observation,
new = matrix(c(28,190,1,1,85,10500,5,85,70,80,75,70,25,90), nrow=1, ncol=14)



#use lasso regression model to predict response value
predict(best_model, s = best_lambda, newx = new)
#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)
y_predicted

mape(predict(best_model, s = best_lambda, newx = x))
#find SST and SSE



sst <- sum((y - mean(y))^2)
sst
sse <- sum((y_predicted - y)^2)
sse
#find R-Squared 
#THIS INDICATES ACCURACY OF OUR MODEL, CLOSER THE VALUE OF R^2 TO 1 , more it is accurate.
rsq <- 1 - sse/sst
rsq
###That is, the best model was able to explain 76.55% of the variation in the response values of the training data.




#************RIDGE REGRESSION******#
#fit ridge regression model
model <- glmnet(x, y, alpha = 0)
#view summary of model
summary(model)
#perform k-fold cross-validation to find optimal lambda value
cv_model_ridge <- cv.glmnet(x, y, alpha = 0)


#find optimal lambda value that minimizes test MSE
best_lambda_r <- cv_model_ridge$lambda.min
best_lambda_r
#produce plot of test MSE by lambda value
plot(cv_model)


#find coefficients of best model
best_model_r <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model_r)
#produce Ridge trace plot
plot(model, xvar = "lambda")
#define new observation,


new = matrix(c(28,190,1,1,85,10500,5,85,70,80,75,70,25,90), nrow=1, ncol=14)


#use fitted best model to make predictions
y_predicted_R <- predict(model, s = best_lambda, newx = x)
y_predicted_R
#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted_R - y)^2)


#find R-Squared
rsq <- 1 - sse/sst
rsq
#thus we have accuracy of 75.956% as ouR R square value is 0.75956, which is somewhat close to 1 .


# Set Brazil as the reference group
train_set$nationality <- relevel(train_set$nationality, ref = "Brazil")

# Create dummy variables for nationality with Brazil group omitted
nationality_dummy_train <- model.matrix(~ nationality + 0, data = train_set)
colnames(nationality_dummy_train) <- gsub("nationality", "", colnames(nationality_dummy_train))


levels(new_fifa$nationality)
head(nationality_dummy_train, 1)
levels(new_fifa$club)
str(new_fifa)
#linear models

lm7 <- lm(wages~ overall + potential + age + height_cm + club_numeric + nationality_dummy_train + weight_kg + market_value + goal_keeping + attacking + movement + defending + skilling + international_reputation , data=train_set)
summary(lm7)

#H_0 : the correlation between wages and nationality is equal to 0
#H_A : the correlation between wages and nationality is not equal to 0

new_fifa$nationality <- factor(new_fifa$nationality)
levels(new_fifa$nationality)
cor.test(new_fifa$wages, as.numeric(new_fifa$nationality))
print(cor.test(new_fifa$wages, as.numeric(new_fifa$nationality)))

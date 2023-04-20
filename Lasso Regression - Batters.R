library(glmnet)

?glmnet

setwd("/Users/mogrysko/Documents/Coursework/DataScience/DSP/data3")
df_bat2 <- read.csv(file="df_comb_prev_next_3_bat_R.csv", header=TRUE)

#check col names
colnames(df_bat2)

#define response variable
y <- df_bat2$Next_WAR

#define matrix of predictor variables
x <- data.matrix(df_bat2[, c('WAR','Season','Age','Team','Lg','G','PA','AB','R','H','X1B','X2B','X3B','HR','RBI','SB','CS','BB','SO','OPS.','TB','GIDP','HBP','SH','SF','IBB','Prev_WAR','Max_WAR','Max_WAR_Age','Pos_Cat','Season_WAR_Class','Prev_WAR_Class','Team_WL','Games_played','war_season','player_season','war_corr')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)


#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#[1] 0.5099058



#trying with fewer predictors

#define matrix of predictor variables
#x <- data.matrix(df_bat2[, c("Age","X1B","SB","BB","Max_WAR","Max_WAR_Age","Pos_Cat","Prev_WAR_Class","war_season")])

#perform k-fold cross-validation to find optimal lambda value
#cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
#best_lambda <- cv_model$lambda.min
#best_lambda

#produce plot of test MSE by lambda value
#plot(cv_model) 

#find coefficients of best model
#best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
#coef(best_model)

#use fitted best model to make predictions
#y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
#sst <- sum((y - mean(y))^2)
#sse <- sum((y_predicted - y)^2)

#find R-Squared
#rsq <- 1 - sse/sst
#rsq

#[1] 0.5070093



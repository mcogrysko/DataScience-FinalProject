library(glmnet)

setwd("/Users/mogrysko/Documents/Coursework/DataScience/DSP/data3")
df_bat2 <- read.csv(file="df_comb_prev_next_2_bat_R.csv", header=TRUE)

#check col names
colnames(df_bat2)

#define response variable
y <- df_bat2$Next_WAR

#define matrix of predictor variables
x <- data.matrix(df_bat2[, c('FG_playerid','Lg','H','RBI','OPS.','SF','Rbat','FG_AVG','FG_BsR','FG_Events','FG_wRAA','Prev_WAR','Prev_WAR_Class','war_corr','WAR','G','X1B','SB','TB','IBB','Rdp','FG_BABIP','FG_ISO','FG_SLG','FG_wRC','Max_WAR','Team_WL','Team','R','HR','SO','SH','dWAR','Rfield','FG_BB.','FG_Def','FG_wOBA','FG_OPS','Season_WAR_Class','player_season','Age','AB','X3B','BB','HBP','oWAR','Rbaser...Rdp','FG_BB.K','FG_L.WAR','FG_WAR','FG_OBP','Pos_Cat','war_season','Season','PA','X2B','CS','GIDP','WAA','Rbaser','FG_Bat','FG_K.','FG_Spd','FG_wSB','Max_WAR_Age','Games_played')])

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

#[1] 0.5111249



#trying with fewer predictors

#define matrix of predictor variables
x <- data.matrix(df_bat2[, c("Age","X1B","SB","BB","Max_WAR","Max_WAR_Age","Pos_Cat","Prev_WAR_Class","war_season")])

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

#[1] 0.5070093



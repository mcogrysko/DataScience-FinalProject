library(car)
library(HH)


setwd("/Users/mogrysko/Documents/Coursework/DataScience/DSP/data3")
df_bat <- read.csv(file="df_comb_prev_next_2_bat_R.csv", header=TRUE)

#splom all seasons
#dev.new(height=800, width=800)
#splom(df_bat, main="SPLOM All Batters", pch=19, cex=.3, xlab=NULL, axis.text.cex = 0.1, varname.cex = 0.3, axis.line.tck = .3)


head(df_bat)
str(df_bat)
summary(df_bat)
colnames(df_bat)

#initial lm model
bat.lm <- lm(Next_WAR ~ WAR+Season+Age+Team+Lg+G+PA+AB+R+H+X1B+X2B+X3B+HR+RBI+SB+CS+BB+SO+OPS.+TB+GIDP+HBP+SH+SF+IBB+WAA+oWAR+dWAR+Rbat+Rdp+Rbaser+Rbaser...Rdp+Rfield+FG_AVG+FG_BABIP+ FG_Bat+FG_BB.K+FG_BB.+FG_BsR+FG_ISO+FG_K.+FG_L.WAR+FG_Def+FG_Events+FG_SLG+FG_Spd+FG_WAR+FG_wOBA+FG_wRAA+FG_wRC+FG_wSB+FG_OBP+FG_OPS+Prev_WAR+Max_WAR+Max_WAR_Age+Pos_Cat+Season_WAR_Class+Prev_WAR_Class +Team_WL+Games_played+war_season+player_season+war_corr, data=df_bat)
bat.lm$coefficients
anova(bat.lm)
summary(bat.lm)

#vif initial model
vif(bat.lm)

#correlation
cor(df_bat[,c("WAR","Season","Age","Team","Lg","G","PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SB","CS","BB","SO","OPS.","TB","GIDP","HBP","SH","SF","IBB","WAA","oWAR","dWAR","Rbat","Rdp","Rbaser","Rbaser...Rdp","Rfield","FG_AVG","FG_BABIP", "FG_Bat","FG_BB.K","FG_BB.","FG_BsR","FG_ISO","FG_K.","FG_L.WAR","FG_Def","FG_Events","FG_SLG","FG_Spd","FG_WAR","FG_wOBA","FG_wRAA","FG_wRC","FG_wSB","FG_OBP","FG_OPS","Prev_WAR","Max_WAR","Max_WAR_Age","Pos_Cat","Season_WAR_Class","Prev_WAR_Class" ,"Team_WL","Games_played","war_season","player_season","war_corr")])

#subset
models <- leaps::regsubsets(Next_WAR ~ WAR+Season+Age+Team+Lg+G+PA+AB+R+H+X1B+X2B+X3B+HR+RBI+SB+CS+BB+SO+OPS.+TB+GIDP+HBP+SH+SF+IBB+WAA+oWAR+dWAR+Rbat+Rdp+Rbaser+Rbaser...Rdp+Rfield+FG_AVG+FG_BABIP+ FG_Bat+FG_BB.K+FG_BB.+FG_BsR+FG_ISO+FG_K.+FG_L.WAR+FG_Def+FG_Events+FG_SLG+FG_Spd+FG_WAR+FG_wOBA+FG_wRAA+FG_wRC+FG_wSB+FG_OBP+FG_OPS+Prev_WAR+Max_WAR+Max_WAR_Age+Pos_Cat+Season_WAR_Class+Prev_WAR_Class +Team_WL+Games_played+war_season+player_season+war_corr, data = df_bat, nbest=2, really.big=T)
models.summary <- summaryHH(models)
tmp <- (models.summary$cp < 10)
models.summary[tmp,]

#Age-X1B-SB-BB-Max_WAR-Max_WAR_Age-Pos_Cat-Prev_WAR_Class-war_season

#steps with full linear model
models.step <- step(bat.lm)

#Next_WAR ~ Season + Age + Lg + G + PA + AB + R + H + X1B + X2B + CS + BB + SO + GIDP + SH + SF + WAA + dWAR + Rbat + Rbaser...Rdp + Rfield + FG_ISO + FG_SLG + FG_WAR + FG_wOBA + FG_wRAA + FG_wRC + FG_wSB + FG_OPS + Max_WAR + Max_WAR_Age + Pos_Cat + Season_WAR_Class + Prev_WAR_Class + Games_played + war_season + player_season

#step1
bat.step.lm <- lm(Next_WAR ~ WAR + Season + Age + Lg + G + PA + AB + R + H + X1B + X2B + CS + BB + SO + GIDP + SH + SF + WAA + dWAR + Rbat + Rbaser...Rdp + Rfield + FG_ISO + FG_SLG + FG_WAR + FG_wOBA + FG_wRAA + FG_wRC + FG_wSB + FG_OPS + Max_WAR + Max_WAR_Age + Pos_Cat + Season_WAR_Class + Prev_WAR_Class + Games_played + war_season + player_season, data=df_bat)
summary(bat.step.lm)
#Multiple R-squared:  0.5116,	Adjusted R-squared:  0.5104 

vif(bat.step.lm)

cor(df_bat[,c("WAR","Season","Age","Lg","G","PA","AB","R","H","X1B","X2B","CS","BB","SO","GIDP","SH","SF","WAA","dWAR","Rbat","Rbaser...Rdp","Rfield","FG_ISO","FG_SLG","FG_WAR","FG_wOBA","FG_wRAA","FG_wRC","FG_wSB","FG_OPS","Max_WAR","Max_WAR_Age","Pos_Cat","Season_WAR_Class","Prev_WAR_Class","Games_played","war_season","player_season")])

bat.step.lm$coefficients

anova(bat.step.lm)

#step2
bat.step2.lm <- lm(Next_WAR ~ WAR + Age + Lg + R + H + X1B + X2B + CS + BB + SO + GIDP + SH + SF + Rbat + Rbaser...Rdp + Rfield + FG_ISO + FG_SLG + FG_wOBA + FG_wRAA +  FG_wSB + FG_OPS + Max_WAR + Max_WAR_Age + Pos_Cat + Season_WAR_Class + Prev_WAR_Class + Games_played + war_season + player_season, data=df_bat)
summary(bat.step2.lm)

vif(bat.step2.lm)


cor(df_bat[,c("WAR","Age","Lg","R","H","X1B","X2B","CS","BB","SO","GIDP","SH","SF","Rbat","Rbaser...Rdp","Rfield","FG_ISO","FG_SLG","FG_wOBA","FG_wRAA","FG_wSB","FG_OPS","Max_WAR","Max_WAR_Age","Pos_Cat","Season_WAR_Class","Prev_WAR_Class","Games_played","war_season","player_season")])

#step3
bat.step3.lm <- lm(Next_WAR ~ Age+X1B+SB+BB+Max_WAR+Max_WAR_Age+Pos_Cat+Prev_WAR_Class+war_season, data=df_bat)
summary(bat.step3.lm)

vif(bat.step3.lm)

cor(df_bat[,c("Age","X1B","SB","BB","Max_WAR","Max_WAR_Age","Pos_Cat","Prev_WAR_Class","war_season")])




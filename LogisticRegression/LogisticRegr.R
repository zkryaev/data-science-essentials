df <- read.csv("~/survey.csv",header=TRUE,sep=",")
cor.mat <- cor(df[,-1])
cor.mat
mylogit <- glm(MYDEPV ~ Income + Age + as.factor(Price) ,
               data=df,family=binomial(link="logit"),
               na.action=na.pass)
summary(mylogit)
confint(mylogit)
exp(mylogit$coefficients) # отношение шансов

df$Odds_ratio <- exp(predict(mylogit, type = "link"))
df$Prediction <- predict(mylogit, type = "response")
head(df)

observed_sums <- sum(df$MYDEPV)
predicted_sums <- sum(df$Prediction)
cat("observed_sums:\n", observed_sums, "\n")
cat("predicted_sums:\n", predicted_sums, "\n")

Price <- c(10,20,30)
Age <- c(25)
Income <- c(58)
task_pred <- data.frame(Income,Age,Price)
task_pred
task_pred$PurchaseP <- predict(mylogit,newdata=task_pred,type="response")
task_pred

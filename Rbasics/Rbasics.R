lab <- read.table("~/zipincome.txt", sep="|", header=TRUE)

summary(lab)

ds <- subset(lab, lab$meanhouseholdincome > 7000 & lab$meanhouseholdincome < 200000)

colnames(ds) <- c("zipcode", "income")
summary(ds)

income <- ds$income
zipcode <- ds$zipcode

boxplot(income ~ zipcode, data=ds, xlab="Zip Codes", ylab="Income")

boxplot(income ~ zipcode, data=ds, main="Average Household Income by Zip Code", xlab="Zip Codes", ylab="Income")

ggplot(data=ds, aes(x=as.factor(ds$zipcode), y=income)) + geom_point(position="jitter", alpha=0.2) + geom_boxplot(alpha=0.1, outlier.size=0) + scale_y_log10()

ggplot(data=ds, aes(x=as.factor(ds$zipcode), y=income)) + geom_point(aes(colour=as.factor(ds$zipcode)), position="jitter", alpha=0.2) + geom_boxplot(alpha=0.1, outlier.size=0) + ggtitle("Average Income by Zip Code") + labs(color="Region") + scale_y_log10()


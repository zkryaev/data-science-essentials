# load("path/nbtrain.csv")
df <- read.table("nbtrain.csv", header=TRUE, sep=",")
train_data <- df[1:9010, ]
test_data <- df[9011:10000, ]
# 4.5
# a)
nbc <- naiveBayes(income ~ age + sex + educ, data=train_data)
nbc$apriori
nbc$tables
# b)
predictions <- predict(nbc, newdata = test_data)
conf_matrix <- table(predictions, test_data$income)
conf_matrix
overall_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
overall_rate
rate_10_50_ <- 1 - conf_matrix["10-50K", "10-50K"] / sum(conf_matrix["10-50K", ])
rate_10_50_
rate_50_80_ <- 1 - conf_matrix["50-80K", "50-80K"] / sum(conf_matrix["50-80K", ])
rate_50_80_
rate_GT_80_ <- 1 - conf_matrix["GT 80K", "GT 80K"] / sum(conf_matrix["GT 80K", ])
rate_GT_80_
# 4.6
# a)
nb_model <- naiveBayes(sex ~ age + educ + income, data=train_data)
predictions_nb <- predict(nb_model, newdata = test_data)
conf_matrix_nb <- table(predictions_nb, test_data$sex)
conf_matrix_nb
overall_rate_nb <- 1 - sum(diag(conf_matrix_nb)) / sum(conf_matrix_nb)
overall_rate_nb
female_rate_nb <- 1 - conf_matrix_nb["F", "F"] / sum(conf_matrix_nb["F", ])
female_rate_nb
male_rate_nb <- 1 - conf_matrix_nb["M", "M"] / sum(conf_matrix_nb["M", ])
male_rate_nb
# b)
data_female <- df[df$sex == "F", ]
data_male <- df[df$sex == "M", ]
set.seed(as.numeric(Sys.time()))
sample_female <- data_female[sample(nrow(data_female), 350), ]
sample_male <- data_male[sample(nrow(data_male), 350), ]
new_train_data <- rbind(sample_female, sample_male)
nb_model_new <- naiveBayes(sex ~ age + educ + income, data = new_train_data) 
nb_model_new$apriori
nb_model_new$tables
# c,d,e)
predictions_new <- predict(nb_model_new, newdata = test_data)
conf_matrix_new <- table(predictions_new, test_data$sex)
conf_matrix_new
overall_rate_new <- 1 - sum(diag(conf_matrix_new)) / sum(conf_matrix_new)
overall_rate_new
female_rate_new <- 1 - conf_matrix_new["F", "F"] / sum(conf_matrix_new["F", ])
female_rate_new
male_rate_new <- 1 - conf_matrix_new["M", "M"] / sum(conf_matrix_new["M", ])
male_rate_new


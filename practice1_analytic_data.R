library(tidyverse)

# Loading the raw data
raw_data <- read_csv(file="practice_assignment_1_data.csv")

# Inspecting the raw data

str(raw_data)

View(raw_data)

# Loading data to include -999 as a missing value

raw_data <- read_csv(file="practice_assignment_1_data.csv",na=c("","NA","-999"))

View(raw_data)

# Creating a data set with just the categorical variables

categorical_variables <- select(raw_data,sex,major)


# Converting these variables to factors

categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1,"Female"=2)


categorical_variables$major <- as.factor(categorical_variables$major)
levels(categorical_variables$major) <- list("Psychology"=1,"Sociology"=2,"Math"=3,"Engineering"=4,"Science"=5)


# Breaking the scale items into separate data frames

self_esteem_items <- select(raw_data,SE1,SE2,SE3,SE4,SE5)
depression_items <- select(raw_data,D1,D2,D3,D4,D5)
job_satisfaction_items <- select(raw_data,JS1,JS2,JS3,JS4,JS5)

age <- select(raw_data,age)

# Checking for out of range values

psych::describe(self_esteem_items)
psych::describe(depression_items)
psych::describe(job_satisfaction_items)

is_bad_value_self_esteem <- self_esteem_items<1 | self_esteem_items>7
self_esteem_items[is_bad_value_self_esteem] <- NA

is_bad_value_depression <- depression_items<1 | depression_items>4
depression_items[is_bad_value_depression] <- NA

is_bad_value_job_satisfaction <- job_satisfaction_items<1 | job_satisfaction_items>6
job_satisfaction_items[is_bad_value_job_satisfaction] <- NA


View(self_esteem_items)
View(depression_items)
View(job_satisfaction_items)


# Reverse-scoring items

self_esteem_items <- mutate(self_esteem_items,SE1=8-SE1)
depression_items <- mutate(depression_items,D4=5-D4,D5=5-D5)
job_satisfaction_items <- mutate(job_satisfaction_items,JS1=7-JS1,JS2=7-JS2)


# Calculating mean scale scores

self_esteem <- psych::alpha(as.data.frame(self_esteem_items),check.keys = FALSE)$scores
depression <- psych::alpha(as.data.frame(depression_items),check.keys = FALSE)$scores
job_satisfaction <- psych::alpha(as.data.frame(job_satisfaction_items),check.keys = FALSE)$scores


# Combining everything into analytic_data

analytic_data <- cbind(categorical_variables,age,self_esteem,depression,job_satisfaction)
View(analytic_data)


# Saving analytic_data

save(analytic_data,file="practice1_analytic_data.RData")
write_csv(analytic_data,path="practice1_analytic_data.csv")
library(haven)
write_sav(analytic_data,path="practice1_analytic_data.sav")

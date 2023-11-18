data <- read.csv("healthcare-dataset-stroke-data.csv")
str(data)
summary(data)
#Drop Column ID
stroke = subset(data, select = -c(id))
#Transfer column bmi to number
suppressWarnings(stroke$bmi <- as.numeric(as.character(stroke$bmi)))
colSums(is.na(stroke))
#removing rows with N/A
stroke = na.omit(stroke)
# Examine column gender
as.data.frame(table(stroke$gender))
# Drop the column with 'other'.(Since there is only 1 row)
stroke = stroke[!stroke$gender == 'Other',]
as.data.frame(table(stroke$gender))
# Examine work_type
as.data.frame(table(stroke$work_type))
# Examine Residence_type
as.data.frame(table(stroke$Residence_type))
# Examine ever_married
as.data.frame(table(stroke$ever_married))
# Examine smoking
as.data.frame(table(stroke$smoking_status))
# Find null values 
colSums(is.na(stroke))
# Orginal data is stroke and we copy the data into stroke1 
# stroke1 is usefull for only Data Visualization and Logistic Regression
# stroke is usefull for Decision Tree
stroke1 = stroke

head(stroke)

# Transform binary variables to factor
stroke1$stroke <- as.factor(stroke$stroke)
stroke1$hypertension <- as.factor(stroke$hypertension)
stroke1$heart_disease <- as.factor(stroke$heart_disease)
stroke1$avg_glucose_level <- as.factor(stroke1$avg_glucose_level)
stroke1$ever_married <- as.factor(stroke$ever_married)
stroke1$Residence_type <- as.factor(stroke$Residence_type)


library(ggplot2)
#stroke on age
ggplot(data = stroke1, mapping = aes(x = stroke, y = age)) +geom_boxplot()+labs(title='Stroke on Age')
#stroke on hypertension
ggplot(data = stroke1, mapping = aes(x = stroke, y = hypertension)) +geom_boxplot()+labs(title='Stroke on HyperTension level')
#stroke on bmi
ggplot(data = stroke1, mapping = aes(x = stroke, y = bmi)) +geom_boxplot()+labs(title='Stroke on bmi')
#stroke on avg glucose level
ggplot(data = stroke1, mapping = aes(x = stroke, y = avg_glucose_level)) +geom_boxplot()+labs(title='Stroke on avg glucose level ')
#scatter plot on age and bmi with stroke 
cont.plot <- ggplot(data = stroke1, aes(x= age, y = bmi, color = stroke))+geom_point()
cont.plot

"""
# data set have 5,110 rows(observations) and 12 columns(variables)
# Total 6 numeric and 6 categoric variables  are available 
# Target variable type is numeric 
# Before summary table we need to convert categoric data into numeric data
#
# Gender variable data type converting into categoric into numeric here Male == 1 ,Female == 0
stroke$gender[stroke$gender == "Male"]   <- 1
stroke$gender[stroke$gender == "Female"] <- 0

#
#ever_married variable data type converting into categoric into numeric here Yes == 1 ,No == 0
stroke$ever_married[stroke$ever_married  == "Yes" ] <- 1
stroke$ever_married[stroke$ever_married  == "No" ] <- 0

#
#work_type variable data type converting into categoric into numeric 
# Private == 0,Self-employed == 1,Govt_job == 2,Never_worked ==3, children == 4
stroke$work_type[stroke$work_type == "Private"] <- 0
stroke$work_type[stroke$work_type == "Self-employed"] <- 1
stroke$work_type[stroke$work_type == "Govt_job"] <- 2
stroke$work_type[stroke$work_type == "Never_worked"] <- 3
stroke$work_type[stroke$work_type == "children"] <- 4

#
# Residence_type variable data type converting into categoric into numeric Urban == 0 ,Rural == 1
stroke$Residence_type[stroke$Residence_type == "Urban"] <- 1
stroke$Residence_type[stroke$Residence_type == "Rural"] <- 0

#
# smoking_status variable data type converting into categoric into numeric 
# smokes == 0,formerly smoked ==1,never smoked ==2,Unknown == 3
stroke$smoking_status[stroke$smoking_status == "smokes"] <- 2
stroke$smoking_status[stroke$smoking_status == "formerly smoked"] <- 1
stroke$smoking_status[stroke$smoking_status == "never smoked"] <- 0
stroke$smoking_status[stroke$smoking_status == "Unknown"] <- 3
"""

head(stroke)







###################################### DECISION TREE ############################################

healthcare_dataset_stroke_data$stroke[healthcare_dataset_stroke_data$stroke == '1']<- 'Yes'
healthcare_dataset_stroke_data$stroke[healthcare_dataset_stroke_data$stroke == '0']<- 'No'
View(healthcare_dataset_stroke_data)
library(rpart)
library(rpart.plot)
set.seed(356)
train = sample(1:nrow(healthcare_dataset_stroke_data), nrow(healthcare_dataset_stroke_data)*(2/3)) 
df.train = healthcare_dataset_stroke_data[train,] ;df.train
df.test = healthcare_dataset_stroke_data[-train,] ;df.test
View(df.train)
tree1 <- rpart(stroke ~ bmi+ age +gender+ever_married+work_type+Residence_type+ avg_glucose_level + hypertension + heart_disease+ smoking_status, 
               data=df.train, 
               control=rpart.control(cp=.0001,minsplit=30, minbucket = 5),
               parms = list(split= "gini"))
tree1
rpart.plot(tree1, type = 1, extra = 1)








####################################### LOGISTIC REGRESSION ##################################


# split the data into training and test data sets
set.seed(2) # for reproducible results
train <- sample(1:nrow(stroke), (0.6)*nrow(stroke))
train.df <- stroke[train,]
test.df <- stroke[-train,]

# running logistic regression
# using glm() (general linear model) with family = "binomial" to fit a logistic
logit.reg <- glm(stroke ~ .,
                 data = train.df, family = "binomial")
summary(logit.reg)



# running logistic regression
# using glm() (general linear model) with family = "binomial" to fit a logistic
logit.reg2 <- glm(stroke ~ hypertension + heart_disease + avg_glucose_level + smoking_status,
                 data = train.df, family = "binomial")
summary(logit.reg2)



# running logistic regression
# using glm() (general linear model) with family = "binomial" to fit a logistic
logit.reg3 <- glm(stroke ~ bmi + gender + hypertension + heart_disease + smoking_status + avg_glucose_level,
                 data = train.df, family = "binomial")
summary(logit.reg3)



# running logistic regression
# using glm() (general linear model) with family = "binomial" to fit a logistic
logit.reg4 <- glm(stroke ~ bmi + age+ gender + hypertension + avg_glucose_level,
                  data = train.df, family = "binomial")
summary(logit.reg4)





# running logistic regression
# using glm() (general linear model) with family = "binomial" to fit a logistic
logit.reg5 <- glm(stroke ~ bmi + smoking_status + heart_disease + hypertension + avg_glucose_level,
                  data = train.df, family = "binomial")

summary(logit.reg5)




# use predict() with type = "response" to compute predicted probabilities
# i.e., the estimated probability of an observation being in class “1”
logitPredict <- predict(logit.reg5, train.df, type = "response")

# Convert probability to a classification
# if probability > cutoff, then class = 1, otherwise class =0
logitPredictClass <- ifelse(logitPredict > 0.08, 1, 0)

# Confusion matrix
actual <- train.df$stroke
predict <- logitPredictClass
cm <- table(predict, actual)
cm



# consider class "1" as positive
tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]

# Accuracy
(tp + tn)/(tp + tn + fp + fn)

# TPR = Recall = Sensitivity
tp/(fn+tp)

# TNR = Specificity
tn/(fp+tn)

# FPR
fp/(fp+tn)

# FNR
fn/(fn+tp)

# use predict() with type = "response" to compute predicted probabilities
# i.e., the estimated probability of an observation being in class “1”
logitPredict <- predict(logit.reg5, test.df, type = "response")

# Convert probability to a classification
# if probability > cutoff, then class = 1, otherwise class =0
logitPredictClass <- ifelse(logitPredict > 0.08, 1, 0)

# Confusion matrix
actual <- test.df$stroke
predict <- logitPredictClass
cm <- table(predict, actual)
cm



# consider class "1" as positive
tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]

# Accuracy
(tp + tn)/(tp + tn + fp + fn)

# TPR = Recall = Sensitivity
tp/(fn+tp)

# TNR = Specificity
tn/(fp+tn)

# FPR
fp/(fp+tn)

# FNR
fn/(fn+tp)
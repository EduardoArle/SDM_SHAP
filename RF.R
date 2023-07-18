#load packages
library(caret);library(randomForest);library(fields)

#list work directories
wd_titanic <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/titanic'

#load data
setwd(wd_titanic)
train <- read.table('train.csv', sep=",", header= TRUE)
test <- read.table('test.csv', sep = ",", header = TRUE)

#look at the data
head(train)
head(test)

#select part of the data
table(train[,c('Survived', 'Pclass')])

#look at the distribution of data on survival per age
bplot.xy(train$Survived, train$Age)

# The box plot of age for people who survived and who didn’t is nearly the same. This means that Age of a person did not have a large effect on whether one survived or not. The y-axis is Age and the x-axis is Survived.

# Also, if you summarize it, there are lots of NA’s. So, let’s exclude the variable Age, because it doesn’t have a big impact on Survived, and because the NA’s make it hard to work with.

summary(train$Age)

bplot.xy(train$Survived, train$Fare)

# On summarizing you’ll find that there are no NA’s for Fare. So, let’s include this variable

summary(train$Fare)

# The next step is to convert Survived to a Factor data type so that caret builds a classification instead of a regression model. After that, we use a simple train command to train the model.

# Now the model is trained using the Random Forest algorithm that we discussed earlier. Random Forest is perfect for such problems because it performs numerous computations and predicts the results with high accuracy.

# Converting ‘Survived’ to a factor
train$Survived <- factor(train$Survived)
# Set a random seed
set.seed(51)

# Training using ‘random forest’ algorithm
# Survived is a function of the variables we decided to include
# Use the train data frame as the training data
# Use the 'random forest' algorithm
# Use cross-validation
# Use 5 folds for cross-validation

model <- train(Survived ~ Pclass + Sex + SibSp +
               Embarked + Parch + Fare, 
               data = train, 
               method = 'rf',
               trControl = trainControl(method = 'cv'), 
                                        number = 5) 

# To evaluate our model, we will use cross-validation scores.

# Cross-validation is used to assess the efficiency of a model by using the training data. You start by randomly dividing the training data into 5 equally sized parts called “folds”. Next, you train the model on 4/5 of the data, and check its accuracy on the 1/5 of the data you left out. You then repeat this process with each split of the data. In the end, you average the percentage accuracy across the five different splits of the data to get an average accuracy. Caret does this for you, and you can see the scores by looking at the model output:

model

# The first thing to notice is where it says, “The final value used for the model was mtry = 5.” The “mtry” is a hyper-parameter of the random forest model that determines how many variables the model uses to split the trees.

# The table shows different values of mtry along with their corresponding average accuracy under cross-validation. Caret automatically picks the value of the hyper-parameter “mtry” that is the most accurate under cross-validation.

# In the output, with mtry = 5, the average accuracy is 0.8170964, or about 82 percent. Which is the highest value, hence Caret picks this value for us.

# Before we predict the output for the test data, let’s check if there is any missing data in the variables we are using to predict. If Caret finds any missing values, it will not return a prediction at all. So, we must find the missing data before moving ahead:

summary(test)

# Notice the variable “Fare” has one NA value. Let’s fill in that value with the mean of the “Fare” column. We use an if-else statement to do this.

# So, if an entry in the column “Fare” is NA, then replace it with the mean of the column and remove the NA’s when you take the mean:

test$Fare <- ifelse(is.na(test$Fare), mean(test$Fare, na.rm = TRUE), test$Fare)

# Now, our final step is to make predictions on the test set. To do this, you just have to call the predict method on the model object you trained. Let’s make the predictions on the test set and add them as a new column.

test$Survived <- predict(model, newdata = test)

# Finally, it outputs the predictions for the test data,

test$Survived

# Here you can see the “Survived” values (either 0 or 1) for each passenger. Where one stands for survived and 0 stands for died. This prediction is made based on the “pclass” and “Fare” variables. You can use other variables too, if they are somehow related to whether a person boarding the titanic will survive or not.


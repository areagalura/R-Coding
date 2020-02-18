##################################
#####KAMILLE ANDREA L. GALURA#####
########## R FINAL EXAM ##########
##################################

###Question A###
#lambda rate = 0.2
#1. Simulate 5000 observations 
library(MASS)
mean(rexp(5000, rate = 0.2))

#2. Actual mean and variance of the data 
summary(Cars93)
show(Cars93)
View(Cars93)
str(Cars93)

#3. Probabilities for the ff: >4, <2, >6
exp(0.2*4)#probability of train >4 with lambda=0.2 (2.23)
1-exp(0.2*2) #probability of train <2 with lambda=0.2 (-0.49)
exp(0.2*6)#probability of train >6 with lambda=0.2 (3.32)


###Question B###
#build a logistic regression to predict a patient having an ulcer 
library(MASS)
summary(Melanoma)
show(Melanoma)
#RUB the data using replace function
mydf$purpose <- as.numeric(replace(mydf$purpose, "", "x"))[1:1000]
#RUB the data using gsub
mydf$good_bad <- gsub("good", "1", mydf$good_bad)
mydf$good_bad <- gsub("bad", "0", mydf$good_bad)
mydf$good_bad <- as.numeric(mydf$good_bad)

#1. Split data into training and testing (use stratified sampling with splitratio=0.7 for training)
train_index <- sample(1:nrow(Melanoma), size=205) #random sample of 205 indeces
test_index <- c(1:nrow(Melanoma))[-train_index] ##taking out the train index from dataframe

#2. Create a logistic regression and analyze the model. How did you go about designing the model?  
install.packages("caTools")
library(caTools) 
train_index <- sample.split(Melanoma$good_bad, SplitRatio=0.7)
test_index <- c(1:nrow(Melanoma))[-train_index] ##taking out the train index from dataframe
melanoma_train <- Melanoma[train_index,]
melanoma_test <- Melanoma[test_index,]

melanoma_mod <- glm(good_bad ~ age, data=Melanoma, family="binomial")
summary(melanoma_mod) #for every year, increase in Age coefficient at 0.018

melanoma_mod <- glm(good_bad ~ age+time+status+sex+year+thickness+ulcer, data=melanoma_train, family="binomial")
summary(melanoma_mod)

#using the model to predict the 1/0 var in the test dataset with stratified sampling
test$response <- predict(melanoma_mod, test, type="response")
View(test)

#3. What business insight can you get?
# increase of melanoma at 0.018 for every year as Age increases 


###Question C###
#1. Build a classification decision tree (as a challenger model for the logistic regression in the previosu exercise)
#use the same testing and training data
library(MASS)
library(rpart)
library(rpart.plot)
library(MASS)
summary(Melanoma)

my_log_melanoma <- glm(good_bad ~ age+time+status+sex+year+thickness+ulcer, data=melanoma_train, family="binomial")
summary(my_log_melanoma) 
predict(my_log_melanoma, melanoma_train, type="response") # closer to 1 survive, 0 death, middle- unknown

#2. Based on the cp metric, prune or grow your tree
melanoma_tree <- rpart(good_bad ~ age+time+status+sex+year+thickness+ulcer, data=melanoma_train, method="class", cp=0.011)
rpart.plot(melanoma_tree, type=1, extra=1) #initial tree already optimized 
plotcp(melanoma_tree) #grow and prune(generalize) the tree, increase the fit so it won't be too general

#3. What insight does the tree provide (split variables and leaves)?

#4. Compare the tree and logistic regression (LOGIT IS BETTER)
#titanic_tree vs my_log_tit
predict_logit <- predict(my_log_melanoma, melanoma_train, type="response")
val_1 <- predict(melanoma_tree, melanoma_train, type="prob")
install.packages("ROCR")
library(ROCR)
pred_val <- prediction(val_1[,2], melanoma_train$good_bad)
pred_val_logit <- prediction(predict_logit, melanoma_train$good_bad)
perf <- performance(pred_val, "tpr", "fpr") #true positive rate, false positive rate
perf_logit <- performance(pred_val_logit, "tpr", "fpr")
#plot
plot(perf, col="black")
plot(perf_logit, col="blue", add=T) #Logit is better (BLUE), even if Tree is more appealing

###Question D###
#create shiny dashboard with 1 plotly scatterplot that shows relationship between two variables, decision tree from Question 2.3 

###Question E###
#investor allocates 25% each for the 4 companies 
#1. Calculate metrics for each company 
#Company 1 ROA = .25, ROE= 1, Profit Margin=.2
#Company 2 ROA = .13, ROE= .3, Profit Margin=.18
#Company 3 ROA = .29, ROE= .5, Profit Margin=.29
#Company 4 ROA = 4, ROE= 5, Profit Margin=.57

#2. create a user defined function that will output a weighted average 
weighted_ROA=(.25*.25)+(.13*.25)+(.29*.25)+(4*.25) #.0625+.0325+.0725+1=2.675
weighted_ROE=(1*.25)+(.3*.25)+(.5*.25)+(5*.25) #.25+.075+1.25+.125=1.7
weighted_Profit_margin=(.2*.25)+(.18*.25)+(.29*.25)+(.57*.25) #.05+.045+.0725+.1425=.31

#3. How does your average ROA ROE and Profit Margin change if you allocate money the following way? 20 20 35 25
weighted_ROA=(.25*.2)+(.13*.2)+(.29*.35)+(4*.25) #.5+.026+.1015+1=1.6275
weighted_ROE=(1*.2)+(.3*.2)+(.5*.35)+(5*.25) #.2+.06+.175+.125=.56
weighted_Profit_margin=(.2*.2)+(.18*.2)+(.29*.35)+(.57*.25) #.04+.036+.1015+.1425=.32

#4. What is the business insight?
#The 5% allocation taken away from Company 1and 2 dramatically decreased the ROE and ROA weighted average compared to Profit Margin that only had a .01 decrease. 

#5. How would your average ratios change if you removed 15% from Company 1 and added it to Company 4?
weighted_ROA=(.25*.05)+(.13*.2)+(.29*.35)+(4*.4) #.0125+.026+.1015+1.6=1.74
weighted_ROE=(1*.05)+(.3*.2)+(.5*.35)+(5*.4) #.05+.06+.175+2=2.285
weighted_Profit_margin=(.2*.05)+(.18*.2)+(.29*.35)+(.57*.4) #.01+.036+.1015+.228=.3755
#there is a huge increase in ROE (1.73) and only small increases in ROA (.11) and Profit Margin (.06)
#when there is an allocation of 5% to Company A, 20% to Company B, 35% to Company C and 40% to Company D

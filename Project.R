#Logistic Regression

#Model Selection and validation 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Model selection based on z-test with undersampling the majority class

#Criterion of selection: non-significant counters

library(ROSE)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

#-------------------------------------------------------------------------------

#First iteration with all the regressors

N_rep <- 500
mean_pvalues <- rep(0,38)
pvalues <- rep(0,38)
counters <- rep(0,38)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~. , data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:38){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"AgeCategory25-29" is the least significant regressor (500 times)

#-------------------------------------------------------------------------------

#Second iteration without "AgeCategory25-29" regressor

age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,37)
pvalues <- rep(0,37)
counters <- rep(0,37)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~., contrasts = list("AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:37){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"RaceOther" is the least significant regressor (500 times)

#-------------------------------------------------------------------------------

#Third iteration without "RaceOther" and "AgeCategory25-29" regressors

race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,36)
pvalues <- rep(0,36)
counters <- rep(0,36)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~., 
                        contrasts = list("Race"=race_dummy, "AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:36){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"PhysicalActivityYes" is the least significant regressor (498 times)

#-------------------------------------------------------------------------------

#Fourth iteration without "PhysicalActivityYes", "RaceOther" and "AgeCategory25-29" regressors

race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,35)
pvalues <- rep(0,35)
counters <- rep(0,35)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Race"=race_dummy, "AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:35){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"DiabeticYes (during pregnancy)" is the least significant regressor (495 times)

#-------------------------------------------------------------------------------

#Fifth iteration without "DiabeticYes (during pregnancy)", "PhysicalActivityYes", "RaceOther"

#and "AgeCategory25-29" regressors

diabetic_dummy <-contrasts(DS$Diabetic)
diabetic_dummy<- diabetic_dummy[,-3]
race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,34)
pvalues <- rep(0,34)
counters <- rep(0,34)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Diabetic"=diabetic_dummy, "Race"=race_dummy, 
                                         "AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:34){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"RaceWhite" is the least significant regressor (483 times)

#-------------------------------------------------------------------------------

#Sixth iteration without "RaceWhite", "DiabeticYes (during pregnancy)", "PhysicalActivityYes", 

#"RaceOther"and "AgeCategory25-29" regressors

race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-4]
diabetic_dummy <-contrasts(DS$Diabetic)
diabetic_dummy<- diabetic_dummy[,-3]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,33)
pvalues <- rep(0,33)
counters <- rep(0,33)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Diabetic"=diabetic_dummy, "Race"=race_dummy, 
                                         "AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:33){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"RaceHispanic" is the least significant regressor (415 times)

#-------------------------------------------------------------------------------

#Seventh iteration without "RaceHispanic", "RaceWhite", "DiabeticYes (during pregnancy)", 

#"PhysicalActivityYes", "RaceOther"and "AgeCategory25-29" regressors

race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-3]
diabetic_dummy <-contrasts(DS$Diabetic)
diabetic_dummy<- diabetic_dummy[,-3]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,32)
pvalues <- rep(0,32)
counters <- rep(0,32)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Diabetic"=diabetic_dummy, "Race"=race_dummy, 
                                         "AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:32){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#"DiabeticNo, borderline diabetes" is the least significant regressor (340 times)

#-------------------------------------------------------------------------------

#Octave iteration without "DiabeticNo, borderline diabetes", "RaceHispanic", "RaceWhite",

#"DiabeticYes (during pregnancy)", "PhysicalActivityYes", "RaceOther"and "AgeCategory25-29" regressors

diabetic_dummy <-contrasts(DS$Diabetic)
diabetic_dummy<- diabetic_dummy[,-3]
diabetic_dummy<- diabetic_dummy[,-1]
diabetic_dummy <- t(diabetic_dummy)
diabetic_dummy <- t(diabetic_dummy)
colnames(diabetic_dummy) <- "Yes"
race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-3]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_pvalues <- rep(0,31)
pvalues <- rep(0,31)
counters <- rep(0,31)
for (i in 1:N_rep){
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Diabetic"=diabetic_dummy, "Race"=race_dummy, 
                                         "AgeCategory"= age_category_dummy),
                        data = balanced_DS, family = binomial)
  pvalues <- summary(logistic_model)$coefficients[,4]
  for(j in 1:31){
    if(pvalues[j]>=0.01){
      counters[j]=counters[j]+1
    }
  }
}
names(counters) <- names(pvalues)
counters <- sort(counters, decreasing = T)
if(counters[1] >= 300){
  print(counters[1])
}else{
  print("All remaining regressors must stay in the model")
}

#All remaining regressors must stay in the model

#End of model selection

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Model validation with dataset's outliers

library(ROSE)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

#-------------------------------------------------------------------------------

#First validation with classification threshold = 0.5

diabetic_dummy <-contrasts(DS$Diabetic)
diabetic_dummy<- diabetic_dummy[,-3]
diabetic_dummy<- diabetic_dummy[,-1]
diabetic_dummy <- t(diabetic_dummy)
diabetic_dummy <- t(diabetic_dummy)
colnames(diabetic_dummy) <- "Yes"
race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-3]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_train_error_rate <- 0
mean_train_false_positive_rate <- 0
mean_train_false_negative_rate <- 0
mean_test_error_rate <- 0
mean_test_false_positive_rate <- 0
mean_test_false_negative_rate <- 0

for (i in 1:N_rep){
  
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  train_indexes <- sample (dim(balanced_DS)[1], dim(balanced_DS)[1]*(0.7), replace = FALSE)
  train_indexes <-sort(train_indexes)
  
  train_DS <- balanced_DS[train_indexes, ]
  test_DS <- balanced_DS[-train_indexes, ]
  
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Race"=race_dummy, "AgeCategory"= age_category_dummy,
                                         "Diabetic"=diabetic_dummy),
                        data = train_DS, family = binomial)
  
  #train error
  
  train_prediction <- predict(logistic_model, newdata = train_DS, type = "response")
  
  class <- rep ( "No" , dim(train_DS)[1])
  class[train_prediction > .5] = "Yes";
  table(class, train_DS$HeartDisease)
  
  tn<-table(class, train_DS$HeartDisease)[1]
  fp<-table(class, train_DS$HeartDisease)[2]
  fn<-table(class, train_DS$HeartDisease)[3]
  tp<-table(class, train_DS$HeartDisease)[4]
  
  train_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_train_error_rate <- mean_train_error_rate+train_error_rate
  
  train_false_positive_rate <-fp/(tn+fp)
  mean_train_false_positive_rate <- mean_train_false_positive_rate+train_false_positive_rate
  
  train_false_negative_rate <-fn/(tp+fn)
  mean_train_false_negative_rate <- mean_train_false_negative_rate+train_false_negative_rate
  
  #test error
  
  test_prediction <- predict(logistic_model, newdata = test_DS, type = "response")
  
  class <- rep ( "No" , dim(test_DS)[1])
  class[test_prediction > .5] = "Yes";
  table(class, test_DS$HeartDisease)
  
  tn<-table(class, test_DS$HeartDisease)[1]
  fp<-table(class, test_DS$HeartDisease)[2]
  fn<-table(class, test_DS$HeartDisease)[3]
  tp<-table(class, test_DS$HeartDisease)[4]
  
  test_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_test_error_rate<-mean_test_error_rate+test_error_rate
  
  test_false_positive_rate <-fp/(tn+fp)
  mean_test_false_positive_rate <- mean_test_false_positive_rate+test_false_positive_rate
  
  test_false_negative_rate <-fn/(tp+fn)
  mean_test_false_negative_rate <- mean_test_false_negative_rate+test_false_negative_rate
}
mean_train_error_rate <- mean_train_error_rate/N_rep
mean_train_false_positive_rate <- mean_train_false_positive_rate/N_rep
mean_train_false_negative_rate <- mean_train_false_negative_rate/N_rep

mean_test_error_rate <- mean_test_error_rate/N_rep
mean_test_false_positive_rate <- mean_test_false_positive_rate/N_rep
mean_test_false_negative_rate <- mean_test_false_negative_rate/N_rep

mean_train_error_rate
mean_train_false_positive_rate
mean_train_false_negative_rate

mean_test_error_rate
mean_test_false_positive_rate
mean_test_false_negative_rate

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Model validation without dataset's outliers

library(ROSE)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

#Full dataset without outliers (unbalanced)

no_outliers_DS<-heart_2020_cleaned

alpha = 1.5
BMI_Qs <- quantile(DS$BMI, c(.25, .75))
BMI_IQR <- IQR(DS$BMI)
BMI_cond <- DS$BMI > (BMI_Qs[1] - alpha*BMI_IQR) &
DS$BMI < (BMI_Qs[2] + alpha*BMI_IQR)
PhysicalH_Qs <- quantile(DS$PhysicalHealth, c(.25, .75))
PhysicalH_IQR <- IQR(DS$PhysicalHealth)
PhysicalH_cond <- DS$PhysicalHealth > (PhysicalH_Qs[1] - alpha*PhysicalH_IQR) &
DS$PhysicalHealth < (PhysicalH_Qs[2] + alpha*PhysicalH_IQR)
MentalH_Qs <- quantile(DS$MentalHealth, c(.25, .75))
MentalH_IQR <- IQR(DS$MentalHealth)
MentalH_cond <- DS$MentalHealth > (MentalH_Qs[1] - alpha*MentalH_IQR) &
DS$MentalHealth < (MentalH_Qs[2] + alpha*MentalH_IQR)
SleepT_Qs <- quantile(DS$SleepTime, c(.25, .75))
SleepT_IQR <- IQR(DS$SleepTime)
SleepT_cond <- DS$SleepTime > (SleepT_Qs[1] - alpha*SleepT_IQR) &
DS$SleepTime < (SleepT_Qs[2] + alpha*SleepT_IQR)
no_outliers_DS <- subset(DS, BMI_cond & PhysicalH_cond & MentalH_cond & SleepT_cond)

no_outliers_rows<-dim(no_outliers_DS)[1]
no_outliers_no_istances<-table(no_outliers_DS$HeartDisease)[1]
no_outliers_yes_istances<-table(no_outliers_DS$HeartDisease)[2]
no_outliers_no_percentage<-(no_outliers_no_istances/no_outliers_rows)*100
no_outliers_yes_percentage<-(no_outliers_yes_istances/no_outliers_rows)*100

#-------------------------------------------------------------------------------

#First validation with classification threshold = 0.5

diabetic_dummy <-contrasts(DS$Diabetic)
diabetic_dummy<- diabetic_dummy[,-3]
diabetic_dummy<- diabetic_dummy[,-1]
diabetic_dummy <- t(diabetic_dummy)
diabetic_dummy <- t(diabetic_dummy)
colnames(diabetic_dummy) <- "Yes"
race_dummy <-contrasts(DS$Race)
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-4]
race_dummy <- race_dummy[,-3]
age_category_dummy <-contrasts(DS$AgeCategory)
age_category_dummy <- age_category_dummy[,-1]

N_rep <- 500
mean_train_error_rate <- 0
mean_train_false_positive_rate <- 0
mean_train_false_negative_rate <- 0
mean_test_error_rate <- 0
mean_test_false_positive_rate <- 0
mean_test_false_negative_rate <- 0

for (i in 1:N_rep){
  
  balanced_DS <- ovun.sample(HeartDisease ~. , data = no_outliers_DS, method = "under", N=no_outliers_yes_istances*2)$data
  train_indexes <- sample (dim(balanced_DS)[1], dim(balanced_DS)[1]*(0.7), replace = FALSE)
  train_indexes <-sort(train_indexes)
  
  train_DS <- balanced_DS[train_indexes, ]
  test_DS <- balanced_DS[-train_indexes, ]
  
  logistic_model <- glm(HeartDisease ~. -PhysicalActivity,
                        contrasts = list("Race"=race_dummy, "AgeCategory"= age_category_dummy,
                                         "Diabetic"=diabetic_dummy),
                        data = train_DS, family = binomial)
  
  #train error
  
  train_prediction <- predict(logistic_model, newdata = train_DS, type = "response")
  
  class <- rep ( "No" , dim(train_DS)[1])
  class[train_prediction > .5] = "Yes";
  table(class, train_DS$HeartDisease)
  
  tn<-table(class, train_DS$HeartDisease)[1]
  fp<-table(class, train_DS$HeartDisease)[2]
  fn<-table(class, train_DS$HeartDisease)[3]
  tp<-table(class, train_DS$HeartDisease)[4]
  
  train_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_train_error_rate <- mean_train_error_rate+train_error_rate
  
  train_false_positive_rate <-fp/(tn+fp)
  mean_train_false_positive_rate <- mean_train_false_positive_rate+train_false_positive_rate
  
  train_false_negative_rate <-fn/(tp+fn)
  mean_train_false_negative_rate <- mean_train_false_negative_rate+train_false_negative_rate
  
  #test error
  
  test_prediction <- predict(logistic_model, newdata = test_DS, type = "response")
  
  class <- rep ( "No" , dim(test_DS)[1])
  class[test_prediction > .5] = "Yes";
  table(class, test_DS$HeartDisease)
  
  tn<-table(class, test_DS$HeartDisease)[1]
  fp<-table(class, test_DS$HeartDisease)[2]
  fn<-table(class, test_DS$HeartDisease)[3]
  tp<-table(class, test_DS$HeartDisease)[4]
  
  test_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_test_error_rate<-mean_test_error_rate+test_error_rate
  
  test_false_positive_rate <-fp/(tn+fp)
  mean_test_false_positive_rate <- mean_test_false_positive_rate+test_false_positive_rate
  
  test_false_negative_rate <-fn/(tp+fn)
  mean_test_false_negative_rate <- mean_test_false_negative_rate+test_false_negative_rate
}
mean_train_error_rate <- mean_train_error_rate/N_rep
mean_train_false_positive_rate <- mean_train_false_positive_rate/N_rep
mean_train_false_negative_rate <- mean_train_false_negative_rate/N_rep

mean_test_error_rate <- mean_test_error_rate/N_rep
mean_test_false_positive_rate <- mean_test_false_positive_rate/N_rep
mean_test_false_negative_rate <- mean_test_false_negative_rate/N_rep

mean_train_error_rate
mean_train_false_positive_rate
mean_train_false_negative_rate

mean_test_error_rate
mean_test_false_positive_rate
mean_test_false_negative_rate

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Performance of LDA model with dataset's outliers

library(ROSE)
library(MASS)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

#LDA (common SIGMA)

N_rep <- 500
mean_train_error_rate <- 0
mean_train_false_positive_rate <- 0
mean_train_false_negative_rate <- 0
mean_test_error_rate <- 0
mean_test_false_positive_rate <- 0
mean_test_false_negative_rate <- 0

for (i in 1:N_rep){
  
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  train_indexes <- sample (dim(balanced_DS)[1], dim(balanced_DS)[1]*(0.7), replace = FALSE)
  train_indexes <-sort(train_indexes)
  
  train_DS <- balanced_DS[train_indexes, ]
  test_DS <- balanced_DS[-train_indexes, ]
  
  lda_model <- lda(HeartDisease ~., data = train_DS)
  
  #train error
  
  train_prediction <- predict(lda_model, newdata = train_DS)
  table(train_prediction$class, train_DS$HeartDisease)
  
  tn<-table(train_prediction$class, train_DS$HeartDisease)[1]
  fp<-table(train_prediction$class, train_DS$HeartDisease)[2]
  fn<-table(train_prediction$class, train_DS$HeartDisease)[3]
  tp<-table(train_prediction$class, train_DS$HeartDisease)[4]
  
  train_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_train_error_rate <- mean_train_error_rate+train_error_rate
  
  train_false_positive_rate <-fp/(tn+fp)
  mean_train_false_positive_rate <- mean_train_false_positive_rate+train_false_positive_rate
  
  train_false_negative_rate <-fn/(tp+fn)
  mean_train_false_negative_rate <- mean_train_false_negative_rate+train_false_negative_rate
  
  #test error
  
  test_prediction <- predict(lda_model, newdata = test_DS)
  table(test_prediction$class, test_DS$HeartDisease)
  
  tn<-table(test_prediction$class, test_DS$HeartDisease)[1]
  fp<-table(test_prediction$class, test_DS$HeartDisease)[2]
  fn<-table(test_prediction$class, test_DS$HeartDisease)[3]
  tp<-table(test_prediction$class, test_DS$HeartDisease)[4]
  
  test_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_test_error_rate<-mean_test_error_rate+test_error_rate
  
  test_false_positive_rate <-fp/(tn+fp)
  mean_test_false_positive_rate <- mean_test_false_positive_rate+test_false_positive_rate
  
  test_false_negative_rate <-fn/(tp+fn)
  mean_test_false_negative_rate <- mean_test_false_negative_rate+test_false_negative_rate
}
mean_train_error_rate <- mean_train_error_rate/N_rep
mean_train_false_positive_rate <- mean_train_false_positive_rate/N_rep
mean_train_false_negative_rate <- mean_train_false_negative_rate/N_rep

mean_test_error_rate <- mean_test_error_rate/N_rep
mean_test_false_positive_rate <- mean_test_false_positive_rate/N_rep
mean_test_false_negative_rate <- mean_test_false_negative_rate/N_rep

print("LDA results: ")

mean_train_error_rate
mean_train_false_positive_rate
mean_train_false_negative_rate

mean_test_error_rate
mean_test_false_positive_rate
mean_test_false_negative_rate

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Performance of QDA model with dataset's outliers

library(ROSE)
library(MASS)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

#QDA (single SIGMA)

N_rep <- 500
mean_train_error_rate <- 0
mean_train_false_positive_rate <- 0
mean_train_false_negative_rate <- 0
mean_test_error_rate <- 0
mean_test_false_positive_rate <- 0
mean_test_false_negative_rate <- 0

for (i in 1:N_rep){
  
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  train_indexes <- sample (dim(balanced_DS)[1], dim(balanced_DS)[1]*(0.7), replace = FALSE)
  train_indexes <-sort(train_indexes)
  
  train_DS <- balanced_DS[train_indexes, ]
  test_DS <- balanced_DS[-train_indexes, ]
  
  qda_model <- qda(HeartDisease ~., data = train_DS)
  
  #train error
  
  train_prediction <- predict(qda_model, newdata = train_DS)
  table(train_prediction$class, train_DS$HeartDisease)
  
  tn<-table(train_prediction$class, train_DS$HeartDisease)[1]
  fp<-table(train_prediction$class, train_DS$HeartDisease)[2]
  fn<-table(train_prediction$class, train_DS$HeartDisease)[3]
  tp<-table(train_prediction$class, train_DS$HeartDisease)[4]
  
  train_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_train_error_rate <- mean_train_error_rate+train_error_rate
  
  train_false_positive_rate <-fp/(tn+fp)
  mean_train_false_positive_rate <- mean_train_false_positive_rate+train_false_positive_rate
  
  train_false_negative_rate <-fn/(tp+fn)
  mean_train_false_negative_rate <- mean_train_false_negative_rate+train_false_negative_rate
  
  #test error
  
  test_prediction <- predict(qda_model, newdata = test_DS)
  table(test_prediction$class, test_DS$HeartDisease)
  
  tn<-table(test_prediction$class, test_DS$HeartDisease)[1]
  fp<-table(test_prediction$class, test_DS$HeartDisease)[2]
  fn<-table(test_prediction$class, test_DS$HeartDisease)[3]
  tp<-table(test_prediction$class, test_DS$HeartDisease)[4]
  
  test_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_test_error_rate<-mean_test_error_rate+test_error_rate
  
  test_false_positive_rate <-fp/(tn+fp)
  mean_test_false_positive_rate <- mean_test_false_positive_rate+test_false_positive_rate
  
  test_false_negative_rate <-fn/(tp+fn)
  mean_test_false_negative_rate <- mean_test_false_negative_rate+test_false_negative_rate
}
mean_train_error_rate <- mean_train_error_rate/N_rep
mean_train_false_positive_rate <- mean_train_false_positive_rate/N_rep
mean_train_false_negative_rate <- mean_train_false_negative_rate/N_rep

mean_test_error_rate <- mean_test_error_rate/N_rep
mean_test_false_positive_rate <- mean_test_false_positive_rate/N_rep
mean_test_false_negative_rate <- mean_test_false_negative_rate/N_rep

print("QDA results: ")

mean_train_error_rate
mean_train_false_positive_rate
mean_train_false_negative_rate

mean_test_error_rate
mean_test_false_positive_rate
mean_test_false_negative_rate

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Performance of Naive Bayes model with dataset's outliers

library(ROSE)
library(MASS)
library(e1071)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

#Naive Bayes (X iid)

N_rep <- 500
mean_train_error_rate <- 0
mean_train_false_positive_rate <- 0
mean_train_false_negative_rate <- 0
mean_test_error_rate <- 0
mean_test_false_positive_rate <- 0
mean_test_false_negative_rate <- 0

for (i in 1:N_rep){
  
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  train_indexes <- sample (dim(balanced_DS)[1], dim(balanced_DS)[1]*(0.7), replace = FALSE)
  train_indexes <-sort(train_indexes)
  
  train_DS <- balanced_DS[train_indexes, ]
  test_DS <- balanced_DS[-train_indexes, ]
  
  nb_model <- naiveBayes(HeartDisease ~., data = train_DS)
  
  #train error
  
  train_prediction <- predict(nb_model, newdata = train_DS)
  table(train_prediction, train_DS$HeartDisease)
  
  tn<-table(train_prediction, train_DS$HeartDisease)[1]
  fp<-table(train_prediction, train_DS$HeartDisease)[2]
  fn<-table(train_prediction, train_DS$HeartDisease)[3]
  tp<-table(train_prediction, train_DS$HeartDisease)[4]
  
  train_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_train_error_rate <- mean_train_error_rate+train_error_rate
  
  train_false_positive_rate <-fp/(tn+fp)
  mean_train_false_positive_rate <- mean_train_false_positive_rate+train_false_positive_rate
  
  train_false_negative_rate <-fn/(tp+fn)
  mean_train_false_negative_rate <- mean_train_false_negative_rate+train_false_negative_rate
  
  #test error
  
  test_prediction <- predict(nb_model, newdata = test_DS)
  table(test_prediction, test_DS$HeartDisease)
  
  tn<-table(test_prediction, test_DS$HeartDisease)[1]
  fp<-table(test_prediction, test_DS$HeartDisease)[2]
  fn<-table(test_prediction, test_DS$HeartDisease)[3]
  tp<-table(test_prediction, test_DS$HeartDisease)[4]
  
  test_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_test_error_rate<-mean_test_error_rate+test_error_rate
  
  test_false_positive_rate <-fp/(tn+fp)
  mean_test_false_positive_rate <- mean_test_false_positive_rate+test_false_positive_rate
  
  test_false_negative_rate <-fn/(tp+fn)
  mean_test_false_negative_rate <- mean_test_false_negative_rate+test_false_negative_rate
}
mean_train_error_rate <- mean_train_error_rate/N_rep
mean_train_false_positive_rate <- mean_train_false_positive_rate/N_rep
mean_train_false_negative_rate <- mean_train_false_negative_rate/N_rep

mean_test_error_rate <- mean_test_error_rate/N_rep
mean_test_false_positive_rate <- mean_test_false_positive_rate/N_rep
mean_test_false_negative_rate <- mean_test_false_negative_rate/N_rep

print("NB results: ")

mean_train_error_rate
mean_train_false_positive_rate
mean_train_false_negative_rate

mean_test_error_rate
mean_test_false_positive_rate
mean_test_false_negative_rate

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Performance of KNN model with dataset's outliers

library(ROSE)
library(MASS)
library(e1071)
library(class)

#Full dataset (unbalanced)

DS<-heart_2020_cleaned
rows<-dim(DS)[1]
no_istances<-table(DS$HeartDisease)[1]
yes_istances<-table(DS$HeartDisease)[2]
no_percentage<-(no_istances/rows)*100
yes_percentage<-(yes_istances/rows)*100

# KNN - non parametric method

N_rep <- 50
mean_train_error_rate <- 0
mean_train_false_positive_rate <- 0
mean_train_false_negative_rate <- 0
mean_test_error_rate <- 0
mean_test_false_positive_rate <- 0
mean_test_false_negative_rate <- 0

for (i in 1:N_rep){
  
  balanced_DS <- ovun.sample(HeartDisease ~. , data = DS, method = "under", N=yes_istances*2)$data
  
  train_indexes <- sample (dim(balanced_DS)[1], dim(balanced_DS)[1]*(0.7), replace = FALSE)
  train_indexes <-sort(train_indexes)
  
  matrix<-cbind(balanced_DS$BMI, balanced_DS$Smoking, balanced_DS$AlcoholDrinking, 
                balanced_DS$Stroke, balanced_DS$PhysicalHealth, balanced_DS$MentalHealth, 
                balanced_DS$DiffWalking, balanced_DS$Sex, balanced_DS$AgeCategory,
                balanced_DS$Race, balanced_DS$Diabetic, balanced_DS$PhysicalActivity,
                balanced_DS$GenHealth, balanced_DS$SleepTime, balanced_DS$Asthma, 
                balanced_DS$KidneyDisease, balanced_DS$SkinCancer)

  matrix <-scale(matrix)
  
  data_train <- matrix[train_indexes, ]
  data_test <- matrix[-train_indexes, ]
  label_train <- balanced_DS$HeartDisease[train_indexes]; 

  #train error
  
  knn_model <- knn(data_train,data_train,label_train, k=30)
  
  table(knn_model, balanced_DS$HeartDisease[train_indexes])
  
  tn<-table(knn_model, balanced_DS$HeartDisease[train_indexes])[1]
  fp<-table(knn_model, balanced_DS$HeartDisease[train_indexes])[2]
  fn<-table(knn_model, balanced_DS$HeartDisease[train_indexes])[3]
  tp<-table(knn_model, balanced_DS$HeartDisease[train_indexes])[4]
  
  train_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_train_error_rate <- mean_train_error_rate+train_error_rate
  
  train_false_positive_rate <-fp/(tn+fp)
  mean_train_false_positive_rate <- mean_train_false_positive_rate+train_false_positive_rate
  
  train_false_negative_rate <-fn/(tp+fn)
  mean_train_false_negative_rate <- mean_train_false_negative_rate+train_false_negative_rate
  
  #test error
  
  knn_model <- knn(data_train,data_test,label_train, k=30)

  table(knn_model, balanced_DS$HeartDisease[-train_indexes])
  
  tn<-table(knn_model, balanced_DS$HeartDisease[-train_indexes])[1]
  fp<-table(knn_model, balanced_DS$HeartDisease[-train_indexes])[2]
  fn<-table(knn_model, balanced_DS$HeartDisease[-train_indexes])[3]
  tp<-table(knn_model, balanced_DS$HeartDisease[-train_indexes])[4]
  
  test_error_rate <-(fn+fp)/(fn+fp+tn+tp)
  mean_test_error_rate<-mean_test_error_rate+test_error_rate
  
  test_false_positive_rate <-fp/(tn+fp)
  mean_test_false_positive_rate <- mean_test_false_positive_rate+test_false_positive_rate
  
  test_false_negative_rate <-fn/(tp+fn)
  mean_test_false_negative_rate <- mean_test_false_negative_rate+test_false_negative_rate
}
mean_train_error_rate <- mean_train_error_rate/N_rep
mean_train_false_positive_rate <- mean_train_false_positive_rate/N_rep
mean_train_false_negative_rate <- mean_train_false_negative_rate/N_rep

mean_test_error_rate <- mean_test_error_rate/N_rep
mean_test_false_positive_rate <- mean_test_false_positive_rate/N_rep
mean_test_false_negative_rate <- mean_test_false_negative_rate/N_rep

print("KNN results: ")

mean_train_error_rate
mean_train_false_positive_rate
mean_train_false_negative_rate

mean_test_error_rate
mean_test_false_positive_rate
mean_test_false_negative_rate

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



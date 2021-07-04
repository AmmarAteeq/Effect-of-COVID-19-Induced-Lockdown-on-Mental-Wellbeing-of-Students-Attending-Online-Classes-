install.packages("plotly")
install.packages("parsnip")
install.packages("tidyverse")
install.packages("yardstick")
install.packages("rsample")
install.packages("recipes")
install.packages("ranger")
install.packages("caret")
install.packages("reprtree")
install.packages("Metrics")


# Load libraries
library(tidyverse)
library("readxl")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(plotly)
library(forcats)
library(data.table)
library(parsnip)
library(tidyverse)
library(yardstick)
library(rpart)
library(rpart.plot)
library(rsample)
library(recipes)
library(ranger)
library(caret)
library(party)


# read file
df_data = read_excel('A Short Survey About Online Studies And Its Effects (Responses).xlsx')
df_data = read_excel('A Short Survey About Online Studies And Its Effects (Responses) (1).xlsx')


# change column names to a more understandable variable that are recieved from the form
df_data = df_data %>% 
  rename(
    age = `What is your age?`,
    country = `Which country are you from?`,
    study_in_germany = `Do you study in Germany?`,
    country_study = `Which country were you in while taking online classes?`,
    living_setup = `What is your living setup?`,
    motivation = `Were you motivated to take online classes as compared to regular classes?`,
    motivation_reason = `Please give reason for your previous question's answer below:`,
    concentration = `How has been your concentration span during online studies vs. in-person classes/lectures?`,
    concentration_reason = `Please share some thoughts or explanation on the concentration span in online studies.`,
    sleep = `Has your sleep been affected during online classes?`,
    sleep_reason = `Please share some thoughts on how has your sleep been affected during online classes if you marked previous question as 'yes':`,
    education_strain = `From 1 being lowest and 5 the highest, how much are you in constant strain around the time of assignment submissions, projects, exams or classes in online setting?`,
    graduation_effect = `Did the online class setting effect your graduation timeline or plans?`,
    graduation_effect_reason = `If your answer is 'yes' to previous question, would you like to take a few seconds and describe to us how has it impacted your graduation timeline or future plans?`,
    feeling_unhappiniess = `From scale of 1 to 5 where 1 being the lowest and 5 being the highest, how much has the feeling of unhappiness or anxiety been felt by you during your online classes in COVID-19 situation?`,
    feeling_unhappiness_reason = `Would you like to share your thoughts on what has been the cause of anxiety or worry during this time for you?`,
    insititute_assistance = `How was the assistance from your Institute during COVID-19 when it comes to getting comfortable in online-class decorum? 1 being 'least assistive' to 5 being 'most assistive'.`,
    internet = `Has the internet connection affected you during online classes or online exams?`,
    internet_reason = `If you answered 'yes' to previous question, can you please share thoughts on what kind of effects have you experienced due to poor internet connection?`,
    job_lost = `Did you lose your job due to Covid-19 pendemic?`,
    full_courses_taken = `Were you able to take all the courses that you wanted?`,
    full_courses_taken_reason = `If your answer is 'no' to previous question, what were your immediate thoughts when you were unable to take all the courses in due time?`,
    stranded_outside = `Were you stranded outside your country of education?`,
    stranded_outside_cope = `If your answer is 'yes' to previous question, how did you cope with online studies at that time?`,
    financial_problem = `Did you have financial problem due to COVID-19?`,
    financial_problem_affect = `If your answer is 'yes' to the question above, how do you think it affected your studies?`,
    results_better = `Did you get better results in online-exam setting where you could take the exam in premise of your own home?`,
    additional_notes = `Please share any thoughts in addition that you feel were not addressed in above questions.`
  ) 

# delete columns 1 & 2
df_data <- df_data[, -c(1:2)]
df_data[1, "motivation"] <- "No"

# columns to be changed from character to factor
cols.char <- c( "study_in_germany","country_study","country" ,"living_setup","concentration","sleep","education_strain","graduation_effect",
                "feeling_unhappiniess","insititute_assistance","internet","job_lost","full_courses_taken","stranded_outside",
                "financial_problem","results_better", "motivation") 


# change character to factors for chosen columns
df_data[cols.char] <- lapply(df_data[cols.char], factor)  
#


##################  ALL ABOVE CODE IS THE SAME AS YOURS #################################
##################  RANDOM FOREST STARTS HERE #################################
##################  JUST ADD THIS PART OF CODE  #################################
##################  THANKS AGAIN  #################################


# Random  Forest Classification from the package is used. 

set.seed(123)

# dividing Testing and Training Data 
init_split <-  initial_split(df_data,prop=3/4)
so_training <- training(init_split)
so_testing <- testing(init_split)

# Setting Cross-Validation Parameter Control
control_1 <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=3)


# Checking Dimensions of Training and Testing sets
dim(so_testing)
dim(so_training)


# building model 1
fit.rf <- train(concentration ~ education_strain + country_study + sleep + graduation_effect +
                  internet + job_lost + stranded_outside + financial_problem
                , data=so_training, method="rf", metric="Accuracy", trControl=control_1, na.action=na.exclude)


# building model 2
fit.rf2 <- train( results_better ~ 
                    education_strain  + sleep + graduation_effect +
                    internet + job_lost + stranded_outside + financial_problem
                  , data=so_training, method="rf", metric="Accuracy", trControl=control_1, na.action=na.exclude)

# building model 3
fit.rf3 <- train(motivation ~ feeling_unhappiniess   + sleep + graduation_effect + stranded_outside + internet + job_lost
                 , data=so_training, method="rf", metric="Accuracy", trControl=control_1, na.action=na.exclude)



# estimate Random Forrest on the testing dataset

# evaluating model 1
fit.rf # model details
predictions_fit.rf <- predict(fit.rf, so_testing)
cm_fit.rf <- confusionMatrix(predictions_fit.rf, so_testing$concentration) #confusion matrix
print(cm_fit.rf)

# evaluating model 2

fit.rf2 # model details
predictions_fit.rf2 <- predict(fit.rf2, so_testing) 
cm_fit.rf2 <- confusionMatrix(predictions_fit.rf2, so_testing$results_better) #confusion matrix
print(cm_fit.rf2)

# evaluating model 3
fit.rf3 # model details
predictions_fit.rf3 <- predict(fit.rf3, so_testing)
cm_fit.rf3 <- confusionMatrix(predictions_fit.rf3, so_testing$motivation) #confusion matrix
print(cm_fit.rf3)


# comparison of models

# making tables for extracted values
tr   aining_accuracies <- list(m1 =max(fit.rf$results$Accuracy) , m2=max(fit.rf2$results$Accuracy), m3= max(fit.rf3$results$Accuracy))
print(training_accuracies)
training_kappa <- list(m1= max(fit.rf$results$Kappa), m2 = max(fit.rf2$results$Kappa), m3 = max(fit.rf3$results$Kappa))
print(training_kappa)
testing_accuracies <- list(m1 = cm_fit.rf$overall[1] , m2 = cm_fit.rf2$overall[1] , m3= cm_fit.rf3$overall[1])
print(testing_accuracies) 

# making data frames to plot graphs
table1 <- do.call(rbind, Map(data.frame, Training_Acc=training_accuracies, Testing_Acc=testing_accuracies))
table2 <- do.call(rbind, Map(data.frame, Training_Acc=training_accuracies, Training_Kappa_values=training_kappa))
print(table1)


# Plotting Training vs Testing Accuracies
ggplot(table1, aes(x=Testing_Acc, y=Training_Acc)) +
  geom_point(shape=16, color="light blue", size = 8) + 
  geom_text(label=rownames(table1)) 

# Plotting Training vs Training Kappa
ggplot(table2, aes(x=Training_Acc, y=Training_Kappa_values)) +
  geom_point(shape=16, color="light green", size = 8) + 
  geom_text(label=rownames(table2)) 



"
 The function train() from Caret Package used for model buidling. Several functions from other R Packages are also used pre-processing, data wrangling and visualziations.
 It includes Tunning algorithm which helps us get better results and give is the freedom to tune more parameters in a better fashion. We tried to balance two main paramters
 i.e. mtry and ntree. mtry: Number of variable is randomly collected to be sampled at each split time and ntree: Number of branches will grow after each time split. 
 
 We trained 3 different model in get the best information out of the data. Following are the data modeling formulae:
 
 Model 1: Was the student able to concerntrate on studies (concentration) as the dependent variable. Independent variables are:
                  How much strain does a student have (education_strain), which country is the student studying in (country_study), 
                  is the student getting enough sleep (sleep), the delay of the students graduation (graduation_effect), internet problems (internet), 
                  did the student lost job due to covid-19 (job_lost), was the student stranded outside germany (stranded_outside) and were there any financial 
                  problems (financial_problem).
                  
                  train(concentration ~ education_strain + country_study + sleep + graduation_effect +
                  internet + job_lost + stranded_outside + financial_problem
                , data=so_training,  method= rf)

 Model 2: Did the result get better (results_better) as the dependent variable. Independent variables are:
                   How much strain does a student have (education_strain), is the student getting enough sleep (sleep), he delay of the students graduation (graduation_effect),
                   did the student lost job due to covid-19 (job_lost), was the student stranded outside germany (stranded_outside), internet problems (internet) and were there any financial 
                   problems (financial_problem).
                   
 
 
                  train( results_better ~ 
                    education_strain  + sleep + graduation_effect +
                    internet + job_lost + stranded_outside + financial_problem
                  , data=so_training, method=rf)
                  
                  
 Model 3: How motivated does the student feel (motivation) as the dependent variable. Independent variables are:
                  How unhappy does the student feel (feeling_unhappiniess), is the student getting enough sleep (sleep),  he delay of the students graduation (graduation_effect),
                  was the student stranded outside germany (stranded_outside), internet problems (internet) and  did the student lost job due to covid-19 (job_lost).
 
 
                  train(motivation ~ feeling_unhappiniess + sleep + graduation_effect + stranded_outside + internet + job_lost
                 , data=so_training, method= rf)
                 
  We compare two major aspects;
  1. Training and Testing accuracies for each model.
  2. Training accuracies with Kappa values for each model. 
  
  
  THE GRAPHS:
  
  The first scatter plot shows the camparison between the three models. Making Model 1 (Concerntration) to be the best modelled Independent variable, having explanation 
  of just over 70% training accuracy and just under 45% of testing accuracy. The remaining two model are having different interpretation as the first one. 
  
  The second scatter plot agaib shows the camparison between the three models, but this time it is Training Accuracies with Kappa Values. Model 1 (Concerntration)
  having describing more relevancy with Kappa value greater than 36%.

" 

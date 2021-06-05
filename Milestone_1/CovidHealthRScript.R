library(tidyverse)
library("readxl")



df_data = read_excel('A Short Survey About Online Studies And Its Effects (Responses).xlsx')


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

df_data <- df_data[, -c(1:2)] # delete columns 1 & 2

  
---
title: "Effect of COVID-19 Induced Lockdown on Mental Wellbeing of Students Attending Online Classes"
output:
  html_document:
    df_print: paged
    toc: true
    number_sections : true
    toc_depth: 2
    toc_float: true 
    theme: readable
    highlight: tango
    self_contained: false
bibliography: citation.bib
csl: ieee.csl
---



```{r setup, include=FALSE}
### Load Libraries
knitr::opts_chunk$set(echo = TRUE)
library(DT)
library(knitr)
library(kableExtra)
library(tidyverse)
library(ggcorrplot)
library(ggpubr)
library(rstatix)
library("readxl")
library(ggpubr)
library(cowplot)
library(grid)
library(remotes)
library(plotly)
library(tidytext)
library(glue) #for pasting strings
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(plotly)
library(forcats)
library(caTools)
library(nnet)
library(caret)
library(Metrics)
library(parsnip)
library(yardstick)
library(rsample)
library(recipes)
library(ranger)
library("reshape2")
library('e1071')
library(randomForest)



```


**By:**

 * Ammar Ateeq
 * Muhammad Hashim Naveed
 * Sidra Aziz
 
```{r dataset, include=FALSE}
options(max.print=1000000)

### Load Datasets

#Datasets
df_data = read_excel('A Short Survey About Online Studies And Its Effects (Responses).xlsx')
df_data_questions = read_excel('A Short Survey About Online Studies And Its Effects (Responses).xlsx')

```


# **Related Works**

COVID-19 has had negative consequences on the planet for more than a year, ranging from economic downturns to high mortality tolls that have brought several nations and areas to their knees. We are currently witnessing its catastrophic impacts in the shape of second and third waves, claiming a large number of human lives every day in places like India.

There have been numerous studies on the effects of COVID-19, its prediction, symptom analysis in various bodies, academic years across institutes, and their preventive measures in the spirit of continuing studies for students in countries such as the United States, the United Kingdom, and European countries that experience a large number of international students every six months.[@10.1371/journal.pone.0245327]

Yusen Zhai, Xue Du, 2020 research [@article] is one of a few publications that has attempted to determine the impact of COVID-19 on the mental health of college students in the United States.

COVID-19 will have ongoing critical repercussions on college students, according to the report, and "it is thus vital for universities to develop an awareness of students' mental health needs and concerns, and to empower their students to seek aid and support throughout this biological crisis." [@10.1371/journal.pone.0236337]

Our objective is to assist in the discovery of the consequences and worries that university students in Germany may have experienced and assist in the development of recommendations that can help shape the future after the debate.

# **Motivation & Overview**

**Objective 1:** The study's primary objective is to find out a relation between the COVID-19 Lockdown effect on the mental health of students who have been taking online semesters, mainly in the Summer and Winter semesters of 2020/2021. When we talk about correlation, we mean to find out whether there was a pattern for students to struggle with online study schedules. This correlation is the potential behind what students have gone through during the year of confinement to dorms or shared home spaces, and whether good or bad, the effect could be found on their mental health as a result.

The aspects that should help us find this correlation should be the factors such as semester of the student, demographics such as whether they were stranded in their home country or staying in the dorm room, the communication process of steps to attending online classes by the institute, their internet connection as well their grade score quality. Furthermore, we would like to accommodate a subset of the GHQ-12 questionnaire where we ask the students questions about their concentration span, the capability of making decisions, feeling of unhappiness or depression, sleep deprivation, constant feeling of stress, and the financial situation of the students during this period, etc.

**Objective 2:** The second objective of the study is to perform and visualize a sentiment analysis using text data gathered using the survey and help find out students' sentiments throughout their online study period that they express. For example, sentiment polarity scores from negative, neutral to positive, happiness, fear or trust, etc.

**Objective 3:** Our third objective aims to predict the grades of the students who are confined in similar demographic situations and share somewhat identical sentiments. This study will not indicate any student’s depression but only trying to understand whether there are adverse effects on students who have gone ahead for a year with little to no social interaction. At the same time had to go through a strenuous schedule of studies and/or part-time jobs.

**Questionnaire:**
[**_A Short Survey About Online Studies And Its Effects_**](https://docs.google.com/forms/d/1iVyj8rxLUdoJLlmdf9S0So3_HSKjNUd31Zn_Pv5r2Pk/edit)



# **Data**

## **Data Cleaning & Manipulation**

Data cleaning wasn't as big an issue for such a dataset. Given the fact that the answers were fixed options and required, there were no NAs. Except in the questions which required reasoning. It made no sense to remove those rows as we would be losing out on a lot of information if we pulled all NAs for those types of questions.

Furthermore, all the categorical answers were converted to factor variable with consistent levels (No-Maybe-Yes).

```{r data_manipulation, include=FALSE}

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

# columns to be changed from character to factor
cols.char <- c("living_setup","education_strain",
              "feeling_unhappiniess","insititute_assistance") 

# change character to factors for chosen columns
df_data[cols.char] <- lapply(df_data[cols.char], factor)


# change the rest of the variables in a consistent level 'No->Maybe->Yes'
df_data$motivation <- factor(df_data$motivation, level = c('No', 'Maybe', 'Yes'))
df_data$concentration <- factor(df_data$concentration, level = c('Bad', 'Same', 'Good'))
df_data$sleep <- factor(df_data$sleep, level = c('No', 'Yes'))
df_data$graduation_effect <- factor(df_data$graduation_effect, level = c('No', 'Yes'))
df_data$internet <- factor(df_data$internet, level = c('No', 'Maybe', 'Yes'))
df_data$job_lost <- factor(df_data$job_lost, level = c('No', 'Yes'))
df_data$full_courses_taken <- factor(df_data$full_courses_taken, level = c('No', 'Yes'))
df_data$stranded_outside <- factor(df_data$stranded_outside, level = c('No', 'Yes'))
df_data$financial_problem <- factor(df_data$financial_problem, level = c('No', 'Maybe', 'Yes'))
df_data$results_better <- factor(df_data$results_better, level = c('No', 'Maybe', 'Yes'))

```

```{r encode_variables,warning=FALSE, message=FALSE, echo=FALSE}

# df_data$living_setup_temp = factor(df_data$living_setup,
# levels = c('In dorm room','WG / Shared apartment','With Family'),
# labels = c(1,2,0))
# 
# df_data$motivation_temp = factor(df_data$motivation,
# levels = c('Yes','Maybe','No'),
# labels = c(0,2,1))
# 
# df_data$concentration_temp = factor(df_data$concentration,
# levels = c('Good','Same','Bad'),
# labels = c(0,2,1))
# 
# df_data$sleep_temp = factor(df_data$sleep,
# levels = c('No','Yes'),
# labels = c(0,1))
# 
# df_data$education_strain_temp = factor(df_data$education_strain,
# levels = c(5,4,3,2,1),
# labels = c(4,3,2,1,0))
# 
# df_data$graduation_effect_temp = factor(df_data$graduation_effect,
# levels = c('No','Yes'),
# labels = c(0,1))
# 
# df_data$feeling_unhappiniess_temp = factor(df_data$feeling_unhappiniess,
# levels = c(5,4,3,2,1),
# labels = c(0,1,2,3,4))
# 
# df_data$insititute_assistance_temp = factor(df_data$insititute_assistance,
# levels = c(1,2,3,4,5),
# labels = c(4,3,2,1,0))
# 
# df_data$internet_temp = factor(df_data$internet,
# levels = c('No','Maybe','Yes'),
# labels = c(0,2,1))
# 
# df_data$job_lost_temp = factor(df_data$job_lost,
# levels = c('No','Yes'),
# labels = c(0,1))
# 
# df_data$full_courses_taken_temp = factor(df_data$full_courses_taken,
# levels = c('Yes','No'),
# labels = c(0,1))
# 
# df_data$stranded_outside_temp = factor(df_data$stranded_outside,
# levels = c('No','Yes'),
# labels = c(0,1))
# 
# df_data$financial_problem_temp = factor(df_data$financial_problem,
# levels = c('Yes','Maybe', 'No'),
# labels = c(0,2,1))
# 
# df_data$results_better_temp = factor(df_data$results_better,
# levels = c('Yes','Maybe', 'No'),
# labels = c(0,2,1))


# For inline R code in next section
germany_students = df_data %>% 
  group_by(study_in_germany) %>%
  summarise(total = n()) %>% 
  filter(study_in_germany == "Yes")

```


## **Overview**


This section will give a breif overview of the demographics of the students who filled out the questionnaire. A total of `r nrow(df_data)` students filled out the form. The graphs will illustrate what nationalities the students are of and the age range. The focus will shift towards the explanatory analysis of the data.

A total of 29 questions were asked in the questionnaire. The format of the options for the questions were ordinal or categorical.

The pie chart below shows that `r germany_students$total` out of the `r nrow(df_data)` students are studying in Germany. Exactly half of the students are in the age range of 26 to 30. Almost 40% are 21 to 25 years of age. The rest are either below 20 or above 30.

```{r data_overview,warning=FALSE, message=FALSE, echo=FALSE }

# Count students who study in Germany and outside Germany
study_country_count = df_data %>% 
  group_by(study_in_germany) %>%
  mutate(
   study_in_germany =  factor(study_in_germany,
levels = c('Yes','No'),
labels = c('Studying in Germany','Studying outside Germany'))
  ) %>% 
  count(sort = TRUE)

# Pie Chart
plot_study_country <- plot_ly(study_country_count, labels = ~study_in_germany, values = ~n, type = 'pie')
plot_study_country <- plot_study_country %>% layout(title = 'Pie Chart of Students Studying in Germany',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

plot_study_country


# Count age rangeof students
df_age_count = df_data %>% 
  group_by(age) %>% 
  count(sort = TRUE)

# Donut chart for age range
fig <- df_age_count %>% plot_ly(labels = ~age, values = ~n)
fig <- fig %>% add_pie(hole = 0.6)
fig <- fig %>% layout(title = "Age Range Distribution",  showlegend = T,
                      legend =list(title=list(text='<b> Age Range </b>')),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
```

## **World Map of Students Nationalities**

Most of the students are from the subcontinent region mainly Pakistan & India.
```{r worldmap,warning=FALSE, message=FALSE, echo=FALSE, fig.width= 12}

# Count students nationalities
df_country_count = df_data %>% 
  group_by(country) %>% 
  count(sort = TRUE)

df_country_count_table = df_country_count%>%
  rename(
    Country = country,
    Count = n
  ) %>% 
kbl() %>%
  kable_material_dark("hover", full_width = FALSE)

# Create world map with counts
df_country_count$region <- df_country_count$country
df_country_count$count_group <- cut(df_country_count$n, 
                      breaks = c(-Inf, 5, 10, 15,20,25, 30, Inf), 
                      labels = c("Less than 5", "5-10", "10-15", "15-20","20-25","25-30", "More than 30"))

world_map <- map_data(map = "world")

 ggplot(df_country_count) +
  geom_map(aes(map_id = country, fill = fct_rev(count_group)), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(name = "Counts", values = rev(brewer.pal(5, name = 'Dark2'))) +
  theme_void() +
  coord_fixed()

```

<!-- ```{r worldmap_plot, echo=FALSE, fig.width= 12, include=FALSE} -->

<!-- renderPlot(world_plot) -->

<!-- ``` -->

# **Exploratory Analysis**

This section will look into the basic distribution of data amongst the different variables. This will show a glimpse of the answers of the students in terms of the selected variables.

```{r frequency_plot,warning=FALSE, message=FALSE, echo=FALSE}

# Function for bar plot for one variable
plot_freq <- function(col1,x_title,y_title,plot_title, plot_caption) {
 df_data %>%
  ggplot(aes(x = .data[[col1]])) +
  geom_bar(fill = "steelblue") + 
  labs(
    x = x_title,
    y = y_title,
    title = plot_title,
    caption = plot_caption
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
}

plot_freq("motivation","Student Motivated For Online Classes","Number of Students","Motivation For Online Classes", "Figure 1" )

plot_freq("sleep","Sleep Disturbed During Online Classes","Number of Students","Sleep Disturbance During Online Classes", "Figure 2" )

plot_freq("education_strain","Education Strain During Online Classes","Number of Students","Strain During Online Classes (1: Low, 5: High)", "Figure 3" )

plot_freq("graduation_effect","Graduation Delayed","Number of Students","Plot for Number of Students with Studies Delayed", "Figure 4" )

plot_freq("feeling_unhappiniess","Feeling of Unhappiness","Number of Students","Feeling of Unhappiness During Online Classes (1: Low, 5: High)", "Figure 5" )

plot_freq("insititute_assistance","University Assistance","Number of Students","University Assistance To Students (1: Low, 5: High)", "Figure 6" )

plot_freq("internet","Internet Problem","Number of Students","Internet Caused Problem During Online Classes", "Figure 7" )

plot_freq("full_courses_taken","Full Courses Taken","Number of Students","Students Able To Take Full Courses As Wanted", "Figure 8" )

plot_freq("stranded_outside","Sleep Disturbance During Online Classes","Number of Students","Number of Students Stranded Outside the Country of Study", "Figure 9" )

plot_freq("financial_problem","Financial Problem","Number of Students","Financial Strain Amongst Students", "Figure 10" )

plot_freq("living_setup","Type of Living","Number of Students","Living Setup of the Students", "Figure 11" )

```


## **Correlations Between Variables**

This section will correlate two variables. This will give a better insight how some factors have an effect on the other. Analysis show that when the concentration and the motivation of the student have been bad, the grades of the student have been bad as well. Which is an obvious statement, but it is still important to know this fact as it also gives an indication of the validity of the answers of the students.

Analysis show that students who are living in shared apartments have been the most unhappy during online studies. We initially predicted the single apartment students to be unhappy because of lonliness. This does not mean it is soley because of this reason, but it is a major factor.

Losing job also leads to unhappiness which can be seen in figure 15.

```{r correlation_plots,warning=FALSE, message=FALSE, echo=FALSE, fig.width=10, fig.height=6}

# Function for bar plot for two variables

plot_freq_correlation <- function(col1,col2) {
 df_data %>%
  group_by(.data[[col1]],.data[[col2]]) %>%
  summarise(total = n()) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(x = .data[[col1]], y = percent))+
  geom_bar(
    aes(fill = .data[[col1]]), stat = "identity", color = "white",
    position = position_dodge(0.9)
    )+
    theme_bw()+
    scale_y_continuous(labels=scales::percent) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.title.x=element_blank())
}


# New facet label names for result variable
result.labs1 <- c("Result Worse", "Not Sure or Same", "Result Better")
names(result.labs1) <- c("No","Maybe", "Yes")

# New facet label names for living setup variable
living.labs1 <- c("Single Apartment", "Shared Apartment", "With Family")
names(living.labs1) <- c("In dorm room","WG / Shared apartment","With Family")

# New facet label names for stranded outside variable
stranded.labs1 <- c("Wasn't Stranded", "Stranded Outisde the Country")
names(stranded.labs1) <- c("No","Yes")

# New facet label names for graduation & study outside variable
grad.labs1 <- c("Graduation Not Delayed", "Graduation Delayed")
names(grad.labs1) <- c("No","Yes")

# New facet label names for job variable
job.labs1 <- c("Didn't Lose Job", "Job Lost")
names(job.labs1) <- c("No","Yes")

# New facet label names for motivation variable
motv.labs1 <- c("Not Motivated", "No Change", "Motivated")
names(motv.labs1) <- c("No", "Maybe", "Yes")

# New facet label names for internet variable
internet.labs1 <- c("No Internet Problems", "Not Sure or Same", "Internet Problems")
names(internet.labs1) <- c("No","Maybe", "Yes")

# New facet label names for concentration variable
conc.labs1 <- c("Bad Concentration", "Same Concentration", "Good Concentration")
names(conc.labs1) <- c("Bad", "Same", "Good")

# New facet label names for result variable
finance.labs1 <- c("No Financial Problems", "Not Sure or Same", "Financial Problems")
names(finance.labs1) <- c("No","Maybe", "Yes")


# Plot1
plot_freq_correlation("results_better","concentration")+
  labs(
    x = "Results Better",
    y = "Number of Students",
    title = "Relationship Between Concentration & Results",
    caption = "Figure 12"
  )+
  scale_x_discrete(breaks=c("No","Maybe","Yes"),
                     labels=c("Worse Result","Same or Uncertain", "Better Result"))+
  facet_wrap(~concentration,
             labeller = labeller(concentration = conc.labs1)) + 
  fill_palette("jco")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
      legend.position = "none")


# Plot2
plot_freq_correlation("results_better","motivation")+
  labs(
    x = "Results Better",
    y = "Number of Students",
    title = "Relationship Between Motivation & Results",
    caption = "Figure 13"
  )+
  scale_x_discrete(breaks=c("No","Maybe","Yes"),
                     labels=c("Worse Result","Same or Uncertain", "Better Result"))+
  facet_wrap(~motivation,
             labeller = labeller(motivation = motv.labs1)) + 
  fill_palette("jco")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
      legend.position = "none")


# Plot3
plot_freq_correlation("feeling_unhappiniess","living_setup")+
  labs(
    x = "Level of Unhappiness (1:Low, 5:High)",
    y = "Number of Students",
    title = "Relationship Between Unhappiness Level & Living Setup",
    caption = "Figure 14"
  )+
  facet_wrap(~living_setup,
             labeller = labeller(living_setup = living.labs1)) + 
  fill_palette("jco")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
      legend.position = "none")


#Plot4
plot_freq_correlation("feeling_unhappiniess","job_lost")+
  labs(
    x = "Level of Unhappiness (1:Low, 5:High)",
    y = "Number of Students",
    title = "Relationship Between Job Lost & Unhappiness Level",
    caption = "Figure 15"
  )+
  facet_wrap(~job_lost,
             labeller = labeller(job_lost = job.labs1)) + 
  fill_palette("jco")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
      legend.position = "none")


#Plot5
plot_freq_correlation("feeling_unhappiniess","stranded_outside")+
  labs(
    x = "Level of Unhappiness (1:Low, 5:High)",
    y = "Number of Students",
    title = "Relationship Between Standed Outside Country of Study & Unhappiness Level",
    caption = "Figure 16"
  )+
  facet_wrap(~stranded_outside,
             labeller = labeller(stranded_outside = stranded.labs1)) + 
  fill_palette("jco")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
      legend.position = "none")



germany_students_result_no = df_data %>% 
  group_by(study_in_germany, results_better) %>%
  summarise(total = n()) %>% 
  filter(study_in_germany == "Yes" & results_better == "No")

germany_students_result_maybe = df_data %>% 
  group_by(study_in_germany, results_better) %>%
  summarise(total = n()) %>% 
  filter(study_in_germany == "Yes" & results_better == "Maybe")


germany_students_result_lost_job = df_data %>% 
  group_by(study_in_germany, job_lost) %>%
  summarise(total = n()) %>% 
  filter(study_in_germany == "Yes" & job_lost == "Yes")

germany_unhappiness = df_data %>% 
  group_by(study_in_germany, feeling_unhappiniess) %>%
  summarise(total = n()) %>%
  mutate(percent = total/sum(total)*100) %>% 
  filter(study_in_germany == "Yes" & (feeling_unhappiniess == 4 | feeling_unhappiniess == 5)) %>%
  select(percent) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(
    percent = round(percent, digits = 2)
  )

germany_outside_unhappiness = df_data %>% 
  group_by(study_in_germany, feeling_unhappiniess) %>%
  summarise(total = n()) %>%
  mutate(percent = total/sum(total)*100) %>% 
  filter(study_in_germany == "No" & (feeling_unhappiniess == 4 | feeling_unhappiniess == 5)) %>%
  select(percent) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(
    percent = round(percent, digits = 2)
  )


germany_students_graduation_effect = df_data %>% 
  group_by(study_in_germany, graduation_effect) %>%
  summarise(total = n()) %>%
  mutate(percent = total/sum(total)*100) %>% 
  filter(study_in_germany == "Yes" & graduation_effect == "Yes") %>%
  select(percent) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(
    percent = round(percent, digits = 2)
  )

germany_outside_students_graduation_effect = df_data %>% 
  group_by(study_in_germany, graduation_effect) %>%
  summarise(total = n()) %>%
  mutate(percent = total/sum(total)*100) %>% 
  filter(study_in_germany == "No" & graduation_effect == "Yes") %>%
  select(percent) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(
    percent = round(percent, digits = 2)
  )

```



## **Germany Specific**


It seems rather logical to look a little deeper into just the students studying in Germany. Upon analysis, it was found that none of the students studying in Germany are living with their families.

Out of `r germany_students$total` students who study in Germany, `r germany_students_result_no$total` have said their results have gotten worse and `r germany_students_result_maybe$total` say 'maybe'. That is very high percentage of people compared to the people not living in Germany.

None of the students outside Germany lost their jobs during the pandemic, whereas out of `r germany_students$total` people, `r germany_students_result_lost_job$total` people lost their jobs.

`r germany_unhappiness$percent`% of students studying in Germany show high levels of unhappiness or anxiety. Compared to `r germany_outside_unhappiness$percent`% of students studying outside.


Almost `r germany_students_graduation_effect$percent`% of students studying in Germany faced a delay in their study schedule compared to only `r germany_outside_students_graduation_effect$percent`% for students studying outside.

The students outside of Germany faced internet problems much more than the students in Germany. The reason for it can be down to the fact that most of the students are from the subcontinent region where it is not so uncommon to have electricity or internet issues.

Students in Germany faced financial problems much more than the students outside Germany. It would be interesting to see for future analysis the semester number of the students. New international students must submit a minimum amount in their banks, which is sufficient for one year. Therefore, it is just a prediction that most of these students facing financial problems are semester two onwards or have been in Germany for more than a year.

Concentration levels have also been harmful during the online studies for German students.

While this analysis does not seal that students in Germany have faced many anxiety-stricken online studies, it indeed points in that direction. It would be more conclusive with an equal number of students from within and outside Germany. Only then can it be said with much more conviction.




```{r germany_specific,warning=FALSE, message=FALSE, echo=FALSE, fig.width=10, fig.height=6}

# function for plotting facet bar graph with percentage

plot_facet <- function(col1, col2,facet_lbl,x_title,y_title,plot_title) {
 df_data %>% 
  group_by(.data[[col1]], .data[[col2]]) %>%
  summarise(total = n()) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(x = .data[[col1]], y = percent))+
  geom_bar(
    aes(fill = .data[[col1]]), stat = "identity", color = "white",
    position = position_dodge(0.9)
    )+
    theme_bw()+
    scale_y_continuous(labels=scales::percent) +
    scale_x_discrete(breaks=c("No","Yes"),
                     labels=c("Outside Germany", "In Germany"))+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.title.x=element_blank())
}


#Plot1
plot_facet("study_in_germany", "graduation_effect") +
  facet_wrap(~graduation_effect,
             labeller = labeller(graduation_effect = grad.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Studying in Germany",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Graduation Delay",
    caption = "Figure 17"
  )


#Plot2
plot_facet("study_in_germany", "results_better")+
  facet_wrap(~results_better,
             labeller = labeller(results_better = result.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Studying in Germany ",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Result",
    caption = "Figure 18"
  )
   
# In terms of job lost

#Plot3
plot_facet("study_in_germany", "job_lost")+
  facet_wrap(~job_lost,
             labeller = labeller(job_lost = job.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Job Lost",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Losing Job",
    caption = "Figure 19"
  )


# In terms of motivation
#Plot4
plot_facet("study_in_germany", "motivation")+
  facet_wrap(~motivation,
             labeller = labeller(motivation = motv.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Studying in Germany ",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Motivation",
    caption = "Figure 20"
  )


# In terms of internet problems
#Plot5
plot_facet("study_in_germany", "internet")+
  facet_wrap(~internet,
             labeller = labeller(internet = internet.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Studying in Germany ",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Internet Problems",
    caption = "Figure 21"
  )


# In terms of financial problem
#Plot6
plot_facet("study_in_germany", "financial_problem")+
  facet_wrap(~financial_problem,
             labeller = labeller(financial_problem = finance.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Studying in Germany ",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Financial Problems",
    caption = "Figure 22"
  )


# In terms of concentration
#Plot7
plot_facet("study_in_germany", "concentration")+
  facet_wrap(~concentration,
             labeller = labeller(concentration = conc.labs1)) + 
    fill_palette("jco") +
  labs(
    x = "Studying in Germany ",
    y = "Number of Students (In percentage)",
    title = "Relationship Between Students Studying in Germany & Outside in terms of Concentration",
    caption = "Figure 23"
  )

```



# **Data Modeling**



  

## **Random Forrest**

The function train() from Caret Package used for model buidling. Several functions from other R Packages are also used pre-processing, data wrangling and visualziations.
 It includes Tunning algorithm which helps us get better results and give is the freedom to tune more parameters in a better fashion. We tried to balance two main paramters
 i.e. mtry and ntree. mtry: Number of variable is randomly collected to be sampled at each split time and ntree: Number of branches will grow after each time split. 
 
 We trained 3 different model in get the best information out of the data. Following are the data modeling formulae:
 
**Model 1:** Was the student able to concerntrate on studies (concentration) as the dependent variable. Independent variables are:
                  How much strain does a student have (education_strain), which country is the student studying in (country_study), 
                  is the student getting enough sleep (sleep), the delay of the students graduation (graduation_effect), internet problems (internet), 
                  did the student lost job due to covid-19 (job_lost), was the student stranded outside germany (stranded_outside) and were there any financial 
                  problems (financial_problem).
                  
                  train(concentration ~ education_strain + country_study + sleep + graduation_effect +
                  internet + job_lost + stranded_outside + financial_problem
                , data=so_training,  method= rf)

**Model 2:** Did the result get better (results_better) as the dependent variable. Independent variables are:
                   How much strain does a student have (education_strain), is the student getting enough sleep (sleep), he delay of the students graduation (graduation_effect),
                   did the student lost job due to covid-19 (job_lost), was the student stranded outside germany (stranded_outside), internet problems (internet) and were there any financial 
                   problems (financial_problem).
                   
 
 
                  train( results_better ~ 
                    education_strain  + sleep + graduation_effect +
                    internet + job_lost + stranded_outside + financial_problem
                  , data=so_training, method=rf)
                  
                  
**Model 3:** How motivated does the student feel (motivation) as the dependent variable. Independent variables are:
                  How unhappy does the student feel (feeling_unhappiniess), is the student getting enough sleep (sleep),  he delay of the students graduation (graduation_effect),
                  was the student stranded outside germany (stranded_outside), internet problems (internet) and  did the student lost job due to covid-19 (job_lost).
 
 
                  train(motivation ~ feeling_unhappiniess + sleep + graduation_effect + stranded_outside + internet + job_lost
                 , data=so_training, method= rf)
                 
  We compare two major aspects;
  1. Training and Testing accuracies for each model.
  2. Training accuracies with Kappa values for each model. 
  
  
  **THE GRAPHS:**
  
  The first scatter plot shows the camparison between the three models. Making Model 1 (Concerntration) to be the best modelled Independent variable, having explanation 
  of just over 70% training accuracy and just under 45% of testing accuracy. The remaining two model are having different interpretation as the first one. 
  
  The second scatter plot agaib shows the camparison between the three models, but this time it is Training Accuracies with Kappa Values. Model 1 (Concerntration)
  having describing more relevancy with Kappa value greater than 36%.
  
  
```{r random_forrest_model,warning=FALSE, message=FALSE, echo=FALSE}

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
#dim(so_testing)
#dim(so_training)


# building model 1
fit.rf <- train(concentration ~ education_strain + country_study + sleep + graduation_effect +
                  internet + job_lost + stranded_outside + financial_problem
                , data=so_training, method="rf", metric="Accuracy", trControl=control_1, na.action=na.exclude)


# building model 2
fit.rf2 <- train( results_better ~ study_in_germany + graduation_effect  + financial_problem
                  , data=so_training, method="rf", metric="Accuracy", trControl=control_1, na.action=na.exclude)

# building model 3
fit.rf3 <- train(motivation ~ feeling_unhappiniess   + sleep + graduation_effect + stranded_outside + internet + job_lost
                 , data=so_training, method="rf", metric="Accuracy", trControl=control_1, na.action=na.exclude)



# estimate Random Forrest on the testing dataset

# evaluating model 1
#fit.rf # model details
predictions_fit.rf <- predict(fit.rf, so_testing)
cm_fit.rf <- confusionMatrix(predictions_fit.rf, so_testing$concentration) #confusion matrix
#print(cm_fit.rf)

# evaluating model 2

#fit.rf2 # model details
predictions_fit.rf2 <- predict(fit.rf2, so_testing) 
cm_fit.rf2 <- confusionMatrix(predictions_fit.rf2, so_testing$results_better) #confusion matrix
#print(cm_fit.rf2)

# evaluating model 3
#fit.rf3 # model details
predictions_fit.rf3 <- predict(fit.rf3, so_testing)
cm_fit.rf3 <- confusionMatrix(predictions_fit.rf3, so_testing$motivation) #confusion matrix
#print(cm_fit.rf3)


# comparison of models

# making tables for extracted values
training_accuracies <- list(m1 =max(fit.rf$results$Accuracy) , m2=max(fit.rf2$results$Accuracy), m3= max(fit.rf3$results$Accuracy))
#print(training_accuracies)
training_kappa <- list(m1= max(fit.rf$results$Kappa), m2 = max(fit.rf2$results$Kappa), m3 = max(fit.rf3$results$Kappa))
#print(training_kappa)
testing_accuracies <- list(m1 = cm_fit.rf$overall[1] , m2 = cm_fit.rf2$overall[1] , m3= cm_fit.rf3$overall[1])
#print(testing_accuracies) 

# making data frames to plot graphs
table1 <- do.call(rbind, Map(data.frame, Training_Acc=training_accuracies, Testing_Acc=testing_accuracies))
table2 <- do.call(rbind, Map(data.frame, Training_Acc=training_accuracies, Training_Kappa_values=training_kappa))
#print(table1)


# Plotting Training vs Testing Accuracies
ggplot(table1, aes(x=Testing_Acc, y=Training_Acc)) +
  geom_point(shape=16, color="light blue", size = 8) + 
  geom_text(label=rownames(table1)) +
  theme_bw() +
  labs(
    x = "Testing Accuracies",
    y = "Testubg Kappa Values",
    caption = "Figure 24"
  )


# Plotting Training vs Training Kappa
ggplot(table2, aes(x=Training_Acc, y=Training_Kappa_values)) +
  geom_point(shape=16, color="light green", size = 8) + 
  geom_text(label=rownames(table2)) +
  theme_bw()+
  labs(
    x = "Training Accuracies",
    y = "Training Kappa Values",
    caption = "Figure 25"
  )

```


## **Multinomial Logistic Regression**

```{r logistic_model,warning=FALSE, message=FALSE, echo=FALSE, include=FALSE}

set.seed(123)
dfModel = df_data[sample(nrow(df_data)),]
split = floor(0.75 * nrow(dfModel))
dfModelTrain = dfModel[0:split,]
dfModelTest = dfModel[(split+1):nrow(dfModel),]

# Fit the model

model <- multinom( results_better ~graduation_effect + education_strain + feeling_unhappiniess + study_in_germany + financial_problem , data = dfModelTest)

# Summarize the model
summary(model)

# Find most important variables for model
topModel = varImp(model)
topModel$Variables = row.names(topModel)
topModel = topModel[order(-topModel$Overall),]
head(topModel, 2)

preds1 = predict(model, type = "probs", newdata = dfModelTest)
preds2 = predict(model, type = "class", newdata = dfModelTest)

head(preds1,5)
head(preds2,5)

# Accuracy & Kappa Value
postResample(dfModelTest$results_better, preds2)

# Cross validation
totalAccuracy <- c()
cv <- 10
cvDivider <- floor(nrow(dfModel) / (cv+1))
 
#for (cv in seq(1:cv)) {
#   assign chunk to data test
#  dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
#  dataTest <- dfModel[dataTestIndex,]
  # everything else to train
#  dataTrain <- dfModel[-dataTestIndex,]
 
#  cylModel <- multinom(results_better~ study_in_germany + graduation_effect + motivation, data=dataTrain, maxit=1000, trace=T) 
 
#  pred <- predict(model, newdata=dataTest, type="class")
 
  #  classification error
#  cv_ac <- postResample(dataTest$results_better, pred)[[1]]
#  print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
#  totalAccuracy <- c(totalAccuracy, cv_ac)
#}
# mean(totalAccuracy)  

cm = table(predict(model), dfModelTest$results_better)

```

Since the target variable was categorical with 3 levels ('Yes' 'No' 'Maybe'). Even though the choice for Logistic regression model was obvious, it was tricky to do it for 3 levels. This is where multinomial logistic regression comes into play. It considers more than 2 levels for the outcome variable as opposed to the standard glm funcion which is used for binomial variable type.

We looked into the method of encoding our variable into '0' '1' '2' , but given the fact that 2 of the variables specifically 'education_strain' & 'feeling_unhappiness' had 5 levels. The encoding didn't make sense with unequal levels. So we proceeded with the factor variables.

The multinom function from the nnet package will be used to estimate a multinomial logistic regression model. Functions from other R packages can also be used to perform multinomial regression. We chose the multinom function since it does not require data reshaping (as the mlogit package does) and it closely mirrors Hilbe's Logistic Regression Models sample code. Multinomial logistic regression is a straightforward extension of binary logistic regression that allows for the inclusion of more than two categories of the dependent or outcome variable. Our model is run using **Multinom**.

*model <- multinom( results_better ~ study_in_germany + graduation_effect + job_lost , data = dfModelTest)*

The predictors for the model will be students studying in and out of Germany (study_in_germany), the delay of the students graduation (graduation_effect) & if the students lost their jobs or not (job_lost). The response variable will be (results_better) which signifies if the students results got better or worse during online studies.

To create a training and testing data set, first shuffle the data and divide it into two equal data frames. We use **caret's** *varImp* to check the most influential variables once the model has converged. Then, on the testing data set, we use the predict function to predict results. There are two methods for calculating predictions: 'class' and 'probs.'

We run the summary of the model and look at it's interpration:

Keeping All other variables constant, the increase in one unit, meaning from Maybe to Yes will have a decrease of 13.02 units relative to result being bad. Similarily keeping all other variables constant, with relative to result being bad. The increase in one unit of Yes has a decrease in 17.9 units.

Keeping all variables in the model constant, relative to grade being bad. A unit increase from Maybe to Yes has a decrease in 34.1 units. Wherase one unit increase from Yes to No has an increase in 30 units.

Perhaps the most significant variable is study in germany factor. If we keep all the variables constant in the model, relative to result being bad, one unit from Yes to No has a decrease of 17.23 units. Whereas an increase in one unit from Maybe to Yes has an increase of 7.5 units.


**Accuracy, Kappa & Missclassification**

We use the postResample function from caret to assess the model's accuracy It employs the mean squared error and R-squared for numeric vectors and the overall agreement rate and Kappa for factors.
round(x, digits = 0)


**Accuracy & Kappa:** `r round(postResample(dfModelTest$results_better, preds2), digits = 2)`


**The misclassification rate is:** `r round(1-sum(diag(cm)/sum(cm)), digits = 2)`


```{r confusion_matrix ,warning=FALSE, message=FALSE, echo=FALSE, fig.width=4}
# confustion matrix and misclassification rate

cm %>%
  kbl(caption = "Confusion Matrix for Predicted & Actual Classification") %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  add_header_above(c(" ", "Actual" = 3))


#round(postResample(dfModelTest$results_better, preds2), digits = 2)

# Misclassification rate
#round(1-sum(diag(cm)/sum(cm)), digits = 2)

# correct prediction per group
n = table(dfModelTest$results_better)

#n/sum(n)
cm_2 = cm/colSums(cm)*100

cm_2 %>%
  kbl(caption = "Confusion Matrix for Predicted & Actual Classification (In percentage)") %>%
  kable_classic(full_width = T, html_font = "Cambria")
```



**Multinomial Logistic Model Plot**

The graph shows the prediction probabily that the person would get bad result with the actual classification specified by the 3 different colors. It shows that the probability gets better when there are actual bad results, the model predicts those instances with a much higher accuracy. The biggest constraint is the lack of data, with higher sample size, the prediction will be a lot better. With higher sample size, cross validation can be used which would make the model more accurate.


```{r logistic_model_plot,warning=FALSE, message=FALSE, echo=FALSE}
# Multinomial logistic model plot
predicted_data = data.frame(probabiliy_of_grade_better = model$fitted.values, results_better = dfModelTest$results_better,
                            study_germany = dfModelTest$study_in_germany)

predicted_data = predicted_data[order(predicted_data$probabiliy_of_grade_better.No, decreasing = FALSE),]

predicted_data$rank = 1:nrow(predicted_data)

ggplot(data = predicted_data, aes(x=rank, y=probabiliy_of_grade_better.No)) +
  geom_point(aes(color = results_better), alpha=1, shape=4, stroke=2) +
  labs(
    x = "Index",
    y = "Predicted Probability of getting bad results",
    caption = "Figure 26"
  )+
  theme_bw()+
  scale_y_continuous(labels=scales::percent) +
  theme(plot.title = element_text(hjust = 0.5))

```


# **Sentiment Analysis**

We have aimed at using text mining techniques using libraries and packages offered by R. From tm to tidyverse, tidytext, glue, wordcloud, dplyr we exploit all of this to understand sentiments behind textual data. 
Furthermore, study also aims to perform and visualise a sentiment analysis using text data that is gathered using the survey and help find out sentiments of students throughout their online study period that they express. For example sentiment polarity scores from negative, neutral to positive and feeling of unhappiness, or fear or trust etc.

<!-- ## **Word Cloud** -->

<!-- ```{r wordcloud,warning=FALSE, message=FALSE, echo=FALSE, include=FALSE} -->

<!-- word_count_func <- function(column_name) { -->

<!--   corpus = Corpus(VectorSource(df_data[column_name])) -->
<!--   corpus = tm_map(corpus, content_transformer(tolower)) -->
<!--   corpus = tm_map(corpus, removeNumbers) -->
<!--   corpus = tm_map(corpus, removeWords, stopwords("english")) -->
<!--   corpus = tm_map(corpus, removePunctuation) -->
<!--   corpus = tm_map(corpus, stripWhitespace) -->

<!--   tdm = TermDocumentMatrix(corpus) -->
<!--   m = as.matrix(tdm) -->
<!--   v = sort(rowSums(m), decreasing = TRUE) -->
<!--   d = data.frame(word = names(v),freq = v) -->

<!--   wordcloud(d$word, d$freq, random.order = FALSE, -->
<!--             rot.per = 0.3, scale = c(4,.5), -->
<!--             colors = brewer.pal(8,"Dark2"),main=column_name, max.words = 200, min.freq = 2) -->

<!--   findFreqTerms(tdm, lowfreq = 2) -->

<!--   barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, -->
<!--           col ="pink", main ="Most frequent words", -->
<!--           ylab = "Word frequencies") -->

<!--   findAssocs(tdm, terms = c("online","classes","different"), corlimit = 0.10)			 -->
<!--   # Find associations for words that occur at least 3 times -->
<!--   #findAssocs(tdm, terms = findFreqTerms(tdm, lowfreq = 3), corlimit = 0.25) -->
<!-- } -->

<!-- fluidRow( -->
<!--    column(6, -->
<!-- selectInput( -->
<!--   inputId = 'PlotColumn', -->
<!--   label = 'Select a variable from the dataset', -->
<!--   choices = c('motivation_reason','concentration_reason', 'sleep_reason','graduation_effect_reason','feeling_unhappiness_reason','internet_reason','full_courses_taken_reason', 'stranded_outside_cope','financial_problem_affect','additional_notes') -->
<!-- ) -->
<!-- ) -->
<!-- ) -->
<!-- ``` -->

<!-- ```{r wordcloud_plot,warning=FALSE, message=FALSE, echo=FALSE, include=FALSE} -->
<!-- renderPlot(word_count_func(input$PlotColumn)) -->
<!-- ``` -->


```{r sentiment_analysis,warning=FALSE, message=FALSE, echo=FALSE }
#sentiment function for textual data columns with tokenization using Bing
# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
#sentiment function for textual data columns with tokenization using Bing
getSentiment_Func <- function(column_name) {
  tidy_column <- df_data %>% 
    unnest_tokens(word, column_name)
  head(tidy_column)
  tidy_column %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(columnName = column_name,
           sentiment = positive - negative) # # of positive words - # of negative words
}

Motivation_sentiment <- getSentiment_Func("motivation_reason")
Concentration_sentiment <- getSentiment_Func("concentration_reason")
Sleep_sentiment <- getSentiment_Func("sleep_reason")
Graduation_sentiment <- getSentiment_Func("graduation_effect_reason")
Unhappiness_sentiment <- getSentiment_Func("feeling_unhappiness_reason")
Internet_sentiment <- getSentiment_Func("internet_reason")
FullCoursesTaken_sentiment <- getSentiment_Func("full_courses_taken_reason")
Stranded_sentiment <- getSentiment_Func("stranded_outside_cope")
Financialprob_sentiment <- getSentiment_Func("financial_problem_affect")
AdditionalNotes_sentiment <- getSentiment_Func("additional_notes")

df_sentiment = Reduce(function(x, y) merge(x, y, all=TRUE), list(
  Motivation_sentiment, Concentration_sentiment, Sleep_sentiment, Graduation_sentiment,
  Unhappiness_sentiment,FullCoursesTaken_sentiment,Stranded_sentiment,Financialprob_sentiment,
  AdditionalNotes_sentiment))


#Most common positive and negative words and 
#positive vs. negative sentiments word cloud together
getPosNegcount_Func <- function(column_name, plot_title) {
  tidy_column <- df_data %>%
    unnest_tokens(word, column_name)
  
  bing_word_counts <- tidy_column %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

   tidy_column %>% 
    inner_join(get_sentiments("bing"), "word") %>%
    count(word, sentiment, sort = TRUE) %>% 
    acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
    comparison.cloud(colors = c("coral1", "chartreuse4"), max.words = 100)
  
  bing_word_counts
  
#Bar chart for most common positive and negative words 
  bing_word_counts %>%
    group_by(sentiment) %>%
    slice_max(n, n = 10) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL,
         title = plot_title) +
    theme_bw()+
    theme(plot.title = element_text(color = "darkslateblue",hjust = 0.5, size = 22, face = "bold"))
  

}
  


getPosNegcount_Func("motivation_reason", "Variable: Motivation")
getPosNegcount_Func("concentration_reason", "Variable: Concentration")
getPosNegcount_Func("sleep_reason", "Variable: Sleep Disturbance")
getPosNegcount_Func("graduation_effect_reason", "Variable: Graduation Delayed/Effected")
getPosNegcount_Func("feeling_unhappiness_reason", "Variable: Unhappiness")
getPosNegcount_Func("internet_reason", "Variable: Internet Problems")
getPosNegcount_Func("full_courses_taken_reason", "Variable: Able to Take Full Courses")
getPosNegcount_Func("stranded_outside_cope", "Variable: Stranded Outside")
getPosNegcount_Func("financial_problem_affect", "Variable: Financial Problems")
getPosNegcount_Func("additional_notes", "Variable: Additional Notes")

#bar plot df_sentiment

ggplot(data=df_sentiment, aes(x=columnName, y=sentiment, fill=columnName)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(color = "darkslateblue", size = 18, face = "bold"))+
  guides(fill=guide_legend(title="Variable Name")) +
  labs(
    title = "Sentiment by Counting Positive & Negative Words",
    caption = "Figure 27"
  )





```


The sentiment analysis show that most of the words used throughout the questions had negative inclination. Only two columns ('motivation_reason', 'addional_notes') had an overall positive sentiment.



# **Final Analysis**

**What did you learn about the data?**

We curated the survey questions based on our own experiences of being a student that has seen a shift of academic and professional ecosystem in the last 1.5 years. This inspired the study, and therefore we wanted to understand what the students have been feeling during this time, the significant concerns they have, what affected their study? Did they get better results? etc. The data was not only open-text questions but also likert scale method of scoring and categorical data etc. We understood the demographics of survey respondents, their age, location, etc.

For future studies, we can incorporate further questions such as analysing in terms of their semesters, whether they or their loved ones were diagnosed with the Covid-19 or not.


**How did you answer the questions?**

We started the basic pre-processing steps of cleaning the data, removing NAs, converting text data to lower case, and also stopword removal. We found answers to our research questions by performing a sentiment analysis on text data. We also tried to find a correlation between different variables. These were integral to find out whether each variable contributes to the other or not. We also ran ML models such as Logistic Regression to predict grades of Masters students. The most essential struggle for us was not having enough data. We highly emphasize future work where we can have a higher number of survey respondents - ideally more than 100, so we could have better training and test data split and offer more promising results of the study.


# **Screencast & Website**

[**_Project Screencast_**](https://www.youtube.com/watch?v=wetSjJo5qoo)



[**_Project Website_**](https://sidraaaziz.wixsite.com/projectpandemic)

# **Questionnaire**

The following are the list of questions used in our survery:

```{r questionnaire,warning=FALSE, message=FALSE, echo=FALSE }

dt_questions = colnames(df_data_questions[-2])

kbl(dt_questions) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

```

# **References**


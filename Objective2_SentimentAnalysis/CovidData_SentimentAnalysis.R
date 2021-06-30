library(tidyverse)
library(tidymodels)
library(readxl)
library(dplyr)

library(SnowballC)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)


#importing the dataset in form of dataframe using survey-excel sheet
df_data = read_excel("/Users/sidraaziz/RProject_Covid19/Survey.xlsx")

#EDA by examining the dataframe using basic functions such as dim, summary, colnames
dim(df_data)
str(df_data)
summary(df_data)
colnames(df_data)

#Converting the column names to much more variable measure]
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

colnames(df_data)
df_data <- df_data[, -c(1:2)] # delete columns 1 & 2

view(df_data)


# columns to be changed from character to factor
cols.char <- c("living_setup","concentration","sleep","education_strain","graduation_effect",
               "feeling_unhappiniess","insititute_assistance","internet","job_lost","full_courses_taken","stranded_outside",
               "financial_problem","results_better") 

# change character to factors for chosen columns
df_data[cols.char] <- lapply(df_data[cols.char], factor)  

word_count_func <- function(column_name) {
  
  corpus = Corpus(VectorSource(df_data[column_name]))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  
  tdm = TermDocumentMatrix(corpus)
  m = as.matrix(tdm)
  v = sort(rowSums(m), decreasing = TRUE)
  d = data.frame(word = names(v),freq = v)
  
  wordcloud(d$word, d$freq, random.order = FALSE,
            rot.per = 0.3, scale = c(4,.5),
            colors = brewer.pal(8,"Dark2"), max.words = 200, min.freq = 2)
  
  findFreqTerms(tdm, lowfreq = 2)
  
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
          col ="pink", main ="Most frequent words",
          ylab = "Word frequencies")
  
  findAssocs(tdm, terms = c("online","classes","different"), corlimit = 0.10)			
  # Find associations for words that occur at least 3 times
  #findAssocs(tdm, terms = findFreqTerms(tdm, lowfreq = 3), corlimit = 0.25)
  
  
}

word_count_func("motivation_reason")
word_count_func("concentration_reason")
word_count_func("sleep_reason")
word_count_func("graduation_effect_reason")
word_count_func("feeling_unhappiness_reason")
word_count_func("internet_reason")
word_count_func("full_courses_taken_reason")
word_count_func("stranded_outside_cope")
word_count_func("financial_problem_affect")
word_count_func("additional_notes")

df_country_count = df_data %>% 
  group_by(country) %>% 
  count()

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
library(tidytext)
library(dplyr)
library(glue) #for pasting strings
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
view(Motivation_sentiment)
Concentration_sentiment <- getSentiment_Func("concentration_reason")
view(Concentration_sentiment)
Sleep_sentiment <- getSentiment_Func("sleep_reason")
view(Sleep_sentiment)
Graduation_sentiment <- getSentiment_Func("graduation_effect_reason")
view(Graduation_sentiment)
Unhappiness_sentiment <- getSentiment_Func("feeling_unhappiness_reason")
view(Unhappiness_sentiment)
Internet_sentiment <- getSentiment_Func("internet_reason")
view(Internet_sentiment)
FullCoursesTaken_sentiment <- getSentiment_Func("full_courses_taken_reason")
view(FullCoursesTaken_sentiment)
Stranded_sentiment <- getSentiment_Func("stranded_outside_cope")
view(Stranded_sentiment)
Financialprob_sentiment <- getSentiment_Func("financial_problem_affect")
view(Financialprob_sentiment)
AdditionalNotes_sentiment <- getSentiment_Func("additional_notes")
view(AdditionalNotes_sentiment)

df_sentiment = Reduce(function(x, y) merge(x, y, all=TRUE), list(
  Motivation_sentiment, Concentration_sentiment, Sleep_sentiment, Graduation_sentiment,
  Unhappiness_sentiment,FullCoursesTaken_sentiment,Stranded_sentiment,Financialprob_sentiment,
  AdditionalNotes_sentiment))

view(df_sentiment)

#Most common positive and negative words and 
#positive vs. negative sentiments word cloud together
getPosNegcount_Func <- function(column_name) {
  tidy_column <- df_data %>%
    unnest_tokens(word, column_name)
  
  bing_word_counts <- tidy_column %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  library("reshape2")
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
         y = NULL)
}
getPosNegcount_Func("motivation_reason")
getPosNegcount_Func("concentration_reason")
getPosNegcount_Func("sleep_reason")
getPosNegcount_Func("graduation_effect_reason")
getPosNegcount_Func("feeling_unhappiness_reason")
getPosNegcount_Func("internet_reason")
getPosNegcount_Func("full_courses_taken_reason")
getPosNegcount_Func("stranded_outside_cope")
getPosNegcount_Func("financial_problem_affect")
getPosNegcount_Func("additional_notes")











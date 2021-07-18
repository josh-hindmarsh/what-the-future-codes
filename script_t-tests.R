#This script takes the raw E-merged flanker data and creates a 
#new variable called congruency, in order to conduct several t-tests to compare where 
#mean error rates and mean RTs were different for congruent vs incongruent trials.

install.packages("ggpubr")
install.packages("rstatix")

library(ggpubr)
library(rstatix)
library(tidyverse)
library(psych)

raw_flanker <- read.delim("~/Master 2020-21/4.5 Thesis and Internship/Data/data cleaning in excel/Josh_merge_raw_for_R.txt")

raw_flanker <- raw_flanker %>%      #select appropirate columns
  select(Subject,
         Balance,
         StimulusRespons.ACC,
         StimulusRespons.RT,
         Trial,
         List1) %>%
  mutate_if(is.character,as.numeric) %>%
  drop_na(List1)%>%  #drop practise trials
  mutate(congruency = case_when(  #new column to indicate (in)congruency
    List1 %in% c(1, 3) ~ "congruent", 
    List1 %in% c(2, 4) ~ "incongruent")) %>% 
    filter(Subject != 11184, 11183, 11062, )

#t-test for difference in RTs
#complete

    sum_flanker <- raw_flanker %>%
      group_by(congruency, Subject) %>%
      summarise(mean_RT = mean(StimulusRespons.RT))
      
    describeBy(sum_flanker, group="congruency")   #get means and SDs
    #congruent: M = 421.33, SD = 44.75
    #incongruent: M = 443.18, SD = 48.51
    
    t.test(mean_RT ~ congruency, sum_flanker, paired = TRUE) %>%
      add_significance()
    #t = -15.139, df = 178, p-value < 2.2e-16
    #alternative hypothesis: true difference in means is not equal to 0
    #95 percent confidence interval: -24.70338 -19.00582

#t-test for difference in error rates

sum_flanker2 <- raw_flanker %>%
  filter(StimulusRespons.ACC == 0) %>%
  group_by(congruency, Subject) %>%
  summarise(error_count = count(StimulusRespons.ACC))

#t-test for difference in RTs based on error/correct


#t-test for difference in post-error mean RT vs post-correct mean RT


##### t-test difference between ERN and CRN #




  
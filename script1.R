#### SECTION 1: DATA IMPORT AND CLEANING ####
  #all packages /libraries ####
  
  install.packages("haven")
  install.packages("tidyverse")
  install.packages("psych")
  install.packages("stringr")
  devtools::install_github("doomlab/MeMoBootR")
  install.packages("reshape2")
  install.packages("apaTables")
  remotes::install_github("craddm/eegUtils@develop")
  install.packages("ggpubr")
  install.packages("rstatix")
  install.packages("ggfortify")
  
  library("readxl")
  library("haven")
  library("tidyverse")
  library("psych")
  library("stringr")
  library(MeMoBootR)
  library(reshape2)
  library(apaTables)
  library(eegUtils)
  library(ggpubr)
  library(rstatix)
  library(ggfortify)
  
  #import questionnaire data ####
  data <- read_excel("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/IU merged ENG+NL_1187.xlsx") %>%
    arrange(ID)
  
  #move recent unprocessed data (ID 1120 onwards) for recoding to ensure previously recoded data is not double-handled
  #no need for this step if original data is raw
  new_data <- data %>%
    filter(ID >= 1120)
  
  #calculate composite scores####
  
  #calculate Intolerance of Uncertainty Scale composite scores
  new_data <- new_data %>%
    rowwise() %>%
    mutate(IUS27_total = sum(c_across(IoU_1:IoU_21))) %>%
    mutate(IUS12_total = sum(IoU_7, IoU_8, IoU_9, IoU_10, IoU_11, 
                                IoU_12, IoU_15, IoU_18, IoU_19, IoU_20,
                                IoU_21, IoU_25)) %>%
    mutate(P_IU = sum(IoU_7, IoU_8, IoU_10, IoU_11, IoU_18, IoU_19, IoU_21)) %>%
    mutate(I_IU = sum(IoU_9, IoU_12, IoU_15, IoU_20, IoU_25))
  
  #Beck Depression Inventory (recode + composite)
  new_data <- new_data %>%
    rowwise() %>%
    mutate_at(vars(BDI_01:BDI_15, BDI_17, BDI_19:BDI_21),
              car::recode, "1=0; 2=1; 3=2; 4=3") %>%
    mutate_at(vars(BDI_16, BDI_18),
              car::recode, "1=0; 2=1; 3=1; 4=2; 5=2; 6=3; 7=3") %>%
    mutate(BDI_total = sum(c_across(BDI_01:BDI_21)))
  
  
  #State-Trait Anxiety Inventory -- State subscale (recode + composite)
  new_data <- new_data %>%
    rowwise() %>%
    mutate_at(vars(STAIS_1, STAIS_2, STAIS_5, STAIS_8, STAIS_10, 
                   STAIS_11, STAIS_15, STAIS_16, STAIS_19, STAIS_20),
              car::recode, "1=4; 2=3; 3=2; 4=1") %>%
    mutate(STAI_S = sum(c_across(STAIS_1:STAIS_20)))
  
  
  #State-Trait Anxiety Inventory -- Trait subscale (recode + composite)
  new_data <- new_data %>%
    rowwise() %>%
    mutate_at(vars(STAIT_1, STAIT_3, STAIT_6, STAIT_7, STAIT_10,
                   STAIT_13, STAIT_14, STAIT_16, STAIT_19),
              car::recode, "1=4; 2=3; 3=2; 4=1") %>%
    mutate(STAI_T = sum(c_across(STAIT_1:STAIT_20)))
  
  #Penn State Worry Questionnaire (recode + composite)
  new_data <- new_data %>%
    rowwise() %>%
    mutate_at(vars(PSWQ_1_1, PSWQ_1_3, PSWQ_1_8, PSWQ_1_10, PSWQ_1_11),
              car::recode, "1=5; 2=4; 3=3; 4=2; 5=1") %>%
    mutate(PSWQ_total = sum(c_across(PSWQ_1_1:PSWQ_1_16)))
  
  #merge new and old data
  new_data <- new_data %>% 
    arrange(ID)
  
  data <- data %>% 
    filter(ID <= 1119) %>%
    bind_rows(new_data) %>%
    arrange(ID)
  
  #check questionnaires for impossible scores####
      #IUS27_total: min 27, max 135
      #IUS12_total: min 12, max 60
      #P_IU: min 7, max 35
      #I_IU: min 5, max 25
      #BDI_total: min 0, max 63
      #STAI_S: min 20, max 80
      #STAI_T: min 20, max 80
      #PSWQ_total: min 16, max 80
  
  data %>% 
    select(IUS27_total:PSWQ_total) %>% 
    describe()
  
      #all scores seem plausible
  
  #re-label ethnicity variables ####
  #You could use similar functions to relabel sex and survey_code as well
  
  unique(data$ethnicity)  #See list. Search for NA and add any possible variations below.
  
  data <- data %>%
    arrange(ethnicity) %>%
    mutate(ethnicity_code = case_when(
      ethnicity %in% c("White", "white", "Wit", "wit", "Dutch", "Caucasian", "wit/nederlands", 
                       "White, European", "wit... maar dit is wel een aparte manier van vragen hiernaar", 
                       "Nederlandse, wit", "Nederlands", 'Nederlands, wit') ~ 1, 
      ethnicity %in% c("Black", "black", "zwart", "Surinaams, zwart") ~ 2, 
      ethnicity %in% c("Asian", "asian", "aziaat", "Aziaat", "Aziaat (Chinees)", "Chinese", "South East Asian") ~ 3,
      ethnicity %in% c("middle eastern", "White (middle eastern/arab)", "arabisch", "Midden-oosten", 
                       "midden oosten", "middle eastern", "Middle Eastern", "turkish", "Turks (aziatisch)") ~ 4,
      ethnicity %in% c("mixed", "Azerbaijani/White", "mix wit zwart midden oosten", 
                       "mixed", "zwart/wit", "white/latino", "mix black/white", "Nederlands/afghaans") ~ 5,
      ethnicity %in% c("latino", "Latino") ~ 6,
      ethnicity %in% c("other", "Armenian", "cypriot", "Indian", "Egyptian", 
                       "Noord-Afrikaans", "Surinaams-Hindoestaans") ~ 7)) %>%
    mutate_at(vars(ethnicity_code), ~replace_na(., 0)) %>%
    add_row(ID = "1183") %>%        #add missing participants, to have all consecutive ppt numbers (some will be removed later)
    add_row(ID = "1062") %>%
    arrange(ID)
  
  #import/merge EEG data, compute difference score ####
  eeg_1029 <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/Josh_ERN_values_1029.txt", sep="", comment.char="#") %>%
    rename(ID2 = File, CRN = Cz.Av_correct, ERN = Cz.Av_incorrect) %>%
    arrange(ID2) %>%
    select(ID2, CRN, ERN) %>%  
    mutate(deltaERN = ERN - CRN) %>%
    mutate(ID2 = ifelse(row_number() == 1, "11029"))
  
  eeg_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/Josh_ERN_values.txt", sep="", comment.char="#") %>%
    rename(ID2 = File, CRN = Cz.Av_correct, ERN = Cz.Av_incorrect) %>%
    arrange(ID2) %>%
    select(ID2, CRN, ERN) %>%  
    mutate(deltaERN = ERN - CRN) %>%
    rbind(eeg_1029) %>%
    add_row(ID2 = "11062") %>%
    add_row(ID2 = "11130") %>%
    add_row(ID2 = "11138") %>%
    add_row(ID2 = "11184") %>%
    arrange(ID2)
  
  #import behavioural data ####
  beh_data <- read_excel("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/Josh_flanker_behavioural_v2.xlsx")
  
  beh_data <- beh_data %>%
    rename(ID1 = ID) %>%
    mutate(ID1 = replace(ID1, ID1 == 1009, 11009)) %>%
    mutate(ID1 = replace(ID1, ID1 == 1010, 11010)) %>%
    mutate(ID1 = replace(ID1, ID1 == 31022, 11022)) %>%
    mutate(ID1 = replace(ID1, ID1 == 31029, 11029)) %>%
    add_row(ID1 = 11062) %>%       #add missing participants so rows match
    add_row(ID1 = 11130) %>%
    add_row(ID1 = 11184) %>%
    arrange(ID1) %>%
    filter(ID1 <=11185)             #remove 186-188 in questionnaire data (we dont have EEG data for them)
    
  #merge eeg, questionnaire and behavioural dataframes ####
  data <- data %>%
    filter(ID <=1185)     #remove 186-188 in questionnaire data (we dont have EEG data for them)
  
  full_data <- bind_cols(beh_data, eeg_data, data) %>%      #merge both dataframes (N = 185) -- use merge() instead with all.x
    drop_na(CRN) %>%                    #drop missing EEG data (N = 181)
    drop_na(PSWQ_total) %>%                       #drop missing questionnaire data (N = 180)
    drop_na(Flanker_accurate)                     #drop missing behavioural data (N = 180)
  
  count(full_data)
  
  #full sample descriptive statistics ####
    descriptives1 <- full_data %>%
    select(Age, sex, ethnicity_code) %>%
    drop_na(Age, sex) %>%
    mutate_if(is.character,as.numeric)
  
  describe(descriptives1)
  
  count(descriptives1, ethnicity_code)
  
  
  
#### SECTION 2: DATA SCREENING AND PRELIMINARY ANALYSES #####

  #run exclusion criteria####
  #participants excluded based on ppts with > 20% missed trials, <55% accuracy
  #(faster than 200m has not been excluded, has to be done with raw data, see flanker_eprime_processing.R)
  
  full_data <- full_data %>%
    filter(Flanker_accurate > 0.55*400) 
  
  count(full_data) #(N = 170)
  
  full_data <- full_data %>%
    filter(Flanker_missed < 0.20*400) 
  
  count(full_data) #(N = 170)
  
  #post-exclusion sample (n = 170) descriptives ####
  
  descriptives2 <- full_data %>%
    select(Age, sex, ethnicity_code) %>%
    drop_na(Age, sex) %>%
    mutate_if(is.character,as.numeric)
  
  describe(descriptives2)
  
  count(descriptives2, ethnicity_code)
  
  #generate correlation matrix with means and SDs ####
  cor_matrix <- full_data %>%
    select(deltaERN, IUS12_total, P_IU, I_IU, STAI_T, BDI_total, PSWQ_total, Age, sex) %>%
    mutate_if(is.character,as.numeric)
  
    apa.cor.table(cor_matrix)
    
  #describe behavioural data ####
  beh_data_subset <- full_data %>% #subset of behavioural data after exclusion (n = 170)
      select(ID1, Flanker_accurate:Mean_PE)
    
  describe(beh_data_subset)
  
  #behavioural data analyses (t-tests) ####
  
  data2 <- data %>% filter(!complete.cases(.))  #check the case numbers that were dropped due to NA (to be added below)
  
  raw_flanker <- read.delim("C:/Users/Josh/Downloads/eprime/merge_again_v2.txt")
  
  raw_flanker <- raw_flanker %>%      #select appropriate columns
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
      filter(Subject < 11186) %>%           #use correct dataset!          
      filter(Subject != 11069) %>%          #participants removed for failing the exclusion criteria of too many inaccurate responses
      filter(Subject != 11077) %>%          #make this section more elegant e.g.,  filter(!ID1 %in% c(11008, 11028, 11043, 11158))
      filter(Subject != 11079) %>%
      filter(Subject != 11088) %>%
      filter(Subject != 11116) %>%
      filter(Subject != 11134) %>%
      filter(Subject != 11147) %>%
      filter(Subject != 11155) %>% 
      filter(Subject != 11159) %>% 
      filter(Subject != 11173)
  
  count(raw_flanker) / 400  #should be n = 170 like final sample from full_data
  
  #1) t-test for difference in RTs
  #complete
  
  sum_flanker <- raw_flanker %>%
    group_by(congruency, Subject) %>%
    summarise(mean_RT = mean(StimulusRespons.RT))
  
  describeBy(sum_flanker, group="congruency")   #get means and SDs
  #congruent: M = 428.18, SD = 25.82 
  #incongruent: M = 450.75, SD = 30.76
  
  t.test(mean_RT ~ congruency, sum_flanker, paired = TRUE) %>%
    add_significance()
  #t = -15.87, df = 169, p-value < 2.2e-16
  #alternative hypothesis: true difference in means is not equal to 0
  #95 percent confidence interval: -25.37, -19.76
  
  #2) t-test for difference in error rates
  #complete, but check M and SD
  
  sum_flanker2 <- raw_flanker %>%
    group_by(congruency, Subject) %>%
    summarise(error_count = sum(StimulusRespons.ACC == 0))
  
  describeBy(sum_flanker2, group="congruency")   #get means and SDs
  #congruent: M = 22.09   SD = 15.66
  #incongruent: M = 44.78   SD = 21.14
  
    t.test(error_count ~ congruency, sum_flanker2, paired = TRUE) %>%
    add_significance()
  #t = -25.33, df = 169, p-value < 2.2e-16
  #alternative hypothesis: true difference in means is not equal to 0
  #95 percent confidence interval: -24.46, -20.91
    
  
  #3) t-test for difference in RTs based on error/correct
    describeBy(raw_flanker, group="StimulusRespons.ACC")   #get means and SDs
    #error (0): M = 297.44 (SD = 204.69)
    #correct (1): M = 467.97   (SD = 87.08)
    
    sum_flanker3 <- raw_flanker %>%
      group_by(StimulusRespons.ACC, Subject) %>%
      summarise(mean_RT = mean(StimulusRespons.RT))
    
    t.test(mean_RT ~ StimulusRespons.ACC, sum_flanker3, paired = TRUE) %>%
      add_significance()
    #data:  mean_RT by StimulusRespons.ACC
    #t = -34.08, df = 169, p-value < 2.2e-16
    #95 percent confidence interval: -177.70 -158.24
  
  
  #4) t-test for difference in post-error mean RTs vs post-correct mean RTs
    #REPEAT WITH FINAL SAMPLE, THIS IS ONLY 73 SUBJECTS
    
    PES <- PES %>% 
      pivot_longer('AVG PRE':'AVG POST', names_to = "Condition", values_to = "RT") %>%
      group_by(Condition, X1)
    
    PES <- full_data %>% 
      select(ID1, Mean_PC, Mean_PE) %>% 
      pivot_longer(Mean_PC:Mean_PE, names_to = "Condition", values_to = "RT") %>%
      group_by(Condition, ID1)
 

    t.test(RT ~ Condition, PES, paired = TRUE) %>%
      add_significance()  
    #t = -13.044, df = 169, p-value < 2.2e-16
    #   95 percent confidence interval: (-39.26, -28.94)
  
  #ERP analyses ####
  #1) t-test difference between ERN and CRN
  
  ERP_long <- full_data %>% 
      select(ID1, ERN, CRN) 

    describeBy(ERP_long) 
    #ERN     M(SD) = 0.18  (3.48)
    #CRN    M(SD) = 3.41  (2.41)
    
   ERP_long <- ERP_long %>% 
      pivot_longer('ERN':'CRN', names_to = "erp", values_to = "amp") %>%
      group_by(erp, ID1)
    
    t.test(amp ~ erp, ERP_long, paired = TRUE) %>%
      add_significance()
    #data:  amp by erp
    #t = 13.155, df = 169, p-value < 2.2e-16
    #alternative hypothesis: true difference in means is not equal to 0
    #95 percent confidence interval: 2.74, 3.71

    
#### SECTION 3: MEDIATION ANALYSES ####


        #dont forget to remove statistical results code
#Hypothesis 1: ERN predicting STAI-T, with P-IU as mediator (without outliers)#####

saved = mediation1(y = "STAI_T", #DV
                   x = "deltaERN", #IV
                   m = "P_IU",  #Mediator
                   cvs = c("PSWQ_total", "BDI_total", "I_IU"), #Any covariates
                   df = full_data, #Dataframe
                   with_out = F, #Not required but can change to F for no outliers
                   nboot = 5000, #Number of bootstraps
                   conf_level = .95 #CI width
)
  
  ####view data screening####
    
  #outlier information is in the DF
  View(saved$datascreening$fulldata)  #bad cases: 11008, 11028, 11043, 11158
    
  #dataset without outliers removed
    
  hyp1_clean <- full_data %>% 
    filter(!ID1 %in% c(11008, 11028, 11043, 11158))
    
  #fake regression to get assumption chart
  
  model1 <- 
    lm(STAI_T ~ deltaERN + + P_IU + PSWQ_total + BDI_total + I_IU, data=hyp1_clean)
  
  autoplot(model1, which=c(1,2,3), ncol = 3)    #assumptions can be checked with this output
    
  
  ####view the analysis####
  summary(saved$model1) #c path
  #deltaERN  b=  -0.28592   se= 0.12697 t= -2.252  p = 0.0257 *
  
  summary(saved$model2) #a path
  #
  
  summary(saved$model3) #b and c' path
  
  
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
#Hypothesis 2: ERN predicting STAI-T, with I-IU mediator (without outliers)#### 

saved = mediation1(y = "STAI_T", #DV
                   x = "deltaERN", #IV
                   m = "I_IU",  #Mediator
                   cvs = c("PSWQ_total", "BDI_total", "P_IU"), #Any covariates
                   df = full_data, #Dataframe
                   with_out = F, #Not required but can change to F for no outliers
                   nboot = 5000, #Number of bootstraps
                   conf_level = .95 #CI width
)

  ####view data screening####
  
  #outlier information is in the DF
  View(saved$datascreening$fulldata)    #bad cases: 11008, 11028, 11043, 11158
  
  hyp2_clean <- full_data %>% 
    filter(!ID1 %in% c(11008, 11028, 11043, 11158))
  
  #fake regression to get assumption chart
  
  model2 <- 
    lm(STAI_T ~ deltaERN + + P_IU + PSWQ_total + BDI_total + I_IU, data=hyp2_clean)
  
  autoplot(model2, which=c(1,2,3), ncol = 3)    #assumptions can be checked with this output
  
  #additivity
  saved$datascreening$correl
  
  #linearity
  saved$datascreening$linearity
  
  #normality
  saved$datascreening$normality
  
  #homogs
  saved$datascreening$homogen
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  

#Hypothesis 3: ERN predicting Depression, with Total IU as mediator (without outliers)#### 
  
  saved = mediation1(y = "BDI_total", #DV
                     x = "deltaERN", #IV
                     m = "IUS12_total",  #Mediator
                     cvs = c("PSWQ_total", "STAI_T"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view data screening####
  
  #outlier information is in the DF
  View(saved$datascreening$fulldata)    #bad cases: 11028, 11043, 11158
  
  hyp3_clean <- full_data %>% 
    filter(!ID1 %in% c(11028, 11043, 11158))
  
  #fake regression to get assumption chart
  
  model3 <- 
    lm(BDI_total ~ deltaERN + IUS12_total + PSWQ_total, data=hyp2_clean)
  
  autoplot(model3, which=c(1,2,3), ncol = 3)    #assumptions can be checked with this output
  
  #additivity
  saved$datascreening$correl
  
  #linearity
  saved$datascreening$linearity
  
  #normality
  saved$datascreening$normality
  
  #homogs
  saved$datascreening$homogen
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  
#### SECTION 4: EXPLORATORY ANALYSES ####

#Exploratory 1: Trait Anxiety from ERN via Total IUS12#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "IUS12_total",  #Mediator
                     cvs = c("PSWQ_total", "BDI_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci

#Exploratory 2: Trait Anxiety from ERN via Total IUS12 (less covariates)#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "IUS12_total",  #Mediator
                     cvs = c("PSWQ_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
   
#Exploratory 3: Depression from ERN via P-IU#### 
  
  saved = mediation1(y = "BDI_total", #DV
                     x = "deltaERN", #IV
                     m = "P_IU",  #Mediator
                     cvs = c("PSWQ_total", "I_IU"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
#Exploratory 4: Depression from ERN via I-IU#### 
  
  saved = mediation1(y = "BDI_total", #DV
                     x = "deltaERN", #IV
                     m = "I_IU",  #Mediator
                     cvs = c("PSWQ_total", "P_IU"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
#Exploratory 5: Trait Anxiety from ERN via Worry (2 covariates)#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "PSWQ_total",  #Mediator
                     cvs = c("IUS12_total", "BDI_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
#Exploratory 6: Trait Anxiety from ERN via Worry (1 covariate)#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "PSWQ_total",  #Mediator
                     cvs = c("IUS12_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
#Exploratory 7: Depression from ERN via Worry (2 covariates)#### 
  
  saved = mediation1(y = "BDI_total", #DV
                     x = "deltaERN", #IV
                     m = "PSWQ_total",  #Mediator
                     cvs = c("IUS12_total", "STAI_T"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view the analysis####
  summary(saved$model1) #c path
  summary(saved$model2) #a path
  summary(saved$model3) #b and c' path
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  
  #Moderation analyses (remove if not using, otherwise fix) ####
  
  #moderation 1: age moderates relationship between ERN and anxiety, ####
  #when controlling for worry and depression! (YES!)
  
  full_data2 <-  full_data %>% 
    mutate_if(is.character,as.numeric)
  
  saved = moderation1(y = "STAI_T", #DV
                      x = "deltaERN", #IV
                      m = "Age", #Moderator for simple slopes
                      cvs = c("PSWQ_total", "BDI_total", "IUS12_total"), #covariates
                      df = full_data2, #data frame of columns
                      with_out = F)
  
  #View the outliers
  View(saved$datascreening$fulldata)
  
  #Additivity/multicollinearity
  saved$datascreening$correl
  
  #Linearity
  saved$datascreening$linearity
  
  #Normality
  saved$datascreening$normality
  
  #Homogeneity + Homoscedasticity
  saved$datascreening$homogen
  
  #Overall Model (Average Simple Slopes)
  summary(saved$model1)
  
  #Low Simple Slope Model
  summary(saved$model1low)
  
  #High Simple Slope Model
  summary(saved$model1high)
  
  #Interpretation of Slopes
  cat(saved$interpretation)
  
  #Graph of the Slopes
  saved$graphslopes
  
  ## alternative calculation
  


  
  #moderation 2: age moderates relationship between ERN and depression, when controlling for worry and anxiety! (NO!)####
  
  full_data2 <-  full_data %>% 
    mutate_if(is.character,as.numeric)
  
  saved = moderation1(y = "BDI_total", #DV
                      x = "deltaERN", #IV
                      m = "Age", #Moderator for simple slopes
                      cvs = c("PSWQ_total", "STAI_T"), #covariates
                      df = full_data2, #data frame of columns
                      with_out = F)
  
  #View the outliers
  View(saved$datascreening$fulldata)
  
  #Additivity/multicollinearity
  saved$datascreening$correl
  
  #Linearity
  saved$datascreening$linearity
  
  #Normality
  saved$datascreening$normality
  
  #Homogeneity + Homoscedasticity
  saved$datascreening$homogen
  
  #Overall Model (Average Simple Slopes)
  summary(saved$model1)
  
  #Low Simple Slope Model
  summary(saved$model1low)
  
  #High Simple Slope Model
  summary(saved$model1high)
  
  #Interpretation of Slopes
  cat(saved$interpretation)
  
  #Graph of the Slopes
  saved$graphslopes
  
  
  ####Hierarchical regressions ####
  #https://rtutorialseries.blogspot.com/2009/12/r-tutorial-series-multiple-linear.html
  #https://www.r-bloggers.com/2010/01/r-tutorial-series-hierarchical-linear-regression/
  
  #how much extra does ERN explain in anxiety
  
  #(model1)Predict STAI from ERN with Worry and Depression
  three.predictor.model <- lm(STAI_T ~ PSWQ_total + BDI_total + IUS12_total, hyp1_clean)
  summary(three.predictor.model)
  
  #(model2)Predict STAI from ERN with worry, depression and IUS12
  four.predictor.model <- lm(STAI_T ~ deltaERN + PSWQ_total + BDI_total + IUS12_total, hyp1_clean)
  summary(four.predictor.model)
  
  #(compare model1 and 2, see if model 2 adds anything)
  anova(three.predictor.model, four.predictor.model)
  
  difference!
    
    #how much extra does ERN explain in depression
    
    #(model1)Predict STAI from ERN with Worry and Depression
    three.predictor.model <- lm(BDI_total ~ PSWQ_total + STAI_T + IUS12_total, hyp1_clean)
  summary(three.predictor.model)
  
  #(model2)Predict STAI from ERN with worry, depression and IUS12
  four.predictor.model <- lm(BDI_total ~ deltaERN + PSWQ_total + STAI_T + IUS12_total, hyp1_clean)
  summary(four.predictor.model)
  
  #(compare model1 and 2, see if model 2 adds anything)
  anova(three.predictor.model, four.predictor.model)
  
  #no difference
  
  #subtypes as predictors for anxiety
  
  #(model1)Predict STAI from ERN with Worry and Depression
  three.predictor.model <- lm(STAI_T ~ PSWQ_total + BDI_total + I_IU + P_IU, hyp1_clean)
  summary(three.predictor.model)
  
  #(model2)Predict STAI from ERN with worry, depression and IUS12
  four.predictor.model <- lm(STAI_T ~ deltaERN + PSWQ_total + BDI_total + I_IU + P_IU, hyp1_clean)
  summary(four.predictor.model)
  
  #(compare model1 and 2, see if model 2 adds anything)
  anova(three.predictor.model, four.predictor.model)
  
  #no difference
  
  #how much does IUS add on top of anxiety
  
  #(model1)Predict STAI from ERN with Worry and Depression
  three.predictor.model <- lm(STAI_T ~ PSWQ_total + BDI_total + deltaERN, hyp1_clean)
  summary(three.predictor.model)
  
  #(model2)Predict STAI from ERN with worry, depression and IUS12
  four.predictor.model <- lm(STAI_T ~ PSWQ_total + BDI_total + deltaERN + IUS12_total, hyp1_clean)
  summary(four.predictor.model)
  
  #(compare model1 and 2, see if model 2 adds anything)
  anova(three.predictor.model, four.predictor.model)  
  
  #no difference
  
  #how much does IUS add on top of depression
  
  #(model1)Predict STAI from ERN with Worry and Depression
  three.predictor.model <- lm(BDI_total ~ PSWQ_total + STAI_T + deltaERN, hyp1_clean)
  summary(three.predictor.model)
  
  #(model2)Predict STAI from ERN with worry, depression and IUS12
  four.predictor.model <- lm(BDI_total ~ PSWQ_total + STAI_T + deltaERN + IUS12_total, hyp1_clean)
  summary(four.predictor.model)
  
  #(compare model1 and 2, see if model 2 adds anything)
  anova(three.predictor.model, four.predictor.model)  
  
  #no difference
  
  #how much variation does trait anxiety explain in depression and vice versa
  
  model_200 <- lm(formula = BDI_total ~ STAI_T , data = hyp1_clean)
  summary(model_200)
  
  model_300 <- lm(formula = STAI_T ~ BDI_total, data = hyp1_clean)
  summary(model_300)
  
  #composite score for anxious-depression ####
  #standardize and then average
  
  full_data_standardized <- full_data %>% 
    mutate(zdep = (BDI_total - mean(BDI_total)) / sd(BDI_total)) %>% 
    mutate(zanx = (STAI_T - mean(STAI_T)) / sd(STAI_T)) %>%
    mutate(z_anxdep = (zanx + zdep) /2)
  
    #IUS as mediator ####
    
    saved = mediation1(y = "z_anxdep", #DV
                       x = "deltaERN", #IV
                       m = "IUS12_total",  #Mediator
                       cvs = c(), #Any covariates
                       df = full_data_standardized, #Dataframe
                       with_out = F, #Not required but can change to F for no outliers
                       nboot = 5000, #Number of bootstraps
                       conf_level = .95 #CI width
    )
    
    
    
    ####view the analysis
    summary(saved$model1) #c path
    
    summary(saved$model2) #a path
    
    summary(saved$model3) #b and c' path
    
    
    
    #total, direct, indirect effects
    saved$total.effect; saved$direct.effect; saved$indirect.effect
    
    #bootstrapped indirect
    saved$boot.results
    
    #bootstrapped CI
    saved$boot.ci
    
    
    
    #worry as mediator ####
    
    saved = mediation1(y = "z_anxdep", #DV
                       x = "deltaERN", #IV
                       m = "PSWQ_total",  #Mediator
                       cvs = c("IUS12_total"), #Any covariates
                       df = full_data_standardized, #Dataframe
                       with_out = F, #Not required but can change to F for no outliers
                       nboot = 5000, #Number of bootstraps
                       conf_level = .95 #CI width
    )
    
    ####view the analysis
    
    summary(saved$model1) #c path
    #deltaERN  b=  -0.28592   se= 0.12697 t= -2.252  p = 0.0257 *
    
    summary(saved$model2) #a path
    #
    
    summary(saved$model3) #b and c' path
    
    
    
    #total, direct, indirect effects
    saved$total.effect; saved$direct.effect; saved$indirect.effect
    
    #bootstrapped indirect
    saved$boot.results
    
    #bootstrapped CI
    saved$boot.ci
    
    
  
  
    
#### SECTION 4: REGRESSION ANALYSES ####
  
#1) ERN from P-IU and I-IU
    
    boxplot.stats(full_data$ERN)$out  #screen for univariate ERP outliers as per Jackson
    
    out <- boxplot.stats(full_data$ERN)$out
    out_ind <- which(full_data$ERN %in% c(out))
    out_ind
    
    full_data[out_ind, ] #outlier ERN data: 11028, 11056, 11060, 11091, 11141, 11144, 11158
    
    ERNIU_data <- full_data %>% 
      filter(!ID1 %in% c(11028, 11056, 11060, 11091, 11141, 11144, 11158))  #remove outliers
    
    model_ERNIU <- lm(ERN ~ P_IU + I_IU, ERNIU_data)    #model without outliers
    
    autoplot(model_ERNIU)   #assumption diagnostics without outliers (previously normality was violated, 
                            #without outliers it is okay)
    
    summary(model_ERNIU)
    
#1a) followup deltaERN from P-IU and I-IU
    
    boxplot.stats(full_data$deltaERN)$out  #screen for univariate ERP outliers as per Jackson
    
    out <- boxplot.stats(full_data$deltaERN)$out
    out_ind <- which(full_data$deltaERN %in% c(out))
    out_ind
    
    full_data[out_ind, ]
    
    ERNIU_data2 <- full_data %>% 
      filter(!ID1 %in% c(11028, 11053, 11056, 11144, 11158))  #remove outliers
    
    model_ERNIU2 <- lm(deltaERN ~ P_IU + I_IU, ERNIU_data2)    #model without outliers
    
    autoplot(model_ERNIU2)   #assumption diagnostics without outliers
    
    summary(model_ERNIU2)    
    
#2a) av post-error RT
    
    model_PESIU <- lm(Mean_PE ~ P_IU + I_IU, full_data)    #model without outliers
    
    autoplot(model_PESIU)
    
    summary(model_PESIU)
    

#### SECTION 5: PLOTS ####

  #import EEG wave data #### 
  #(export GA node as generic data after upsampling to 1000Hz -- will explain)
  difference_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/for diagrams/test1000Hz.txt", sep="") %>%
    rename(Difference = Cz)
  
  ERN_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/for diagrams/test1000Hz_incorrect.txt", sep="") %>%
    rename(ERN = Cz)
  
  CRN_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/for diagrams/test1000Hz_correct.txt", sep="") %>%
    rename(CRN = Cz)
  
  wave_data <- bind_cols(ERN_data, CRN_data, difference_data) %>%
    mutate(time = -99:799) %>%
    filter(time < 400)
  
  #difference wave plot only####

  plot <- ggplot(wave_data,aes(time, Difference))+
    scale_color_brewer(palette = "Set1")+
    theme_classic()
  
  plot+
    stat_summary(fun = mean,geom = "line", size = 1)+
    labs(x = "Time (ms)",y = expression(paste("Amplitude ( ",mu,"V)")),colour = "")+
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed")+
    scale_y_reverse()+
    theme(axis.text = element_text(size = 15))+
    theme(axis.title = element_text(size = 18))
  
  #plot ERN, CRN and differnce ####
    #(still need to work out how to label the ERN and CRN peaks like this: 
    #https://ars.els-cdn.com/content/image/1-s2.0-S0167876020302452-gr1.jpg)
  
  wave_long <- melt(wave_data, id = "time", measure = c("Difference", "ERN", "CRN"))
  
  plot_all <- ggplot(wave_long, aes(time, value, colour = variable))+
    theme_classic()
    
  plot_all+
    stat_summary(fun = mean,geom = "line", size = 1)+
    labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
    scale_color_hue(labels = c("Difference", "Error trials", "Correct trials"))+
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed")+
    scale_y_reverse()+  #revese y axis
    theme(axis.text = element_text(size = 15))+   #larger axis text
    theme(axis.title = element_text(size = 18))+  #larger axis titles
    theme(legend.text = element_text(size = 15))+ #larger legend text
    annotate("text", x = 30, y = -2.5, label = "ERN")+  #ERN, CRN labels
    annotate("text", x = 30, y = 2, label = "CRN")  #ERN, CRN labels
    

#### SECTION 1: DATA IMPORT AND CLEANING ####
  #all packages /libraries ####
  
  install.packages("haven")
  install.packages("tidyverse")
  install.packages("psych")
  install.packages("stringr")
  devtools::install_github("doomlab/MeMoBootR")
  install.packages("reshape2")
  
  library("readxl")
  library("haven")
  library("tidyverse")
  library("psych")
  library("stringr")
  library(MeMoBootR)
  library(reshape2)
  
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
  #(still need to re-import with PES for RQ2)
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
  
  full_data <- bind_cols(beh_data, eeg_data, data) %>%      #merge both dataframes (N = 185)
    drop_na(CRN) %>%                    #drop missing EEG data (N = 181)
    drop_na(PSWQ_total) %>%                       #drop missing questionnaire data (N = 180)
    drop_na(Flanker_accurate)                     #drop missing behavioural data (N = 180)
  
  count(full_data)
  
  
#### SECTION 2: DATA SCREENING #####

  #run exclusion criteria####
#participants excluded based on ppts with > 20% missed trials, <55% accuracy
#(faster than 200m has not been excluded, has to be done with raw data, see flanker_eprime_processing.R)

full_data <- full_data %>%
  filter(Flanker_accurate > 0.55*400) %>%
  filter(Flanker_missed < 0.20*400)                                      

count(full_data) #(N = 170)






#### SECTION 3: MEDIATION ANALYSES ####


        #dont forget to remove statistical results code
#Hypothesis 1.1: ERN predicting STAIT, with P-IU as mediator (with outliers)#####

saved = mediation1(y = "STAI_T", #DV
                   x = "deltaERN", #IV
                   m = "I_IU",  #Mediator
                   cvs = c("PSWQ_total", "BDI_total", "P_IU"), #Any covariates
                   df = full_data, #Dataframe
                   with_out = T, #Not required but can change to F for no outliers
                   nboot = 5000, #Number of bootstraps
                   conf_level = .95 #CI width
)

  ####view data screening####
  #outlier information is in the DF
  View(saved$datascreening$fulldata)
  
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
  
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ####power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 
  

#Hypothesis 1.2: ERN predicting STAIT, with P-IU as mediator (without outliers)####

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
  View(saved$datascreening$fulldata)
  
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
  
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ####power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 
  
  

#Hypothesis 2.1: ERN predicting STAIT, with I-IU mediator (with outliers)####

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
  View(saved$datascreening$fulldata)
  
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
  
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ####power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 
  

#Hypothesis 2.2: ERN predicting STAIT, with I-IU mediator (without outliers)#### 

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
  View(saved$datascreening$fulldata)
  
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
  
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ###power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 


#Exploratory 1: Trait Anxiety from ERN via Total IUS12 (without outliers)#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "IUS12_total",  #Mediator
                     cvs = c("PSWQ_total", "BDI_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view data screening####
  #outlier information is in the DF
  View(saved$datascreening$fulldata)
  
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
  
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ###power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 
  
  
#Exploratory 2: Trait Anxiety from ERN via Total IUS12 (without outliers)#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "IUS12_total",  #Mediator
                     cvs = c("PSWQ_total", "BDI_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view data screening####
  #outlier information is in the DF
  View(saved$datascreening$fulldata)
  
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
  
  #remove the below
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ###power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 
  

#Exploratory 3: Trait Anxiety from ERN via Total IUS27 (without outliers)#### 
  
  saved = mediation1(y = "STAI_T", #DV
                     x = "deltaERN", #IV
                     m = "IUS27_total",  #Mediator
                     cvs = c("PSWQ_total", "BDI_total"), #Any covariates
                     df = full_data, #Dataframe
                     with_out = F, #Not required but can change to F for no outliers
                     nboot = 5000, #Number of bootstraps
                     conf_level = .95 #CI width
  )
  
  ####view data screening####
  #outlier information is in the DF
  View(saved$datascreening$fulldata)
  
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
  
  #X predicts Y total effects
  #c path b = -1.52, t(28) = -3.64, p = .001
  #F(3,28) = 46.53, p < .001, R2 = .83
  
  #X predicts M
  #a path b = 32.97, t(28) = 6.64, p < .001
  
  #X predicts Y with M direct effects
  #c' path b = -0.81, t(27) = -1.23, p = .231
  #M predicts Y with X 
  #b path b = -0.02, t(27) = -1.38, p = .179
  
  #total, direct, indirect effects
  saved$total.effect; saved$direct.effect; saved$indirect.effect
  
  #Sobel test
  saved$z.score; saved$p.value
  
  #bootstrapped indirect
  saved$boot.results
  
  #bootstrapped CI
  saved$boot.ci
  
  #indirect = -0.72, SE = 0.52, 95% CI[-1.81, 0.22]
  
  ###power####
  library(pwr)
  ##power runs on cohen's f - not to be confused with anova f. 
  ##take the R squared to convert
  R2 =  .06
  feta = R2 / (1-R2)
  
  #u is df model, which is number of predictors 
  #v is df error, but we are trying to figure out 
  #sample size so we leave this one blank. 
  #f2 is cohen f squared 
  
  pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)
  #remember that you need to add u + v to get final sample size 
  
  
#### SECTION X: REGRESSION ANALYSES?????? ####





#### SECTION X: PLOTS ####

  #testing eeg visualisations https://www.mattcraddock.com/blog/2016/09/19/comparing-two-erps/ (later)
    
  #import EEG wave data (export GA node as generic data after upsampling to 1000Hz -- will explain)
  difference_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/for diagrams/test1000Hz.txt", sep="") %>%
    rename(Difference = Cz)
  
  ERN_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/for diagrams/test1000Hz_incorrect.txt", sep="") %>%
    rename(ERN = Cz)
  
  CRN_data <- read.csv2("~/Master 2020-21/4.5 Thesis and Internship/Data/master_thesis/for diagrams/test1000Hz_correct.txt", sep="") %>%
    rename(CRN = Cz)
  
  wave_data <- bind_cols(ERN_data, CRN_data, difference_data) %>%
    mutate(time = -99:799) %>%
    filter(time < 400)
  
  #difference wave plot only

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
    theme(axis.title = element_text(size = 16))
  
  #plot three lines (still experimenting!)

  plot <- ggplot(wave_data,aes(time, Difference, ERN, CRN))+
    scale_color_brewer(palette = "Set1")+
    theme_classic()
  
  plot+
    stat_summary(fun = mean,geom = "line", size = 1)+
    labs(x = "Time (ms)",y = expression(paste("Amplitude ( ",mu,"V)")),colour = "")+
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed")+
    scale_y_reverse()+  #revese y axis
    theme(axis.text = element_text(size = 15))+   #larger axis text
    theme(axis.title = element_text(size = 16))   #larger axis titles

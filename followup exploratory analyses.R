
#ERN predicts anxiety when controlling for IU, worry and depression (this model explained more variance than when age was included) ####

full_data2 <-  full_data %>% 
  mutate_if(is.character,as.numeric)

saved = moderation1(y = "STAI_T", #DV
                    x = "deltaERN", #IV
                    m = "IUS12_total", #Moderator for simple slopes
                    cvs = c("PSWQ_total", "BDI_total"), #covariates
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

##overall model statistics
#F(5, 44) = 10.30, p < .001, R2 = .54

##each predictor one at a time
#Illiteracy b = -114.78, t(44) = -0.67, p = .508, not significant predictor
#Murder, also not signifificant
#Population, as population increases, income increases
#Area, as area increases, income increases
#Interaction b = -115.56, t(44) = -3.36, p = .002 - significant
#difficult to interpret...

#Low Simple Slope Model
summary(saved$model1low)

#look for is X because M is the slope we are manipulating
#Illiteracy not predictor of Income

#High Simple Slope Model
summary(saved$model1high)

#Illiteracy is predictor of income

#Interpretation of Slopes
cat(saved$interpretation)

##what does it mean if the interaction is significant but none of the simple
##slopes are significant...??
###implies the simple slopes are changing/different but that the main effect
###X to Y is not significant

#Graph of the Slopes
saved$graphslopes


#Hierarchical regressions ####
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
  
  no difference

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
    
    
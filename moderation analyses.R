saved = moderation1(y = "STAI_T", #DV
                    x = "deltaERN", #IV
                    m = "IUS12_total", #Moderator for simple slopes
                    cvs = c("PSWQ_total", "BDI_total"), #covariates
                    df = full_data, #data frame of columns
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

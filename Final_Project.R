getwd()
install.packages("psych")
library(stargazer) 
library(psych) 
library(ggplot2)

data <- read.csv("Education_Pak.csv")


data1 <- na.omit(data[,c("GirlsEnrolled",
                         "Educationscore", 
                         "EducationalBudgetSpendofGDP", 
                         "Enrolmentscore", 
                         "Genderparityscore",
                         "Learningscore",
                         "Numberofprimaryschools",
                         "Numberofsecondaryschools",
                         "Schoolinfrastructurescore",
                         "Totalnumberofschools",
                         "Electricity",
                         "Drinkingwater",
                         "Boundarywall",
                         "Toilet",
                         "Areakm")])

data1$GirlsEnrolledL <- log1p(data1$GirlsEnrolled) 
data1$EducationscoreL <- log1p(data1$Educationscore) 
data1$EnrolmentscoreL <- log1p(data1$Enrolmentscore) 
data1$GenderparityscoreL <- log1p(data1$Genderparityscore) 
data1$TotalnumberofschoolsL <- log1p(data1$Totalnumberofschools) 
data1$ElectricityL <- log1p(data1$Electricity) 
data1$DrinkingwaterL <- log1p(data1$Drinkingwater) 
data1$BoundarywallL <- log1p(data1$Boundarywall) 
data1$ToiletL <- log1p(data1$Toilet) 
data1$AreakmL <- log1p(data1$Areakm) 


stargazer(cor(data1[, c("GirlsEnrolled", 
                     "Totalnumberofschools", 
                     "Electricity", 
                     "Drinkingwater", 
                     "Boundarywall", 
                     "Toilet", 
                     "Areakm")]), 
          type = "html",  
          title="Correlation Matrix",  
          digits = 2,  
          out = '897R4correlations.html') 




df$Lstudents_free <- log1p(df$students_free) 
df$Lstudents_reduced <- log1p(df$students_reduced) 
df$Lstudents_fr <- log1p(df$students_fr) 
df$LADP_ineligible <- log1p(df$ADP_ineligible) 
df$LADP_free <- log1p(df$ADP_free) 
df$LADP_reduced <- log1p(df$ADP_reduced) 
df$LADP_fr <- log1p(df$ADP_fr) 
df$Lschool_size <- log1p(df$school_size) 
df$Lprice <- log1p(df$price) 
lm <- lm(GirlsEnrolled ~ EducationalBudgetSpendofGDP + Totalnumberofschools + Electricity + Toilet +Drinkingwater +
            Boundarywall + Areakm, data = data1)
summary(lm)

lm1 <- lm(GirlsEnrolledL ~ EducationalBudgetSpendofGDP + TotalnumberofschoolsL + ElectricityL + ToiletL +DrinkingwaterL +
           BoundarywallL + AreakmL, data = data1)
summary(lm1)

data1$CGirlsEnrolledL <- I(data1$GirlsEnrolledL - mean(data1$GirlsEnrolledL)) 
data1$CTotalnumberofschoolsL <- I(data1$TotalnumberofschoolsL - mean(data1$TotalnumberofschoolsL))
data1$CElectricityL <- I(data1$ElectricityL - mean(data1$ElectricityL))
data1$CToiletL <- I(data1$ToiletL - mean(data1$ToiletL))
data1$CDrinkingwaterL <- I(data1$DrinkingwaterL - mean(data1$DrinkingwaterL))
data1$CBoundarywallL <- I(data1$BoundarywallL - mean(data1$BoundarywallL))
data1$CAreakmL <- I(data1$AreakmL - mean(data1$AreakmL))

lm3 <- lm(CGirlsEnrolledL ~ EducationalBudgetSpendofGDP + CTotalnumberofschoolsL + CElectricityL + CToiletL + CDrinkingwaterL +
            CBoundarywallL + CAreakmL, data = data1)
summary(lm3)

hist(resid(lm3), breaks = 20) 

#GDP and number of schools

lm4<- lm(CGirlsEnrolledL ~ EducationalBudgetSpendofGDP + CTotalnumberofschoolsL + CElectricityL + CToiletL + CDrinkingwaterL +
            CBoundarywallL + CAreakmL + EducationalBudgetSpendofGDP:CTotalnumberofschoolsL, data = data1)
summary(lm4)

lm5<- lm(CGirlsEnrolledL ~ EducationalBudgetSpendofGDP + CTotalnumberofschoolsL + CElectricityL + CToiletL + CDrinkingwaterL +
           CBoundarywallL + CAreakmL + CToiletL:CDrinkingwaterL, data = data1)
summary(lm5)

lm6<- lm(CGirlsEnrolledL ~ EducationalBudgetSpendofGDP + CTotalnumberofschoolsL + CElectricityL + CToiletL + CDrinkingwaterL +
           CBoundarywallL + CAreakmL + CBoundarywallL:CAreakmL, data = data1)
summary(lm6)

lm_final_std <- lm(scale(data1$CGirlsEnrolledL) ~ scale(data1$EducationalBudgetSpendofGDP) +  
                     scale(data1$CTotalnumberofschoolsL) +  
                     scale(data1$CElectricityL) + 
                     scale(data1$CToiletL) +  
                     scale(data1$CDrinkingwaterL) + 
                     scale(data1$CBoundarywallL) +
                   scale(data1$CAreakmL) +
                     (scale(data1$CBoundarywallL):scale(data1$CAreakmL))) 
summary(lm_final_std) 

hist(resid(lm3), breaks = 20)


df2$CLstudents_fr <- I(df2$Lstudents_fr - mean(df2$Lstudents_fr)) 
df2$CLschool_size <- I(df2$Lschool_size - mean(df2$Lschool_size)) 
df2$Cprice <- I(df2$price - mean(df2$price)) 
df2$CLADP_fr <- I(df2$LADP_fr - mean(df2$LADP_fr)) 
df2$CLstudents_ineligible <- I(df2$Lstudents_ineligible - 
                                 mean(df2$Lstudents_ineligible)) 
df2$CLstudents_free <- I(df2$Lstudents_free - mean(df2$Lstudents_free)) 
df2$CLstudents_reduced <- I(df2$Lstudents_reduced - mean(df2$Lstudents_reduced)) 

hist(resid(lm1), breaks = 50) 



stargazer(lm2, 
          title = 'Base model regression',  
          align = TRUE,  
          type = 'html', 
          out = '897R4Base.html') 

ggplot(data1, aes(x = EducationalBudgetSpendofGDP, y = GirlsEnrolled ))+ 
  geom_point(color = "grey") +  
  theme_classic() +  
  geom_smooth(method = "loess", color = "red", fill = "red") + 
  geom_smooth(method=lm, color="black", size = .5, se=FALSE) 

ggplot(data1, aes(x = Totalnumberofschools, y = GirlsEnrolled))+ 
  geom_point(color = "grey") +  
  theme_classic() +  
  geom_smooth(method = "loess", color = "red", fill = "red") + 
  geom_smooth(method=lm, color="black", size = .5, se=FALSE) 

ggplot(data1, aes(x = Electricity, y = GirlsEnrolled))+ 
  geom_point(color = "grey") +  
  theme_classic() +  
  geom_smooth(method = "loess", color = "red", fill = "red") + 
  geom_smooth(method=lm, color="black", size = .5, se=FALSE) 

ggplot(data1, aes(x = Toilet, y = GirlsEnrolled))+ 
  geom_point(color = "grey") +  
  theme_classic() +  
  geom_smooth(method = "loess", color = "red", fill = "red") + 
  geom_smooth(method=lm, color="black", size = .5, se=FALSE) 

ggplot(data1, aes(x = Drinkingwater, y = GirlsEnrolled))+ 
  geom_point(color = "grey") +  
  theme_classic() +  
  geom_smooth(method = "loess", color = "red", fill = "red") + 
  geom_smooth(method=lm, color="black", size = .5, se=FALSE) 

ggplot(data1, aes(x = Boundarywall, y = GirlsEnrolled))+ 
  geom_point(color = "grey") +  
  theme_classic() +  
  geom_smooth(method = "loess", color = "red", fill = "red") + 
  geom_smooth(method=lm, color="black", size = .5, se=FALSE)

hist(resid(lm2), breaks = 20) 

plot(lm2) 








summary(data1[,c("GirlsEnrolled",
         "Educationscore", 
         "EducationalBudgetSpendofGDP", 
         "Enrolmentscore", 
         "Genderparityscore",
         "Learningscore",
         "Numberofprimaryschools",
         "Numberofsecondaryschools",
         "Schoolinfrastructurescore",
         "Totalnumberofschools",
         "Electricity",
         "Drinkingwater",
         "Boundarywall",
         "Toilet",
         "Areakm")])

sd(data1$GirlsEnrolled)
sd(data1$Educationscore)
sd(data1$EducationalBudgetSpendofGDP)
sd(data1$Enrolmentscore)
sd(data1$Genderparityscore)
sd(data1$Learningscore)
sd(data1$Numberofprimaryschools)
sd(data1$Numberofsecondaryschools)
sd(data1$Schoolinfrastructurescore)
sd(data1$Totalnumberofschools)
sd(data1$Electricity)
sd(data1$Drinkingwater)
sd(data1$Boundarywall)
sd(data1$Areakm)

#Assumptions

lm1 <- lm(Educationscore ~ EducationalBudgetSpendofGDP + Totalnumberofschools + Electricity + Toilet +Drinkingwater +
Boundarywall + Areakm, data = data1)
summary(lm1)

lm2 <- lm(GirlsEnrolled ~ EducationalBudgetSpendofGDP + Totalnumberofschools + Electricity + Toilet +Drinkingwater +
            Boundarywall + Areakm, data = data1)
summary(lm2)

lm2 <- lm(GirlsEnrolled ~ Genderparityscore + Learningscore + Schoolinfrastructurescore +Drinkingwater +
                   Boundarywall + Areakm, data = data1)






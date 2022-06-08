
### Import Datasets ###

Educaciขn <- read_dta("~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Data/DANE info/2020/Educacion/Educaciขn.dta")
Caracterกsticas_y_composiciขn_del_hogar <- read_dta("~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Data/DANE info/2020/Caracteristicas y composicion del hogar/Caracterกsticas y composiciขn del hogar.dta")
Fuerza_de_trabajo <- read_dta("~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Data/DANE info/2020/Fuerza de trabajo/Fuerza de trabajo.dta")
Tecnologกas_de_informaciขn_y_comunicaciขn <- read_dta("~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Data/DANE info/2020/Tecnologias de informacion y comunicacion/Tecnologกas de informaciขn y comunicaciขn.dta")


###  Extract variables ###


Personal <- Caracterกsticas_y_composiciขn_del_hogar[, c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN",
                                                        "P6040", "P6076S1", "P6076S2", "P767", "P6096", "P6020")]
Education <- Educaciขn[, c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN", "P8587")]
Employment <- Fuerza_de_trabajo[, c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN",
                                    "P6240", "P6280", "P6435", "P8624", "P6595S1", "P6605S1", "P6623S1", "P6615S1",
                                    "P8626S1", "P8628S1", "P1087S4A1", "P6750", "P550")]
Technologies <- Tecnologกas_de_informaciขn_y_comunicaciขn[, c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN", 
                                                              "P1910", "P1911", "P1912", "P1084", "P1085S1", "P1085S2", "P1085S3", "P1085S4", "P1085S5", "P1085S6", "P1085S7",
                                                              "P1083S1", "P1083S2", "P1083S3", "P1083S4", "P1083S5", "P1083S6", "P1083S7", "P1083S8", "P1083S9", "P1083S10", "P1083S12", "P1083S13", "P1083S14", "P1083S15", "P1083S11")]



###  Rename Variables ###


#Personal
names(Personal)[1]<-paste("directory")
names(Personal)[2]<-paste("sequence")
names(Personal)[3]<-paste("sequence1")
names(Personal)[4]<-paste("order")
names(Personal)[5]<-paste("age")
names(Personal)[6]<-paste("past_state")
names(Personal)[7]<-paste("past_municipality")
names(Personal)[8]<-paste("yearshere")
names(Personal)[9]<-paste("movereason")
names(Personal)[10]<-paste("gender")

#Education
names(Education)[1]<-paste("directory")
names(Education)[2]<-paste("sequence")
names(Education)[3]<-paste("sequence1")
names(Education)[4]<-paste("order")
names(Education)[5]<-paste("edulevel")

# Employment
names(Employment)[1]<-paste("directory")
names(Employment)[2]<-paste("sequence")
names(Employment)[3]<-paste("sequence1")
names(Employment)[4]<-paste("order")
names(Employment)[5]<-paste("labor")
names(Employment)[6]<-paste("unemployed")
names(Employment)[7]<-paste("worktype")
names(Employment)[8]<-paste("baseincome")
names(Employment)[9]<-paste("foodassistance")
names(Employment)[10]<-paste("livingassistance")
names(Employment)[11]<-paste("otherincome")
names(Employment)[12]<-paste("transassistance")
names(Employment)[15]<-paste("bonus")
names(Employment)[16]<-paste("totalincome")
names(Employment)[17]<-paste("totalincomefarm")


# Technology 
names(Technologies)[1]<-paste("directory")
names(Technologies)[2]<-paste("sequence")
names(Technologies)[3]<-paste("sequence1")
names(Technologies)[4]<-paste("order")
names(Technologies)[5]<-paste("desktop")
names(Technologies)[6]<-paste("laptop")
names(Technologies)[7]<-paste("tablet")
names(Technologies)[8]<-paste("internetusage")
names(Technologies)[9]<-paste("I5home")
names(Technologies)[10]<-paste("I5work")
names(Technologies)[11]<-paste("I5learningcenter")
names(Technologies)[12]<-paste("I5publicaccess")
names(Technologies)[13]<-paste("I5cafeinternet")
names(Technologies)[14]<-paste("I5friendhouse")
names(Technologies)[15]<-paste("I5other")
names(Technologies)[16]<-paste("I6searchinfo")
names(Technologies)[17]<-paste("I6email")
names(Technologies)[18]<-paste("I6socialnetworks")
names(Technologies)[19]<-paste("I6buyandorder")
names(Technologies)[20]<-paste("I6ebank")
names(Technologies)[21]<-paste("I6learning")
names(Technologies)[22]<-paste("I6admin")
names(Technologies)[23]<-paste("I6downloads")
names(Technologies)[24]<-paste("I6media")
names(Technologies)[25]<-paste("I6entertainment")
names(Technologies)[26]<-paste("I6lookforwork")
names(Technologies)[27]<-paste("I6storeinfo")
names(Technologies)[28]<-paste("I6sell")
names(Technologies)[29]<-paste("I6work")
names(Technologies)[30]<-paste("I6other")

rm(Caracterกsticas_y_composiciขn_del_hogar, Educaciขn, Fuerza_de_trabajo, Tecnologกas_de_informaciขn_y_comunicaciขn)



### Merge existing datasets ###

data1 <- merge(Employment, Education, by=c("directory", "sequence", "sequence1", "order"), all = TRUE)
data2 <- merge(data1, Personal, by=c("directory", "sequence", "sequence1", "order"), all = TRUE)
mergedata <- merge(data2, Technologies, by=c("directory", "sequence", "sequence1", "order"), all = TRUE)
rm(data1, data2)



### Focusing on cohort of interest ###

# 1. forced migrants

Main1 <- mergedata[!is.na(mergedata$movereason),]
Main1$forced <- ifelse(Main1$movereason==2 | Main1$movereason==3, 1, 0)
main <- subset(Main1, forced!=0)
rm(mergedata, Main1)


# 2. labor force participants

main <- subset(main, labor==1 | labor==2 | unemployed==1)

# We are looking at those who were employed last week (labor==1), those unemployed 
# but looking for a job in the past week (labor==2) and those who were unemployed, 
# but looking for a job in the past month

# 3. focus on employees 

Main <- subset(main, worktype==1 | worktype==2 | worktype==3 | worktype==4 |
                 worktype==5)
rm(main)


# 4. income of workers  

Main[is.na(Main)]=0
Main$income <- Main$baseincome+Main$foodassistance+Main$livingassistance+Main$otherincome+
  Main$transassistance+Main$bonus+Main$totalincome



# 5. focus on the type of internet usage 

Main <- subset(Main, I6searchinfo==1 | I6email==1 | I6ebank==1 | I6learning==1 |
                 I6admin==1 | I6media==1 | I6lookforwork==1 | I6storeinfo==1 | I6sell==1 |
                 I6work==1)



### Cleaning up data ###


Mai$labor1 <- ifelse(Main$labor ==1, 1, 0)
Main$labor2 <- ifelse(Main$labor ==2 | Main$labor==2 & Main$unemployed==1, 1, 0)

# unemployed but part of the labor force 
Main$unemployed[Main$unemployed == 2] <- 0 

# internet usage 
Main$daily <- ifelse(Main$internetusage == 1, 1, 0)
Main$weekly <- ifelse(Main$internetusage == 1 |
                        Main$internetusage ==2, 1, 0)

# change 2's to 0's
Main$gender[Main$gender == 2] <-0

# Electronics use daily and at least once a week 
Main$ictuse <- ifelse(Main$desktop==1 | Main$laptop==1 | Main$tablet==1, 1, 0)
Main$ictuse1 <- ifelse(Main$desktop==1 | Main$laptop==1 | Main$tablet==1 | 
                         Main$desktop==2 | Main$laptop==2 | Main$tablet==2, 1, 0)

# Changing income of 0 to income of 1 
Main$income[Main$income==0]<- 1



### Climate migrants ###

climate <- subset(Main, movereason!=3)


### Descriptive statistics of dataset ###


table(Main$daily)
table(Main$weekly)
table(Main$daily, Main$labor1)
aggregate( income ~ daily, Main, mean)
table(Main$ictuse)
table(Main$ictuse, Main$labor1)

### Regressions ###


install.packages("stargazer")
library(stargazer)

model1 <- lm(log(income)~daily, data = Main)
summary(model1)
model2 <- lm(log(income)~daily+gender+age+edulevel+yearshere, data = Main)
summary(model2)

stargazer(model1, model2, 
          type = "html", 
          out = "~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Meeting Preps /Week - 05.30.2022/regression.htm")

stargazer(model1, model2, 
          title = "Daily Internet Usage Affects on Income for Forced Migrants", 
          dep.var.caption = "DV: Employee Income", 
          covariate.labels = c("Daily Internet Usage", "Male", "Age", "Level of Education", 
                               "Years in Current Municipality"), 
          notes.label = "Significance Levels", 
          type = "html", 
          out = "~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Meeting Preps /Week - 05.30.2022/regression.htm")


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

model1 <- glm(labor1~daily+gender+age+edulevel+yearshere, family = binomial(link = 'logit'), data = Main)
summary(model1)
coef(model1)
logit2prob(coef(model1))
(intercept <- coef(model1)[1])
(daily <- coef(model1)[2])
(male <- coef(model1)[3])
(age <- coef(model1)[4])
(edulevel <- coef(model1)[5])
logits_daily <- intercept + 1*daily
logits_male <- intercept + 1*male
logits_age40 <- intercept + 40*age 
logits_edulevel13 <- intercept + 13*edulevel
model3 <- logit2prob(logits_daily)
logit2prob(logits_male)
rm(age, edulevel, gender, weekly, intercept, logits_age20, logits_age40, logits_age60, 
   logits_edulevel11, logits_edulevel13, logits_edulevel7, logits_edulevel9, 
   logits_male, logits_daily, daily, male)

stargazer(model1, model3, 
          title = "Daily Internet Usage Affects on Employment Probability for Forced Migrants", 
          dep.var.caption = "DV: Log Odds - Employment", 
          covariate.labels = c("Daily Internet Usage", "Male", "Age", "Level of Education", 
                               "Years in Current Municipality"), 
          notes.label = "Significance Levels", 
          type = "html", 
          out = "~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Meeting Preps /Week - 05.30.2022/prob.regression.htm")

stargazer(model2, 
          title = "Log Odds to Probability", 
          dep.var.caption = "DV: Log Odds - Employment", 
          covariate.labels = c("Intercept", "Daily Internet Usage", "Male", "Age", "Level of Education", 
                               "Years in Current Municipality"), 
          notes.label = "Significance Levels", 
          type = "html", 
          out = "~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Meeting Preps /Week - 05.30.2022/prob.regression1.htm")
rm(model1, model2, model3, Main, main)



########################################################################################################



### Not controlling for employment or how internet is used ###

### Merge existing datasets ###

data1 <- merge(Employment, Education, by=c("directory", "sequence", "sequence1", "order"), all = TRUE)
data2 <- merge(data1, Personal, by=c("directory", "sequence", "sequence1", "order"), all = TRUE)
mergedata <- merge(data2, Technologies, by=c("directory", "sequence", "sequence1", "order"), all = TRUE)
rm(data1, data2)
rm(Education, Employment, Personal, Technologies)



### Focusing on cohort of interest ###

# 1. forced migrants

Main1 <- mergedata[!is.na(mergedata$movereason),]
Main1$forced <- ifelse(Main1$movereason==2 | Main1$movereason==3, 1, 0)
main <- subset(Main1, forced!=0)
rm(mergedata, Main1)


# 2. labor force participants

Main <- subset(main, labor==1 | labor==2 | unemployed==1)
rm(main)

# We are looking at those who were employed last week (labor==1), those unemployed 
# but looking for a job in the past week (labor==2) and those who were unemployed, 
# but looking for a job in the past month


# 4. income of workers  

Main[is.na(Main)]=0
Main$income <- Main$baseincome+Main$foodassistance+Main$livingassistance+Main$otherincome+
  Main$transassistance+Main$bonus+Main$totalincome+Main$totalincomefarm

Main$income[Main$income ==0]<- 1



### Cleaning up data ###

Main$labor1 <- ifelse(Main$labor ==1, 1, 0)

# unemployed but part of the labor force 
Main$unemployed[Main$unemployed == 2] <- 0 

# internet usage 
Main$daily <- ifelse(Main$internetusage == 1, 1, 0)
Main$weekly <- ifelse(Main$internetusage == 1 |
                        Main$internetusage ==2, 1, 0)

# change 2's to 0's
Main$gender[Main$gender == 2] <-0

# Electronics use daily and at least once a week 
Main$ictuse <- ifelse(Main$desktop==1 | Main$laptop==1 | Main$tablet==1, 1, 0)
Main$ictuse1 <- ifelse(Main$desktop==1 | Main$laptop==1 | Main$tablet==1 | 
                         Main$desktop==2 | Main$laptop==2 | Main$tablet==2, 1, 0)

# Changing income of 0 to income of 1 
Main$income[Main$income==0]<- 1



### Climate migrants ###

climate <- subset(Main, movereason!=3)
rm(climate)


### Descriptive statistics of dataset ###


table(Main$daily)
table(Main$weekly)
table(Main$daily, Main$labor1)
table(Main$weekly, Main$labor1)
aggregate(income ~ daily, Main, mean)
aggregate(income ~ weekly, Main, mean)
table(Main$ictuse)
table(Main$ictuse, Main$labor1)

cor(Main$income, Main$daily, method = c("pearson"))
cor.test(Main$income, Main$daily, method = c("pearson"))



### Regressions ###

model1 <- lm(log(income)~daily, data = Main)
summary(model1)
model2 <- lm(log(income)~daily+gender+age+edulevel+yearshere, data = Main)
summary(model2)
model3 <- lm(log(income)~weekly, data = Main)
summary(model3)
model4 <- lm(log(income)~weekly+gender+age+edulevel+yearshere, data = Main)
summary(model4)


stargazer(model1, model2, 
          title = "Daily Internet Usage Affects on Income for Forced Migrants", 
          dep.var.caption = "Employee Income", 
          covariate.labels = c("Daily Internet Usage", "Male", "Age", "Level of Education", 
                               "Years in Current Municipality"), 
          notes.label = "Significance Levels", 
          type = "html", 
          out = "~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Meeting Preps /Week - 05.30.2022/dailyregression.htm")

stargazer(model3, model4, 
          title = "Weekly Internet Usage Affects on Income for Forced Migrants", 
          dep.var.caption = "Employee Income", 
          covariate.labels = c("Weekly Internet Usage", "Male", "Age", "Level of Education", 
                               "Years in Current Municipality"), 
          notes.label = "Significance Levels", 
          type = "html", 
          out = "~/Documents/Education/Ecole Polytechnique/Year 1 /Third Trimester (Internship) /Meeting Preps /Week - 05.30.2022/weeklyregression.htm")




################################################################################################



### Looking at all the municipalities in the dataset ###

# number of forced migrants per municipality 
Municipalities <- table(Municipality = Main$past_municipality, State = Main$past_state)
Municipalities <- as.data.frame.matrix(Municipalities) 
Municipalities$migrants <- Municipalities$`11`+Municipalities$`13`+Municipalities$`15`+Municipalities$`17`+
  Municipalities$`18`+Municipalities$`19`+Municipalities$`20`+Municipalities$`23`+Municipalities$`25`+
  Municipalities$`27`+Municipalities$`41`+Municipalities$`44`+Municipalities$`47`+Municipalities$`5`+
  Municipalities$`50`+Municipalities$`52`+Municipalities$`54`+Municipalities$`63`+Municipalities$`66`+
  Municipalities$`68`+Municipalities$`70`+Municipalities$`73`+Municipalities$`76`+Municipalities$`8`+
  Municipalities$`81`+Municipalities$`85`+Municipalities$`86`+Municipalities$`91`+Municipalities$`94`+
  Municipalities$`95`+Municipalities$`97`+Municipalities$`99`
Municipalities <- cbind(rownames(Municipalities), Municipalities)
rownames(Municipalities) <- NULL
names(Municipalities)[1] <- "municipality"
names(Municipalities)[34]<-"migrants"
migrants_per_municipality <- data.frame(Municipalities$municipality, Municipalities$migrants)
rm(Municipalities)
names(migrants_per_municipality)[1]<-"municipality"
names(migrants_per_municipality)[2]<-"migrants"

# Municipalities not of 5 and 6th strata (used to remove from "migrants_per_municiplity" dataframe)
rmunicipios <- subset(CT01_Categorizacion_2022, Categoría==1 | Categoría==2 | Categoría==3 | Categoría==4 | Categoría=="ESP")
rmunicipios$`Código CGN` <- sub('....', '', rmunicipios$`Código CGN`)

#remove those municipalities from dataset 
table(rmunicipios$`Código CGN`)
migrants_per_municipality$delete <- ifelse(migrants_per_municipality$municipality==5001 | migrants_per_municipality$municipality==5045 | migrants_per_municipality$municipality==5079 |
                                             migrants_per_municipality$municipality==5088 | migrants_per_municipality$municipality==5129 | migrants_per_municipality$municipality==5148 |
                                             migrants_per_municipality$municipality==5212 | migrants_per_municipality$municipality==5266 | migrants_per_municipality$municipality==5308 |
                                             migrants_per_municipality$municipality==5318 | migrants_per_municipality$municipality==5360 | migrants_per_municipality$municipality==5376 |
                                             migrants_per_municipality$municipality==5380 | migrants_per_municipality$municipality==5440 | migrants_per_municipality$municipality==5604 |
                                             migrants_per_municipality$municipality==5607 | migrants_per_municipality$municipality==5615 | migrants_per_municipality$municipality==5631 |
                                             migrants_per_municipality$municipality==5736 | migrants_per_municipality$municipality==5837 | migrants_per_municipality$municipality==8001 |
                                             migrants_per_municipality$municipality==8433 | migrants_per_municipality$municipality==8573 | migrants_per_municipality$municipality==8758 |
                                             migrants_per_municipality$municipality==11001 | migrants_per_municipality$municipality==13001 | migrants_per_municipality$municipality==13836 |
                                             migrants_per_municipality$municipality==15001 | migrants_per_municipality$municipality==15238 | migrants_per_municipality$municipality==15759 |
                                             migrants_per_municipality$municipality==17001 | migrants_per_municipality$municipality==18001 | migrants_per_municipality$municipality==19001 |
                                             migrants_per_municipality$municipality==20001 | migrants_per_municipality$municipality==20011 | migrants_per_municipality$municipality==23001 |
                                             migrants_per_municipality$municipality==25126 | migrants_per_municipality$municipality==25175 | migrants_per_municipality$municipality==25214 |
                                             migrants_per_municipality$municipality==25269 | migrants_per_municipality$municipality==25286 | migrants_per_municipality$municipality==25290 |
                                             migrants_per_municipality$municipality==25307 | migrants_per_municipality$municipality==25430 | migrants_per_municipality$municipality==25473 |
                                             migrants_per_municipality$municipality==25612 | migrants_per_municipality$municipality==25754 | migrants_per_municipality$municipality==25758 |
                                             migrants_per_municipality$municipality==25799 | migrants_per_municipality$municipality==25817 | migrants_per_municipality$municipality==25899 |
                                             migrants_per_municipality$municipality==27001 | migrants_per_municipality$municipality==41001 | migrants_per_municipality$municipality==41551 |
                                             migrants_per_municipality$municipality==44001 | migrants_per_municipality$municipality==44430 | migrants_per_municipality$municipality==44560 |
                                             migrants_per_municipality$municipality==44847 | migrants_per_municipality$municipality==47001 | migrants_per_municipality$municipality==50001 |
                                             migrants_per_municipality$municipality==50006 | migrants_per_municipality$municipality==50568 | migrants_per_municipality$municipality==50573 |
                                             migrants_per_municipality$municipality==52001 | migrants_per_municipality$municipality==52356 | migrants_per_municipality$municipality==52835 |
                                             migrants_per_municipality$municipality==54001 | migrants_per_municipality$municipality==54405 | migrants_per_municipality$municipality==54874 |
                                             migrants_per_municipality$municipality==63001 | migrants_per_municipality$municipality==66001 | migrants_per_municipality$municipality==66170 |
                                             migrants_per_municipality$municipality==68001 | migrants_per_municipality$municipality==68081 | migrants_per_municipality$municipality==68276 |
                                             migrants_per_municipality$municipality==68307 | migrants_per_municipality$municipality==68547 | migrants_per_municipality$municipality==70001 |
                                             migrants_per_municipality$municipality==73001 | migrants_per_municipality$municipality==73268 | migrants_per_municipality$municipality==76001 |
                                             migrants_per_municipality$municipality==76109 | migrants_per_municipality$municipality==76111 | migrants_per_municipality$municipality==76130 |
                                             migrants_per_municipality$municipality==76147 | migrants_per_municipality$municipality==76364 | migrants_per_municipality$municipality==76520 |
                                             migrants_per_municipality$municipality==76834 | migrants_per_municipality$municipality==76892 | migrants_per_municipality$municipality==81001 |
                                             migrants_per_municipality$municipality==85001 | migrants_per_municipality$municipality==99773, 1, 0)
migrants_per_municipality <- subset(migrants_per_municipality ,delete!=1)
rm(migrants_per_municipality$delete)






library(haven) # to read SAV files
library(tidyverse) # to do things easily
library(lavaan) #  for cfa
library(psych)  # for alpha to omega
library(apaTables)

## CFA anlayses are long so increase maximum number of lines to print
options(max.print = 99999)
options(width=300)

## create a set of FIT indices to report easily
fit.scaled<-c("chisq.scaled", "df", "cfi.robust", "tli.robust", "aic", "bic", "srmr", "srmr_bentler", "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust")

# read in the two data files
allprolificapril19 <- read_csv("allprolificapril19.csv") # general public

raw_students <- read_csv("raw students.csv") # undergraduate psychology pool

# rename the student variables and remove ID key
students <- raw_students %>% 
  rename("HC1"="Q11_1", "HC2"="Q11_2","HC3"="Q11_3","HC4"="Q11_4","HC5"="Q11_5","HC6"="Q11_6","HC7"="Q11_7","HC8"="Q11_8","HC9"="Q11_9","HC10"="Q11_10",
         "HC11"="Q11_11", "HC12"="Q11_12","HC13"="Q11_13","HC14"="Q11_14","HC15"="Q11_15","HC16"="Q11_16","HC17"="Q11_17","HC18"="Q11_18","HC19"="Q11_19","HC20"="Q11_20",
         "HC21"="Q11_21", "HC22"="Q11_22","HC23"="Q11_23","HC24"="Q11_24","HC25"="Q11_25","HC26"="Q11_26","HC27"="Q11_27","HC28"="Q11_28","HC29"="Q11_29","HC30"="Q11_30",
         "HC31"="Q11_31", "HC32"="Q11_32","HC33"="Q11_33","HC34"="Q11_34","HC35"="Q11_35","HC36"="Q11_36","HC37"="Q11_37","HC38"="Q11_38","HC39"="Q11_39","HC40"="Q11_40",
         "HC41"="Q11_41", "HC42"="Q11_42","HC43"="Q11_43","HC44"="Q11_44","HC45"="Q11_45","HC46"="Q11_46") %>% 
  select(-sona)

# retain the same variables from the public
public <- allprolificapril19 %>% select(age, HC1:HC46)

# combine the two files, and drop one P with missing data
attitudes <-rbind(students,public) %>% filter(!is.na(HC1))

# stats on the variables
describe(attitudes)
apa.cor.table(attitudes, filename='corrtable.docx', table.number=1, show.conf.interval = TRUE, landscape = TRUE)


items46.list <- list(compassion = c("HC18", "HC4", "HC25","HC30", "HC37", "HC23","HC17","HC2", "HC14", "HC15"),
                  denial=c("HC42", "HC1", "HC44", "HC3", "HC31", 
                              "HC10", "HC5", "HC11", "HC28", "HC20", "HC8", "HC6", "HC43", "HC21", 
                              "HC33", "HC16", "HC45", "HC40", "HC46", "HC34",
                              "HC7", "HC12", "HC32", "HC39"),
                   gravity =c("HC9","HC13", "HC38", "HC35", "HC29", "HC22", "HC26", "HC36", "HC19", "HC27", "HC41", "HC24"))


scores46 <- scoreItems(items46.list,attitudes)
summary(scores46)
describe(scores46$scores)
scores46$G6
scores46$item.corrected
# unifactorial solution for original Cabeldue items
# not used HC7 HC12 HC22 HC26 HC32 HC39

model.40I.1Factor <- "factor =~ HC1 + HC2 + HC3 + HC4 + HC5 + HC6 +       HC8 + HC9 + HC10 +
                            HC11 +        HC13 + HC14 + HC15 + HC16 + HC17 + HC18 + HC19 + HC20 +
                            HC21 +        HC23 + HC24 + HC25 +       HC27 + HC28 + HC29 + HC30 +
                            HC31 +       HC33 + HC34 + HC35 + HC36 + HC37 + HC38 +        HC40 +
                            HC41 + HC42 + HC43 + HC44 + HC45 + HC46
   HC29 ~~ HC38
   HC4 ~~ HC18
   HC19 ~~ HC36
    HC9 ~~ HC24
 HC5 ~~ HC11
HC30 ~~ HC37
HC13 ~~ HC38
HC13 ~~ HC29
HC35 ~~ HC41
HC18 ~~ HC19
HC15 ~~ HC25
HC1 ~~  HC6
 HC42 ~~ HC43
HC31 ~~ HC46"
fit <- cfa(model.40I.1Factor, data = attitudes, estimator = 'MLR')
#summary(fit, modindices=TRUE, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit, fit.scaled)
modindices(fit) %>% filter(op == "~~" & mi>30) %>% arrange(desc(mi))

# chisq.scaled                    df            cfi.robust            tli.robust                   aic 
# 2047.213               726.000                 0.773                 0.756             40024.196 
# bic                  srmr          srmr_bentler          rmsea.robust rmsea.ci.lower.robust 
# 40407.710                 0.090                 0.090                 0.070                 0.066 
# rmsea.ci.upper.robust 
# 0.073 


# original papers model using only Cabeldue items
# not used HC7 HC12 HC22 HC26 HC32 HC39
model.40I.Original <- "nb =~ HC1 + HC2  + HC3 +  HC5+ HC6+   HC8   + 
                        HC10   + HC11 + HC14 + HC16 + HC19 +
                        HC20   + HC21 + HC23 + HC28 +
                        HC30 + HC31 + HC33  + HC34 + HC36+ HC37 +
                        HC40 + HC42 + HC43+ HC44+ HC45+ HC46
                   d =~ HC4 + HC18 + HC25 
                   op =~ HC13 + HC15+ HC29 + HC35 + HC38
                   vh =~ HC9 + HC17+ HC24 + HC27+ HC41
HC19 ~~ HC36
HC5 ~~ HC11
HC30 ~~ HC37
 HC1 ~~  HC6
 HC42 ~~ HC43
"


# 
#        chisq.scaled                    df            cfi.robust            tli.robust                   aic                   bic                  srmr 
#           1754.813               730.000                 0.824                 0.812             39673.633             40040.827                 0.082 
#        srmr_bentler          rmsea.robust rmsea.ci.lower.robust rmsea.ci.upper.robust 
#             0.082                 0.061                 0.058                 0.065 
# 
fit <- cfa(model.40I.Original, data = attitudes, estimator = 'MLR')
#summary(fit, modindices=TRUE, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit, fit.scaled)
modindices(fit) %>% filter(op == "~~" & mi>20) %>% arrange(desc(mi))
omegaFromSem(fit,m=NULL,flip=TRUE,plot=TRUE)


# new model with 40 items
#  not used HC7 HC12 HC22 HC26 HC32 HC39

model.40I.Newmodel <- "denial =~  HC1   + HC3  + HC5  + HC6 + HC8 + 
                                  HC10  + HC11  + HC16 + 
                                  HC20  + HC21 + HC28+ 
                                  HC31 + HC33 + HC34 + 
                                  HC40 + HC42+ HC43+ HC44+ HC45 + HC46 
                       gravity =~ HC9 +HC13+ HC19 + HC24+ HC27+ HC29  + HC35  + HC36 + HC38 + HC41
                   compassion =~  HC2+ HC4 +HC14+ HC15 +HC17+ HC18 + HC23+ HC25 +HC30 + HC37  
HC5 ~~ HC11
HC18 ~~  HC4
HC9 ~~ HC24
HC44 ~~  HC6
 HC4 ~~ HC25 
HC18 ~~ HC25 
 HC31 ~~ HC46
HC42 ~~ HC43
 HC3 ~~  HC8 
HC44 ~~ HC11
 HC1 ~~  HC6
 "

fit <- cfa(model.40I.Newmodel, data = attitudes, estimator = 'MLR')
#summary(fit, modindices=TRUE, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit, fit.scaled)
modindices(fit) %>% filter(op == "~~" & mi>10) %>% arrange(desc(mi))

# chisq.scaled                    df            cfi.robust            tli.robust                   aic 
# 1216.750               726.000                 0.916                 0.909             39051.749 
# bic                  srmr          srmr_bentler          rmsea.robust rmsea.ci.lower.robust 
# 39435.263                 0.053                 0.053                 0.042                 0.038 
# rmsea.ci.upper.robust 
# 0.047 


# new model with 46 items
model.46I.Newmodel <- "denial =~  HC1  + HC3 + HC5  + HC6 + HC7 + HC8  + 
                                  HC10  + HC11+ HC12 + HC16  + 
                                  HC20 + HC21 + HC28 + 
                                  HC31+ HC32 + HC33 + HC34  + HC39 +
                                  HC40  +HC42 + HC43 + HC44 + HC45+  HC46
                       gravity =~ HC9    +HC13 + HC19  + HC22 + HC24+ HC27+ HC26 + HC29 + HC35+ HC36 + HC38 + HC41
                    compassion =~ HC2+ HC4+HC14+ HC15 +HC17 +  HC18 + HC23 + HC25 +HC30 + HC37 
HC8 ~~  HC7
HC5 ~~ HC11
HC22 ~~ HC26
HC18 ~~  HC4
 HC9 ~~ HC24
HC4 ~~ HC25
HC18 ~~ HC25
HC1 ~~  HC6
 HC31 ~~ HC46"




fit <- cfa(model.46I.Newmodel, data = attitudes, estimator = 'MLR')
#summary(fit, modindices=TRUE, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit, fit.scaled)
modindices(fit) %>% filter(op == "~~" & mi>20) %>% arrange(desc(mi))

# chisq.scaled                    df            cfi.robust            tli.robust                   aic 
# 1892.084               977.000                 0.886                 0.879             43867.066 
# bic                  srmr          srmr_bentler          rmsea.robust rmsea.ci.lower.robust 
# 44291.379                 0.053                 0.053                 0.050                 0.047 
# rmsea.ci.upper.robust 
# 0.054 

omegaFromSem(fit,m=NULL,flip=TRUE,plot=TRUE)





#  compute alphas for the subscales based on 46 items
compassion <- attitudes %>% select(HC18, HC4, HC25,HC30, HC37, HC23,HC17,HC2, HC14, HC15)
denial<- attitudes %>% select(HC42, HC1, HC44, HC3, HC31, 
                              HC10, HC5, HC11, HC28, HC20, HC8, HC6, HC43, HC21, 
                              HC33, HC16, HC45, HC40, HC46, HC34,
                              HC7, HC12, HC32, HC39)
gravity <- attitudes %>% select(HC9,HC13, HC38, HC35, HC29, HC22, HC26, HC36, HC19, HC27, HC41, HC24)



corr.test(compassion)
alpha(compassion)

corr.test(gravity)
alpha(gravity)

corr.test(denial)
alpha(denial)

#  compute alphas for the subscales based on 20 items


denial <- attitudes %>% select(HC40, HC7, HC8, HC6, HC39, HC32, HC20, HC11, HC43, HC44)
compassion <- attitudes %>% select(HC18, HC23, HC30, HC25, HC37)
gravity <- attitudes %>% select(HC22, HC26, HC29, HC36, HC38)

alpha(compassion)
alpha(gravity)
alpha(denial)

items20.list <- list(denial =c("HC40", "HC7", "HC8", "HC6", "HC39", "HC32", "HC20", "HC11", "HC43", "HC44"),
                     compassion =c("HC18", "HC23", "HC30", "HC25", "HC37"),
                     gravity =c("HC22", "HC26", "HC29", "HC36", "HC38"))

scores20 <- scoreItems(items20.list,attitudes)
summary(scores20)
describe(scores20$scores)
scores20$G6
scores20$item.corrected

# new model with 20 items
model.20I.Newmodel <- "denial =~ HC6 +  HC7 + HC8 + HC11  + HC20 + HC32 + HC39+ HC40 +HC43 + HC44
compassion =~ HC18 + HC23 + HC25 + HC30 + HC37
gravity =~  HC22 + HC26 + HC29 + HC36 + HC38
 HC7 ~~  HC8
 HC18 ~~ HC25 
HC36 ~~ HC38 
 HC44 ~~ HC39
HC32 ~~ HC36
HC7 ~~ HC39
HC6 ~~ HC11"
#HC40 ~~ HC32

# cchisq.scaled                    df            cfi.robust            tli.robust                   aic                   bic                  srmr 
#       301.390               160.000                 0.962                 0.955             17995.278             18199.274                 0.055 
#  srmr_bentler          rmsea.robust rmsea.ci.lower.robust rmsea.ci.upper.robust 
#         0.055                 0.052                 0.043                 0.061 

fit <- cfa(model.20I.Newmodel, data = attitudes, estimator = 'MLR')
#summary(fit, modindices=TRUE, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit, fit.scaled)
modindices(fit) %>% filter(op == "~~" & mi>10) %>% arrange(desc(mi))


################
################  STUDY 2
################  
################  

study2 <- read.csv("study2.csv") # validity data
study3 <- read.csv("study 2 extra.csv")

# add an index column to Study3
study3 <- study3 %>% mutate(index = rownames(study3))
# remove the second response from people who did it twice
study3 <- study3 %>% filter(!index==40) # prolific=="5af49a481b55800001f495bb"
study3 <- study3 %>% filter(!index==61) # prolific=="5cd726aab1fa62001a8de584"  

study2all <- merge(study2,study3,by="prolific", all=TRUE)
write.csv(study2all,"Study2All.csv")

study2Complete <- study2all %>% filter(!is.na(HC1) & !is.na(X01))
write.csv(study2Complete,"Study2Complete.csv")

# this data has renumbered HC-UK items, RWA, SDO and BF
# 

items20.list <- list(compassion = c("HC12", "HC15", "HC5", "HC8", "HC9"),
                     denial=c( "HC13", "HC3", "HC6", "X01", "X02", "X03", "X04", "X05", "X06", "X10"),
                     gravity =c("HC7", "HC11", "X07", "X08", "X09"))


scores20 <- scoreItems(items20.list,study2all, missing=FALSE)
summary(scores20)
describe(scores20$scores)
scores20$G6
scores20$item.corrected

itemsAll <-  list(compassion = c("HC12", "HC15", "HC5", "HC8", "HC9"),
                  denial=c( "HC13", "HC3", "HC6", "X01", "X02", "X03", "X04", "X05", "X06", "X10"),
                  gravity =c("HC7", "HC11", "X07", "X08", "X09") ,
                  rwa = c("-RWA1","-RWA2", "RWA3", "RWA4", "RWA5", "-RWA6", "RWA7",  "RWA8", "-RWA9", "RWA10","-RWA11","RWA12" ),
                  sdo = c("SDO1", "-SDO2","SDO3", "SDO4", "-SDO5", "SDO6", "-SDO7", "-SDO8", "-SDO9",
                          "-SDO10","SDO11","SDO12","-SDO13","SDO14","-SDO15","SDO16"   ),
                  
                  Extraversion=c( "BFI1", "-BFI6", "BFI11", "BFI16", "-BFI21", "BFI26", "-BFI31", "BFI36" ),
                  Agreeableness=c( "-BFI2", "BFI7", "-BFI12", "BFI17", "BFI22", "-BFI27", "BFI32", "-BFI37", "BFI42" ),
                  Conscientiousness=c( "BFI3", "-BFI8", "BFI13", "-BFI18", "-BFI23", "BFI28", "BFI33", "BFI38", "-BFI43"),
                  Neuroticism=c( "BFI4", "-BFI9", "BFI14", "BFI19", "-BFI24", "BFI29", "-BFI34", "BFI39"),
                  Openness=c( "BFI5", "BFI10", "BFI15", "BFI20", "BFI25", "BFI30", "-BFI35", "BFI40", "-BFI41", "BFI44")
                  
                    )
scoresAll <- scoreItems(itemsAll,study2all, missing=FALSE)
summary(scoresAll)
describe(scoresAll$scores)

# put the scale scores into a new frame
study2.scores <- data.frame(scoresAll$scores)

#### mr to assess corr of rwa andf sdo above big5

#denial - big 5
denial.b5 <- lm(data=study2.scores, denial ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism)
summary(denial.b5)
# add in rwa
denial.rwa <- lm(data=study2.scores, denial ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism +rwa)
summary(denial.rwa)
summary(denial.rwa)$r.squared-summary(denial.b5)$r.squared
anova(denial.b5, denial.rwa)
# add in sdo instead
denial.sdo <- lm(data=study2.scores, denial ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism +sdo)
summary(denial.sdo)
summary(denial.sdo)$r.squared-summary(denial.b5)$r.squared
anova(denial.b5, denial.sdo)

#compassion
compassion.b5 <- lm(data=study2.scores, compassion ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism)
summary(compassion.b5)
# add in rwa
compassion.rwa <- lm(data=study2.scores, compassion ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism +rwa)
summary(compassion.rwa)
summary(compassion.rwa)$r.squared-summary(compassion.b5)$r.squared
anova(compassion.b5, compassion.rwa)
# add in sdo instead
compassion.sdo <- lm(data=study2.scores, compassion ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism +sdo)
summary(compassion.sdo)
summary(compassion.sdo)$r.squared-summary(compassion.b5)$r.squared
anova(compassion.b5, compassion.sdo)

#gravity
gravity.b5 <- lm(data=study2.scores, gravity ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism)
summary(gravity.b5)
# add in rwa
gravity.rwa <- lm(data=study2.scores, gravity ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism +rwa)
summary(gravity.rwa)
summary(gravity.rwa)$r.squared-summary(gravity.b5)$r.squared
anova(gravity.b5, gravity.rwa)
# add in sdo instead
gravity.sdo <- lm(data=study2.scores, gravity ~ Extraversion + Agreeableness + Openness + Conscientiousness + Neuroticism +sdo)
summary(gravity.sdo)
summary(gravity.sdo)$r.squared-summary(gravity.b5)$r.squared
anova(gravity.b5, gravity.sdo)

# hayes mediation
psych::mediate(denial ~ Openness +(rwa), data=study2.scores) 
psych::mediate(denial ~ Openness +(sdo), data=study2.scores) 
psych::mediate(denial ~ Openness +(rwa) -Extraversion -Agreeableness - Neuroticism - Conscientiousness, data=study2.scores) 
psych::mediate(denial ~ Openness +(sdo) -Extraversion -Agreeableness - Neuroticism - Conscientiousness, data=study2.scores) 

psych::mediate(compassion ~ Openness +(rwa), data=study2.scores) 
psych::mediate(compassion ~ Openness +(sdo), data=study2.scores) 
psych::mediate(compassion ~ Openness +(rwa) -Extraversion -Agreeableness - Neuroticism - Conscientiousness, data=study2.scores) 
psych::mediate(compassion ~ Openness +(sdo) -Extraversion -Agreeableness - Neuroticism - Conscientiousness, data=study2.scores) 

psych::mediate(gravity ~ Openness +(rwa), data=study2.scores) 
psych::mediate(gravity ~ Openness +(sdo), data=study2.scores) 
psych::mediate(gravity ~ Openness +(rwa) -Extraversion -Agreeableness - Neuroticism - Conscientiousness, data=study2.scores) 
psych::mediate(gravity ~ Openness +(sdo) -Extraversion -Agreeableness - Neuroticism - Conscientiousness, data=study2.scores) 


itemsUniHC <-  list(hatecrime = c("HC12", "HC15", "HC5", "HC8", "HC9", 
                                  "-HC13", "-HC3", "-HC6", "-X01", "-X02", "-X03", "-X04", "-X05", "-X06", "-X10",
                                  "HC7", "HC11", "X07", "X08", "X09") ,
                  rwa = c("-RWA1","-RWA2", "RWA3", "RWA4", "RWA5", "-RWA6", "RWA7",  "RWA8", "-RWA9", "RWA10","-RWA11","RWA12" ),
                  sdo = c("SDO1", "-SDO2","SDO3", "SDO4", "-SDO5", "SDO6", "-SDO7", "-SDO8", "-SDO9",
                          "-SDO10","SDO11","SDO12","-SDO13","SDO14","-SDO15","SDO16"   ),
                  
                  Extraversion=c( "BFI1", "-BFI6", "BFI11", "BFI16", "-BFI21", "BFI26", "-BFI31", "BFI36" ),
                  Agreeableness=c( "-BFI2", "BFI7", "-BFI12", "BFI17", "BFI22", "-BFI27", "BFI32", "-BFI37", "BFI42" ),
                  Conscientiousness=c( "BFI3", "-BFI8", "BFI13", "-BFI18", "-BFI23", "BFI28", "BFI33", "BFI38", "-BFI43"),
                  Neuroticism=c( "BFI4", "-BFI9", "BFI14", "BFI19", "-BFI24", "BFI29", "-BFI34", "BFI39"),
                  Openness=c( "BFI5", "BFI10", "BFI15", "BFI20", "BFI25", "BFI30", "-BFI35", "BFI40", "-BFI41", "BFI44")
                  
)
scoresUniHC <- scoreItems(itemsUniHC,study2all, missing=FALSE)
summary(scoresUniHC)


##### stop here



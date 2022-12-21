# script for preparing the Kingston University studydata April 2020


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readxl)
library(psych)
library(car)
library(ggplot2)
library(ggpubr)



studydata = read_excel("surveydata_cleaned_FULLAnon.xlsx")

# rename variables
colnames(studydata)[10] <- "age"
colnames(studydata)[11] <- "gender"
colnames(studydata)[12] <- "nationality"
colnames(studydata)[13] <- "residence"
colnames(studydata)[14] <- "postcode"
colnames(studydata)[15] <- "edu"
colnames(studydata)[16] <- "identity_religious"
colnames(studydata)[18] <- "covid_infection_status"


#national identity vars
colnames(studydata)[19] <- "sid_nation1"
colnames(studydata)[20] <- "sid_nation2"
colnames(studydata)[21] <- "sid_nation3"
colnames(studydata)[22] <- "sid_nation4"
# calculate national identity
studydata$sid_nation_full <- rowMeans(studydata[,19:22])
scale_sid_nation <- studydata[,19:22]

#national fusion vars
colnames(studydata)[23] <- "ift_nation1"
colnames(studydata)[24] <- "ift_nation2"
colnames(studydata)[25] <- "ift_nation3"
colnames(studydata)[26] <- "ift_nation4"
colnames(studydata)[27] <- "ift_nation5"
colnames(studydata)[28] <- "ift_nation6"
colnames(studydata)[29] <- "ift_nation7"
studydata$ift_nation_full <- rowMeans(studydata[,23:29])
scale_ift_nation <- studydata[,23:29]

# recode undesirables
colnames(studydata)[30] <- "wvs_undes_drugaddicts"
studydata$wvs_undes_drugaddicts[is.na(studydata$wvs_undes_drugaddicts)] <- 0
colnames(studydata)[31] <- "wvs_undes_diff_race"
studydata$wvs_undes_diff_race[is.na(studydata$wvs_undes_diff_race)] <- 0
colnames(studydata)[32] <- "wvs_undes_aids"
studydata$wvs_undes_aids[is.na(studydata$wvs_undes_aids)] <- 0
colnames(studydata)[33] <- "wvs_undes_immigrants"
studydata$wvs_undes_immigrants[is.na(studydata$wvs_undes_immigrants)] <- 0
colnames(studydata)[34] <- "wvs_undes_homosexuals"
studydata$wvs_undes_homosexuals[is.na(studydata$wvs_undes_homosexuals)] <- 0
colnames(studydata)[35] <- "wvs_undes_diff_religion"
studydata$wvs_undes_diff_religion[is.na(studydata$wvs_undes_diff_religion)] <- 0
colnames(studydata)[36] <- "wvs_undes_drinkers"
studydata$wvs_undes_drinkers[is.na(studydata$wvs_undes_drinkers)] <- 0
colnames(studydata)[37] <- "wvs_undes_unmarriedcouples"
studydata$wvs_undes_unmarriedcouples[is.na(studydata$wvs_undes_unmarriedcouples)] <- 0
colnames(studydata)[38] <- "wvs_undes_diff_language"
studydata$wvs_undes_diff_language[is.na(studydata$wvs_undes_diff_language)] <- 0


colnames(studydata)[43] <- "feelingstoward_muslims"
colnames(studydata)[44] <- "feelingstoward_christians"
colnames(studydata)[45] <- "feelingstoward_jews"
colnames(studydata)[46] <- "feelingstoward_atheists"
colnames(studydata)[47] <- "feelingstoward_immigrants"
colnames(studydata)[48] <- "feelingstoward_other_races"

#rename economic spectrums
colnames(studydata)[49] <- "politics_economic"
colnames(studydata)[50] <- "politics_social"
colnames(studydata)[51] <- "party_support"
colnames(studydata)[52] <- "EU_referendum"

#rename nationalism score
colnames(studydata)[53] <- "nationalism_1"
colnames(studydata)[54] <- "nationalism_2"
colnames(studydata)[55] <- "nationalism_3"
colnames(studydata)[56] <- "nationalism_4"
colnames(studydata)[57] <- "nationalism_5"
colnames(studydata)[58] <- "nationalism_6"
colnames(studydata)[59] <- "nationalism_7"
colnames(studydata)[60] <- "nationalism_8"
studydata$nationalism_orig <- rowMeans(studydata[,53:59])
studydata$nationalism_full <- rowMeans(studydata[,53:60])
scale_nationalism <- studydata[,53:59]
scale_nationalism_extended <- studydata[,53:60]

# rename supernatural belief scale
colnames(studydata)[61] <- "sbs1"
colnames(studydata)[62] <- "sbs2"
colnames(studydata)[63] <- "sbs3"
colnames(studydata)[64] <- "sbs4"
colnames(studydata)[65] <- "sbs5"
colnames(studydata)[66] <- "sbs6"
colnames(studydata)[67] <- "sbs7"
colnames(studydata)[68] <- "sbs8"
colnames(studydata)[69] <- "sbs9"
colnames(studydata)[70] <- "sbs10"
studydata$sbs_orig_full <- rowMeans(studydata[,61:70])
scale_sbs_origional <- studydata[,61:70]
# these two are not part of the origional scale, they're additions to address karma and a "universal force"
colnames(studydata)[71] <- "sbs11"
colnames(studydata)[72] <- "sbs12"
studydata$sbs_extended_full <- rowMeans(studydata[,61:70])
scale_sbs_extended <- studydata[,61:70]
# frequency of religious attendance
colnames(studydata)[73] <- "frequency"

# religious identity
colnames(studydata)[74] <- "sid_religious1"
colnames(studydata)[75] <- "sid_religious2"
colnames(studydata)[76] <- "sid_religious3"
colnames(studydata)[77] <- "sid_religious4"
studydata$sid_religious_full <- rowMeans(studydata[,74:77])
scale_sid_religion <- studydata[,74:77]

# fusion with religion
colnames(studydata)[78] <- "ift_religious1"
colnames(studydata)[79] <- "ift_religious2"
colnames(studydata)[80] <- "ift_religious3"
colnames(studydata)[81] <- "ift_religious4"
colnames(studydata)[82] <- "ift_religious5"
colnames(studydata)[83] <- "ift_religious6"
colnames(studydata)[84] <- "ift_religious7"
studydata$ift_religious_full <- rowMeans(studydata[,78:84])
scale_ift_religion <- studydata[,78:84]

# rename and reverse code appropriate personality variables
colnames(studydata)[85] <- "big_5_reserved"
studydata$big_5_reserved <- recode(studydata$big_5_reserved,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[86] <- "big_5_trusting"
colnames(studydata)[87] <- "big_5_lazy"
studydata$big_5_lazy <- recode(studydata$big_5_lazy,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[88] <- "big_5_relaxed"
colnames(studydata)[89] <- "big_5_unartistic"
studydata$big_5_unartistic <- recode(studydata$big_5_unartistic,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[90] <- "big_5_outgoing"
colnames(studydata)[91] <- "big_5_faultfinding"
studydata$big_5_faultfinding <- recode(studydata$big_5_faultfinding,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[92] <- "big_5_thorough"
colnames(studydata)[93] <- "big_5_nervous"
studydata$big_5_nervous <- recode(studydata$big_5_nervous,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[94] <- "big_5_imaginative"
scale_big_5 <- studydata[,85:94]
# now calculate the 5 dimensions
Openness <- data.frame(studydata$big_5_imaginative, studydata$big_5_unartistic)
Conscientiousness <- data.frame(studydata$big_5_thorough, studydata$big_5_trusting)
Extraversion <- data.frame(studydata$big_5_reserved, studydata$big_5_outgoing)
Agreeableness <- data.frame(studydata$big_5_faultfinding, studydata$big_5_relaxed)
Neuroticism <- data.frame(studydata$big_5_nervous, studydata$big_5_lazy)

studydata$Openness_full <- rowMeans(Openness)
studydata$Conscientiousness_full <- rowMeans(Conscientiousness)
studydata$Extraversion_full <- rowMeans(Extraversion)
studydata$Agreeableness_full <- rowMeans(Agreeableness)
studydata$Neuroticism_full <- rowMeans(Neuroticism)


#threat perception
colnames(studydata)[95] <- "cultureWar"
colnames(studydata)[96] <- "waylifethreatcovid"
colnames(studydata)[97] <- "financiallysecure"
studydata$financiallysecure <- recode(studydata$financiallysecure,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[98] <- "financiallyinsecure"
colnames(studydata)[99] <- "naturaldisasterconcern"
colnames(studydata)[100] <- "naturaldisasterunconcern"
studydata$naturaldisasterunconcern <- recode(studydata$naturaldisasterunconcern,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[101] <- "sociallysecure"
studydata$sociallysecure <- recode(studydata$sociallysecure,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[102] <- "sociallyinsecure"
colnames(studydata)[103] <- "contagionconcerned"
colnames(studydata)[104] <- "contagionunconcerned"
studydata$contagionunconcerned <- recode(studydata$contagionunconcerned,  '1=5; 2=4; 3=3; 4=2; 5=1')
colnames(studydata)[105] <- "predationconcern"
colnames(studydata)[106] <- "worldwillneverbethesame"
colnames(studydata)[107] <- "predationunconcerned"
studydata$predationunconcerned <- recode(studydata$predationunconcerned,  '1=5; 2=4; 3=3; 4=2; 5=1')

threat_extended <- data.frame(studydata$financiallyinsecure, 
                     studydata$financiallysecure,
                     studydata$naturaldisasterunconcern,
                     studydata$naturaldisasterconcern,
                     studydata$sociallyinsecure,
                     studydata$sociallysecure,
                     studydata$contagionconcerned,
                     studydata$contagionunconcerned,
                     studydata$predationconcern,
                     studydata$predationunconcerned)
studydata$threat_extended_full <- rowMeans(threat_extended)
studydata$threat_social <- rowMeans(threat_extended[,5:6])
studydata$threat_natural <- rowMeans(threat_extended[,3:4])
studydata$threat_financial <- rowMeans(threat_extended[,1:2])
studydata$threat_contagion <- rowMeans(threat_extended[,7:8])
studydata$threat_predation <- rowMeans(threat_extended[,9:10])


scale_threat_extended <- threat_extended
rm(threat_extended)
threat_simple <- data.frame(studydata$naturaldisasterunconcern,
                            studydata$naturaldisasterconcern,
                            studydata$sociallyinsecure,
                            studydata$sociallysecure,
                            studydata$contagionconcerned,
                            studydata$contagionunconcerned,
                            studydata$predationconcern,
                            studydata$predationunconcerned)
studydata$threat_simple_full <- rowMeans(threat_simple)
scale_threat_simple <- threat_simple
rm(threat_simple)
colnames(studydata)[108] <- "wayoflifethreatened"
colnames(studydata)[109] <- "socialmedia_timespent"
colnames(studydata)[110] <- "media_frequency"
colnames(studydata)[111] <- "news_preference"






###################################################################################################################################
# Descriptives

#gender
table(studydata$gender)

#age
table(studydata$age)
mean(na.omit(as.numeric(studydata$age)))
sd(na.omit(as.numeric(studydata$age)))
hist(as.numeric(studydata$age), 
     main="Participant Age Distribution", 
     xlab="Age")

#nationality
table(studydata$nationality)
table(studydata$residence)

#education
table(studydata$edu)

#infection status
table(studydata$covid_infection_status)

# party
table(studydata$party_support)

#eu referendum
table(studydata$EU_referendum)

#religion
table(studydata$identity_religious)

# religious frequency
table(studydata$frequency)
#hist(as.numeric(studydata$frequency))
hist(as.numeric(studydata$frequency), 
     main="Religious Attendance", 
     xlab="Freqeuncy")


# economic politics
#hist(as.numeric(studydata$politics_economic))
hist(as.numeric(studydata$politics_economic), 
     main="Economic Values (liberal-conservative)", 
     xlab="Economic Values")

# social politics
#hist(as.numeric(studydata$politics_social))
hist(as.numeric(studydata$politics_social), 
     main="Social Values (liberal-conservative)", 
     xlab="Social Values")






# Default call (as object)
#p <- ggplot(studydata, aes(politics_economic,politics_social))
#h3 <- p + stat_bin2d()
#h3


qplot(politics_economic,politics_social,data=studydata, geom='bin2d')

qplot(ift_nation_full,sid_nation_full,data=studydata, geom='bin2d')
qplot(ift_religious_full,sid_religious_full,data=studydata, geom='bin2d')

qplot(ift_nation_full,politics_economic,data=studydata, geom='bin2d')
qplot(ift_religious_full,politics_economic,data=studydata, geom='bin2d')

qplot(ift_nation_full,politics_social,data=studydata, geom='bin2d')
qplot(ift_religious_full,politics_social,data=studydata, geom='bin2d')

qplot(ift_nation_full,feelingstoward_other_races,data=studydata, geom='bin2d')
qplot(ift_religious_full,feelingstoward_other_races,data=studydata, geom='bin2d')
qplot(ift_nation_full,feelingstoward_immigrants,data=studydata, geom='bin2d')
qplot(ift_religious_full,feelingstoward_immigrants,data=studydata, geom='bin2d')

qplot(ift_religious_full,sbs_extended_full,data=studydata, geom='bin2d')
qplot(sid_religious_full,sbs_extended_full,data=studydata, geom='bin2d')

qplot(ift_religious_full,sbs_orig_full,data=studydata, geom='bin2d')
qplot(sid_religious_full,sbs_orig_full,data=studydata, geom='bin2d')

qplot(ift_religious_full,sbs11,data=studydata, geom='bin2d')
qplot(ift_religious_full,sbs12,data=studydata, geom='bin2d')


# threat
#hist(as.numeric(studydata$threat_simple_full))
hist(as.numeric(studydata$threat_simple_full), 
     main="Threat", 
     xlab="Threat")
#hist(as.numeric(studydata$threat_extended_full))
hist(as.numeric(studydata$threat_extended_full), 
     main="Threat", 
     xlab="Threat (extended)")

# nationalism
#hist(as.numeric(studydata$nationalism_full))
hist(as.numeric(studydata$nationalism_full), 
     main="Nationalism", 
     xlab="Nationalism")
#sbs
hist(as.numeric(studydata$sbs_orig_full), 
     main="Supernatural Belief Scale", 
     xlab="Supernatural Belief Strength")
hist(as.numeric(studydata$sbs_extended_full), 
     main="Supernatural Belief Scale (extended)", 
     xlab="Supernatural Belief Strength")







###################################################################################################################################
# Now for scale validation of the different scales
library(ltm)
cronbach.alpha(scale_sid_nation, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_sid_religion, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_ift_nation, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_ift_religion, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_nationalism, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_nationalism_extended, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_sbs_origional, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_sbs_extended, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_threat_simple, standardized = TRUE, CI = TRUE,na.rm=TRUE)
cronbach.alpha(scale_threat_extended, standardized = TRUE, CI = TRUE,na.rm=TRUE)


fit_personality <- principal(scale_big_5, nfactors=5, rotate="varimax")
fit_personality
load <- fit_personality$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(scale_big_5),cex=.7) # add variable names

library(nFactors)
ev <- eigen(cor(na.omit(scale_big_5))) # get eigenvalues
ap <- parallel(subject=nrow(scale_big_5),var=ncol(scale_big_5),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(scale_big_5) # graphs generated automatically


fit_threat_simple <- principal(scale_threat_simple, nfactors=4, rotate="varimax")
fit_threat_simple
load <- fit_threat_simple$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(scale_threat_simple),cex=.7) # add variable names

library(nFactors)
ev <- eigen(cor(na.omit(scale_threat_simple))) # get eigenvalues
ap <- parallel(subject=nrow(scale_threat_simple),var=ncol(scale_threat_simple),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(scale_threat_simple) # graphs generated automatically




fit_threat_extended <- principal(scale_threat_extended, nfactors=4, rotate="varimax")
fit_threat_extended
load <- fit_threat_extended$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(scale_threat_extended),cex=.7) # add variable names

#library(nFactors)
ev <- eigen(cor(na.omit(scale_threat_extended))) # get eigenvalues
ap <- parallel(subject=nrow(scale_threat_extended),var=ncol(scale_threat_extended),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# PCA Variable Factor Map
#library(FactoMineR)
result <- PCA(scale_threat_extended) # graphs generated automatically




fit_nationalism <- principal(scale_nationalism, nfactors=1, rotate="varimax")
fit_nationalism
load <- fit_nationalism$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(scale_nationalism),cex=.7) # add variable names

library(nFactors)
ev <- eigen(cor(na.omit(scale_nationalism))) # get eigenvalues
ap <- parallel(subject=nrow(scale_nationalism),var=ncol(scale_nationalism),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(scale_nationalism) # graphs generated automatically





fit_nationalism_extended <- principal(scale_nationalism_extended, nfactors=2, rotate="varimax")
fit_nationalism_extended
load <- fit_nationalism_extended$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(scale_nationalism_extended),cex=.7) # add variable names

library(nFactors)
ev <- eigen(cor(na.omit(scale_nationalism_extended))) # get eigenvalues
ap <- parallel(subject=nrow(scale_nationalism_extended),var=ncol(scale_nationalism_extended),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(scale_nationalism_extended) # graphs generated automatically












###################################################################################################################################
# Now for between groups analysis

library(ggpubr)
bpg <- ggboxplot(studydata, x = "gender", y = "threat_simple_full", 
          color = "gender", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("M", "F", "O"),
          ylab = "Threat", xlab = "Gender")
bpg + rotate_x_text(45)
gen_threat_aov <- aov(threat_simple_full ~ gender, data = studydata)
summary(gen_threat_aov)
TukeyHSD(gen_threat_aov)




library(ggpubr)
bpn <- ggboxplot(studydata, x = "nationality", y = "threat_simple_full", 
          color = "nationality", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = unique(studydata$nationality),
          ylab = "Threat", xlab = "Nationality")
bpn + rotate_x_text(45)
nation_threat_aov <- aov(threat_simple_full ~ nationality, data = studydata)
summary(nation_threat_aov)
TukeyHSD(nation_threat_aov)



library(ggpubr)
bp <- ggboxplot(studydata, x = "identity_religious", y = "threat_simple_full", 
          color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = unique(studydata$identity_religious),
          ylab = "Threat", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_threat_aov <- aov(threat_simple_full ~ identity_religious, data = studydata)
summary(rel_threat_aov)
TukeyHSD(rel_threat_aov)




library(ggpubr)
bpp <- ggboxplot(studydata, x = "party_support", y = "threat_simple_full", 
                color = "party_support", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = c("Republican", "Democrat", "Libertarian", "Green Party", "Labor", "Liberal Democrat", "Independent", "Liberal"),
                ylab = "Threat", xlab = "Political Party")
bpp + rotate_x_text(45)
party_threat_aov <- aov(threat_simple_full ~ party_support, data = studydata)
summary(party_threat_aov)
TukeyHSD(party_threat_aov)


# now for individual threats
#threat_predation
#threat_contagion
#threat_financial
#threat_natural
#threat_social

library(ggpubr)
bpp <- ggboxplot(studydata, x = "party_support", y = "threat_social", 
                 color = "party_support", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 order = c("Republican", "Democrat", "Libertarian", "Green Party", "Labor", "Liberal Democrat", "Independent", "Liberal"),
                 ylab = "Threat", xlab = "Political Party")
bpp + rotate_x_text(45)
party_threat_aov <- aov(threat_predation ~ party_support, data = studydata)
summary(party_threat_aov)
TukeyHSD(party_threat_aov)



bp <- ggboxplot(studydata, x = "identity_religious", y = "threat_predation", 
                color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = unique(studydata$identity_religious),
                ylab = "Threat", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_threat_aov <- aov(threat_social ~ identity_religious, data = studydata)
summary(rel_threat_aov)
TukeyHSD(rel_threat_aov)



# politics by nationality and religion 
bp <- ggboxplot(studydata, x = "identity_religious", y = "politics_economic", 
                color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = unique(studydata$identity_religious),
                ylab = "Politics Economic", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_threat_aov <- aov(politics_economic ~ identity_religious, data = studydata)
summary(rel_threat_aov)
TukeyHSD(rel_threat_aov)

bp <- ggboxplot(studydata, x = "identity_religious", y = "politics_social", 
                color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = unique(studydata$identity_religious),
                ylab = "Politics Social", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_threat_aov <- aov(politics_social ~ identity_religious, data = studydata)
summary(rel_threat_aov)
TukeyHSD(rel_threat_aov)


library(ggpubr)
bpn <- ggboxplot(studydata, x = "nationality", y = "politics_economic", 
                 color = "nationality", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 order = unique(studydata$nationality),
                 ylab = "Politics Economic", xlab = "Nationality")
bpn + rotate_x_text(45)
nation_threat_aov <- aov(politics_economic ~ nationality, data = studydata)
summary(nation_threat_aov)
TukeyHSD(nation_threat_aov)


bpn <- ggboxplot(studydata, x = "nationality", y = "politics_social", 
                 color = "nationality", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                 order = unique(studydata$nationality),
                 ylab = "Politcs Social", xlab = "Nationality")
bpn + rotate_x_text(45)
nation_threat_aov <- aov(politics_social ~ nationality, data = studydata)
summary(nation_threat_aov)
TukeyHSD(nation_threat_aov)




bp <- ggboxplot(studydata, x = "identity_religious", y = "nationalism_full", 
                color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = unique(studydata$identity_religious),
                ylab = "Nationalism", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_nationalism_aov <- aov(nationalism_full ~ identity_religious, data = studydata)
summary(rel_nationalism_aov)
TukeyHSD(rel_nationalism_aov)



bp <- ggboxplot(studydata, x = "identity_religious", y = "sbs11", 
                color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = unique(studydata$identity_religious),
                ylab = "SBS Karma", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_karma_aov <- aov(sbs11 ~ identity_religious, data = studydata)
summary(rel_karma_aov)
TukeyHSD(rel_karma_aov)



bp <- ggboxplot(studydata, x = "identity_religious", y = "sbs12", 
                color = "identity_religious", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = unique(studydata$identity_religious),
                ylab = "SBS Universe", xlab = "Religious Identity")
bp + rotate_x_text(45)
rel_universe_aov <- aov(sbs12 ~ identity_religious, data = studydata)
summary(rel_universe_aov)
TukeyHSD(rel_universe_aov)









########################################################################
# some regressions for analyses
library(QuantPsyc)

# First break out all the threat correlates by measure

nat_threat <- lm(nationalism_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                 data = studydata)
summary(nat_threat)

immithreat_glm_fit <- glm(wvs_undes_immigrants ~  threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                          data = studydata)
summary(immithreat_glm_fit)

sbs_threat <- lm(sbs_orig_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                 data = studydata)
summary(sbs_threat)

sbs_ex_threat <- lm(sbs_extended_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                 data = studydata)
summary(sbs_ex_threat)

rel_sid_threat <- lm(sid_religious_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                    data = studydata)
summary(rel_sid_threat)

rel_ift_threat <- lm(ift_religious_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                     data = studydata)
summary(rel_ift_threat)

nat_sid_threat <- lm(sid_nation_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                     data = studydata)
summary(nat_sid_threat)

nat_ift_threat <- lm(ift_nation_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                     data = studydata)
summary(nat_ift_threat)




open_threat <- lm(Openness_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                  data = studydata)
summary(open_threat)

consc_threat <- lm(Conscientiousness_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                  data = studydata)
summary(consc_threat)

extrav_threat <- lm(Extraversion_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                   data = studydata)
summary(extrav_threat)

agree_threat <- lm(Agreeableness_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                    data = studydata)
summary(agree_threat)

neurot_threat <- lm(Neuroticism_full ~ threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                   data = studydata)
summary(neurot_threat)


# so religious people appear far more nationalistic than nonreligious across identities. 
reg_nation_rel_threat <- lm(nationalism_full ~ identity_religious + sid_religious_full + ift_religious_full + threat_predation + threat_contagion + threat_financial + threat_natural + threat_social, 
                            data = studydata)
summary(reg_nation_rel_threat)


glm_immi_all_fit <- glm(wvs_undes_immigrants ~ identity_religious + covid_infection_status + 
                 politics_economic + politics_social + frequency + cultureWar + waylifethreatcovid + worldwillneverbethesame + 
                 sid_religious_full + ift_religious_full +
                 sid_nation_full + ift_nation_full +
                 Openness_full + Conscientiousness_full + Extraversion_full + Agreeableness_full + Neuroticism_full + 
                 threat_predation + threat_contagion + threat_financial + threat_natural + threat_social +
                 socialmedia_timespent + media_frequency
               , data = studydata)
summary(glm_immi_other_fit)


glm_immi_protestant_fit <- glm(wvs_undes_immigrants ~ covid_infection_status + 
                            politics_economic + politics_social + frequency + cultureWar + waylifethreatcovid + worldwillneverbethesame + 
                            sid_religious_full + ift_religious_full +
                            sid_nation_full + ift_nation_full +
                            Openness_full + Conscientiousness_full + Extraversion_full + Agreeableness_full + Neuroticism_full + 
                            threat_predation + threat_contagion + threat_financial + threat_natural + threat_social +
                            socialmedia_timespent + media_frequency,
                            data = subset(studydata, identity_religious=="Protestant"))
summary(glm_immi_protestant_fit)

glm_immi_muslim_fit <- glm(wvs_undes_immigrants ~ covid_infection_status + 
                                 politics_economic + politics_social + frequency + cultureWar + waylifethreatcovid + worldwillneverbethesame + 
                                 sid_religious_full + ift_religious_full +
                                 sid_nation_full + ift_nation_full +
                                 Openness_full + Conscientiousness_full + Extraversion_full + Agreeableness_full + Neuroticism_full + 
                                 threat_predation + threat_contagion + threat_financial + threat_natural + threat_social +
                                 socialmedia_timespent + media_frequency,
                               data = subset(studydata, identity_religious=="Muslim"))
summary(glm_immi_muslim_fit)


glm_rel_immi_fit_all <-glm(wvs_undes_immigrants ~ identity_religious,
                           data = studydata)
summary(glm_rel_immi_fit_all)


national1 <- lm(nationalism_full ~ age + gender + identity_religious + nationality + edu + covid_infection_status + 
                  politics_economic + politics_social + frequency + cultureWar + waylifethreatcovid + worldwillneverbethesame + 
                  sid_nation_full + sid_religious_full + ift_religious_full + ift_nation_full +
                  sbs_extended_full + threat_extended_full + Openness_full + Conscientiousness_full + 
                  Extraversion_full + Agreeableness_full + Neuroticism_full + frequency +
                  socialmedia_timespent + media_frequency + wvs_undes_drugaddicts + 
                  wvs_undes_diff_race + wvs_undes_aids + wvs_undes_immigrants + wvs_undes_homosexuals + wvs_undes_diff_religion + 
                  wvs_undes_drinkers + wvs_undes_unmarriedcouples + wvs_undes_diff_language, data = studydata)
summary(national1)


cor.test(as.numeric(studydata$age), studydata$nationalism_full, method = "spearman", use = "na.or.complete")

qplot(as.numeric(age),nationalism_full,data=studydata, geom='bin2d')

qplot(as.numeric(age),nationalism_full,data=studydata[which(studydata$gender=='M'),], geom='bin2d')
qplot(as.numeric(age),nationalism_full,data=studydata[which(studydata$gender=='F'),], geom='bin2d')



immigrant1 <- lm(wvs_undes_immigrants ~ as.numeric(age) + gender + identity_religious + nationality + edu + covid_infection_status + 
                  politics_economic + politics_social + frequency + cultureWar + waylifethreatcovid + worldwillneverbethesame + 
                  sid_nation_full + sid_religious_full + ift_religious_full + ift_nation_full +
                  sbs_extended_full + threat_extended_full + Openness_full + Conscientiousness_full + 
                  Extraversion_full + Agreeableness_full + Neuroticism_full + frequency +
                  socialmedia_timespent + media_frequency + wvs_undes_drugaddicts + 
                  wvs_undes_diff_race + wvs_undes_aids + wvs_undes_homosexuals + wvs_undes_diff_religion + 
                  wvs_undes_drinkers + wvs_undes_unmarriedcouples + wvs_undes_diff_language, data = studydata)
summary(immigrant1)


glm.fit <- glm(wvs_undes_immigrants ~ as.numeric(age) + gender + identity_religious + edu + covid_infection_status + 
                 politics_economic + politics_social + frequency + cultureWar + waylifethreatcovid + worldwillneverbethesame + 
                 sid_nation_full + sid_religious_full + ift_religious_full + ift_nation_full +
                 sbs_extended_full + threat_extended_full + Openness_full + Conscientiousness_full + 
                 Extraversion_full + Agreeableness_full + Neuroticism_full + frequency +
                 socialmedia_timespent + media_frequency + wvs_undes_drugaddicts + 
                 wvs_undes_diff_race + wvs_undes_aids + wvs_undes_homosexuals + wvs_undes_diff_religion + 
                 wvs_undes_drinkers + wvs_undes_unmarriedcouples + wvs_undes_diff_language, data = studydata)
summary(glm.fit)



econpolitics <- lm(politics_economic ~ age + gender + edu + covid_infection_status + media_frequency + socialmedia_timespent + 
                     threat_simple_full + threat_extended_full, data=studydata)
summary(econpolitics)
lm.beta(econpolitics)


econpolitics2 <- lm(politics_economic ~ as.numeric(age) + gender + edu + covid_infection_status + media_frequency + socialmedia_timespent + 
                      threat_social + threat_natural + threat_financial + threat_contagion + threat_predation,
                    data=studydata)
summary(econpolitics2)
lm.beta(econpolitics2)


socialpolitics2 <- lm(politics_social ~ as.numeric(age) + gender + edu + covid_infection_status + media_frequency + socialmedia_timespent + 
                      threat_social + threat_natural + threat_financial + threat_contagion + threat_predation,
                    data=studydata)
summary(socialpolitics2)
lm.beta(socialpolitics2)

reli1 <- lm(sbs_extended_full ~ as.numeric(age) + gender + edu + covid_infection_status + media_frequency + socialmedia_timespent + 
                        ift_religious_full + sid_religious_full + frequency,
                      data=studydata)
summary(reli1)
lm.beta(reli1)

reli2 <- lm(sbs_orig_full ~ as.numeric(age) + gender + edu + covid_infection_status + media_frequency + socialmedia_timespent + 
              ift_religious_full + sid_religious_full + frequency,
            data=studydata)
summary(reli2)
lm.beta(reli2)





cor.test(studydata$nationalism_full, studydata$sbs_orig_full, method = "spearman", use = "na.or.complete")
cor.test(studydata$nationalism_full, studydata$sid_religious_full, method = "spearman", use = "na.or.complete")
cor.test(studydata$nationalism_orig, studydata$nationalism_8, method = "spearman", use = "na.or.complete")

#To be truly part of my nation one must be born in the nation
#To be truly part of my nation one must live in the nation most of their life
#To be truly part of my nation one must be part of my religious group
#To be truly part of my nation one must speak my language
#To be truly part of my nation one must hold our citizenship
#To be truly part of my nation one must feel like they're part of my nationality
#To be truly part of my nation one must respect its laws
#To be truly part of my nation one must be of a specific race or ethnicity

# test for mediation X = religion, y = nationalism_orig, m1 = nationalism_8




###################################################################################################################################
# the code below will create a correlation plot of variables

important_data <- data.frame(studydata$sid_nation_full,
                             studydata$ift_nation_full,
                             studydata$feelingstoward_muslims,
                             studydata$feelingstoward_christians,
                             studydata$feelingstoward_jews,
                             studydata$feelingstoward_atheists,
                             studydata$feelingstoward_immigrants,
                             studydata$feelingstoward_other_races,
                             studydata$politics_economic,
                             studydata$politics_social,
                             studydata$nationalism_full,
                             studydata$sbs_extended_full,
                             studydata$frequency,
                             studydata$sid_religious_full,
                             studydata$ift_religious_full,
                             studydata$Openness_full,
                             studydata$Conscientiousness_full,
                             studydata$Extraversion_full,
                             studydata$Agreeableness_full,
                             studydata$Neuroticism_full,
                             studydata$cultureWar,
                             studydata$wayoflifethreatened,
                             studydata$waylifethreatcovid,
                             studydata$threat_social,
                             studydata$threat_contagion,
                             studydata$threat_financial,
                             studydata$threat_natural,
                             studydata$threat_predation,
                             studydata$socialmedia_timespent,
                             studydata$media_frequency)

important_data_num <- sapply( important_data, as.numeric )
important_data_num <- as.matrix(na.omit(important_data_num))
library(xtable)
library(Hmisc)
icor<-round(cor(important_data_num),2)
upper<-icor
upper[upper.tri(icor)]<-""
upper<-as.data.frame(upper)
print(xtable(upper), type="html")
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 



corstars(icor, result="html")

icorBIG<-round(cor(important_data_num),2)
corstars(icorBIG, result="html")


res2 <- rcorr(as.matrix(na.omit(important_data_num)))
res2


library(corrplot)
#corrplot(res, type = "upper", order = "hclust", sig.level = 0.05, tl.col = "black", tl.srt = 45)

# Insignificant correlation are blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank",tl.col = "black", tl.srt = 45)











###################################################################################################################################
# Now for the mediations and SEM part
library(lavaan)
library(semPlot)



threat_conservative_model <- '
  #measurement model
  threat =~ threat_financial + threat_contagion + threat_social + threat_predation + threat_natural
  #nationalism =~ nationalism_1 + nationalism_2 + nationalism_3 + nationalism_4 + nationalism_5 + nationalism_6 + nationalism_7 + nationalism_8
  # regressions
  nationalism_full ~ threat
  politics_social ~ threat + nationalism_full
  politics_economic ~ threat + nationalism_full
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'
semfit1 <- lavaan::sem(model = threat_conservative_model, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1, boot.ci.type = "bca.simple")
semPaths(semfit1,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1, fit.measures = TRUE)



immi_threat_conservative_model <- '
  #measurement model
  threat =~ threat_financial + threat_contagion + threat_social + threat_predation + threat_natural
  #nationalism =~ nationalism_1 + nationalism_2 + nationalism_3 + nationalism_4 + nationalism_5 + nationalism_6 + nationalism_7 + nationalism_8
  # regressions
  nationalism_full ~ threat
  politics_social ~ threat + nationalism_full
  politics_economic ~ threat + nationalism_full
  wvs_undes_immigrants ~ politics_social + politics_economic
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'
semfit2 <- lavaan::sem(model = immi_threat_conservative_model, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit2, boot.ci.type = "bca.simple")
semPaths(semfit2,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit2, fit.measures = TRUE)


immi_threat_conserv_b5_model <- '
  #measurement model
  threat =~ threat_financial + threat_contagion + threat_social + threat_predation + threat_natural
  #nationalism =~ nationalism_1 + nationalism_2 + nationalism_3 + nationalism_4 + nationalism_5 + nationalism_6 + nationalism_7 + nationalism_8
  # regressions
  nationalism_full ~ threat
  politics_social ~ threat + nationalism_full
  politics_economic ~ threat + nationalism_full
  wvs_undes_immigrants ~ politics_social + politics_economic
  politics_social ~ Openness_full
  politics_economic ~ Openness_full
  Openness_full ~ Neuroticism_full
  Openness_full ~ Agreeableness_full
  Neuroticism_full ~ socialmedia_timespent
  Agreeableness_full ~ socialmedia_timespent
  Neuroticism_full ~ media_frequency
  Agreeableness_full ~ media_frequency
  wayoflifethreatened ~ Openness_full
  wayoflifethreatened ~ sid_nation_full
  wayoflifethreatened ~ ift_nation_full
  wvs_undes_immigrants ~ wayoflifethreatened
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'
semfit3 <- lavaan::sem(model = immi_threat_conserv_b5_model, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit3, boot.ci.type = "bca.simple")
semPaths(semfit3,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit3, fit.measures = TRUE)




immi_threat_2_conserv_b5_model <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  #nationalism =~ nationalism_1 + nationalism_2 + nationalism_3 + nationalism_4 + nationalism_5 + nationalism_6 + nationalism_7 + nationalism_8
  # regressions
  nationalism_full ~ threat_soc_pred
  nationalism_full ~ threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full
  #politics_social ~ threat_cont_fin_nat + nationalism_full
  #politics_economic ~ threat_cont_fin_nat + nationalism_full
  wvs_undes_immigrants ~ politics_social + politics_economic
  politics_social ~ Openness_full
  politics_economic ~ Openness_full
  Openness_full ~ Neuroticism_full
  Openness_full ~ Agreeableness_full
  Neuroticism_full ~ socialmedia_timespent
  Agreeableness_full ~ socialmedia_timespent
  Neuroticism_full ~ media_frequency
  Agreeableness_full ~ media_frequency
  wayoflifethreatened ~ Openness_full
  wayoflifethreatened ~ sid_nation_full
  wayoflifethreatened ~ ift_nation_full
  wvs_undes_immigrants ~ wayoflifethreatened
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'

semfit4 <- lavaan::sem(model = immi_threat_2_conserv_b5_model, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit4, boot.ci.type = "bca.simple")
semPaths(semfit4,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit4, fit.measures = TRUE)



threat_conservative_model_b <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  # regressions
  nationalism_full ~ threat_soc_pred + threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full
  wvs_undes_immigrants ~ politics_social + politics_economic
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'
semfit1b <- lavaan::sem(model = threat_conservative_model_b, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1b, boot.ci.type = "bca.simple")
semPaths(semfit1b,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1b, fit.measures = TRUE)



threat_conservative_model_c <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  # regressions
  nationalism_full ~ threat_soc_pred + threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + sid_religious_full
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + sbs_extended_full
  wvs_undes_immigrants ~ politics_social + politics_economic
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'
semfit1c <- lavaan::sem(model = threat_conservative_model_c, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1c, boot.ci.type = "bca.simple")
semPaths(semfit1c,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1c, fit.measures = TRUE)









threat_conservative_model_d <- '
  #measurement model
threat_soc_pred =~ threat_social + threat_predation
threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
# regressions
nationalism_full ~ threat_soc_pred + threat_cont_fin_nat
politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + sid_religious_full + sbs_extended_full
politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + sid_religious_full + sbs_extended_full
wvs_undes_immigrants ~ politics_social + politics_economic
# residuals
#threat_financial ~~ threat_contagion
#threat_financial ~~ threat_natural
#threat_contagion ~~ threat_natural
#threat_social ~~ threat_predation
'
semfit1d <- lavaan::sem(model = threat_conservative_model_d, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1d, boot.ci.type = "bca.simple")
semPaths(semfit1d,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1d, fit.measures = TRUE)



threat_conservative_model_e <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  rel =~ sid_religious_full + sbs_extended_full
  # regressions
  nationalism_full ~ threat_soc_pred + threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  wvs_undes_immigrants ~ politics_social + politics_economic
  # residuals
  #threat_financial ~~ threat_contagion
  #threat_financial ~~ threat_natural
  #threat_contagion ~~ threat_natural
  #threat_social ~~ threat_predation
'
semfit1e <- lavaan::sem(model = threat_conservative_model_e, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1e, boot.ci.type = "bca.simple")
semPaths(semfit1e,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1e, fit.measures = TRUE)







threat_conservative_model_f <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  rel =~ sid_religious_full + sbs_extended_full
  # regressions
  nationalism_full ~ threat_soc_pred + threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  wvs_undes_immigrants ~ politics_social + politics_economic + wayoflifethreatened
  wayoflifethreatened ~ sid_nation_full
  wayoflifethreatened ~ ift_nation_full
  #wvs_undes_immigrants ~ wayoflifethreatened
  # residuals
  #none
'
semfit1f <- lavaan::sem(model = threat_conservative_model_f, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1f, boot.ci.type = "bca.simple")
semPaths(semfit1f,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1f, fit.measures = TRUE)





threat_conservative_model_g <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  rel =~ sid_religious_full + sbs_extended_full
  # regressions
  nationalism_full ~ threat_soc_pred + threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  wvs_undes_immigrants ~ politics_social + politics_economic + wayoflifethreatened
  wvs_undes_immigrants ~ sid_nation_full
  wvs_undes_immigrants ~ ift_nation_full
  #wvs_undes_immigrants ~ wayoflifethreatened
  # residuals
  #none
'
semfit1g <- lavaan::sem(model = threat_conservative_model_g, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1g, boot.ci.type = "bca.simple")
semPaths(semfit1g,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1g, fit.measures = TRUE)





threat_conservative_model_h <- '
  #measurement model
  threat_soc_pred =~ threat_social + threat_predation
  threat_cont_fin_nat =~ threat_financial + threat_contagion + threat_natural
  rel =~ sid_religious_full + sbs_extended_full
  # regressions
  #ift_nation_full ~ threat_soc_pred + threat_cont_fin_nat
  nationalism_full ~ ift_nation_full + threat_soc_pred + threat_cont_fin_nat
  #wayoflifethreatened ~ threat_soc_pred + threat_cont_fin_nat
  politics_social ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  politics_economic ~ threat_soc_pred + threat_cont_fin_nat + nationalism_full + rel
  wvs_undes_immigrants ~ politics_social + politics_economic
  wvs_undes_immigrants ~ sid_nation_full
  wvs_undes_immigrants ~ ift_nation_full
  #wvs_undes_immigrants ~ wayoflifethreatened
  # residuals
  #none
'
semfit1h <- lavaan::sem(model = threat_conservative_model_h, data = studydata, se = "boot", bootstrap = 1000)
lavaan::parameterEstimates(semfit1h, boot.ci.type = "bca.simple")
semPaths(semfit1h,"std",edge.label.cex=1.5, curvePivot = TRUE)
summary(semfit1h, fit.measures = TRUE)


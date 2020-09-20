# analysis of the simulation data


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



library(dplyr)
library(ggpubr)
library(QuantPsyc)
library(readxl)
library(psych)
library(car)
library(ggplot2)

simdata = read.csv("20200520_natrelimmmodel_parametersweep.csv", header = TRUE, sep = ',')

# recode variables for names so that its easier to understand
# the pattern is 1-5 = social, financial, contagion, natural, predation
colnames(simdata)[6] <- "initial_concern_social"
colnames(simdata)[7] <- "initial_concern_financial"
colnames(simdata)[8] <- "initial_concern_contagion"
colnames(simdata)[9] <- "initial_concern_natural"
colnames(simdata)[10] <- "initial_concern_predation"

colnames(simdata)[34] <- "engagement_social"
colnames(simdata)[35] <- "engagement_financial"
colnames(simdata)[36] <- "engagement_contagion"
colnames(simdata)[37] <- "engagement_natural"
colnames(simdata)[38] <- "engagement_predation"

colnames(simdata)[39] <- "energy_social"
colnames(simdata)[40] <- "energy_financial"
colnames(simdata)[41] <- "energy_contagion"
colnames(simdata)[42] <- "energy_natural"
colnames(simdata)[43] <- "energy_predation"

colnames(simdata)[44] <- "addedEnergy_social"
colnames(simdata)[45] <- "addedEnergy_financial"
colnames(simdata)[46] <- "addedEnergy_contagion"
colnames(simdata)[47] <- "addedEnergy_natural"
colnames(simdata)[48] <- "addedEnergy_predation"


# Now create quartiles for key variables

simdata <- simdata %>% mutate(nationalism_quartile = ntile(nationalism_level, 4))
simdata <- simdata %>% mutate(anti_immi_sent_quartile = ntile(anti_immigrant_sentiment, 4))
simdata <- simdata %>% mutate(economic_conservativism_quartile = ntile(economic_conservativism, 4))
simdata <- simdata %>% mutate(social_conservativism_quartile = ntile(social_conservativism, 4))
simdata <- simdata %>% mutate(social_media_quartile = ntile(socialMediaUse, 4))
simdata <- simdata %>% mutate(tvMediaUse_quartile = ntile(tvMediaUse, 4))
simdata <- simdata %>% mutate(threat_con_fin_nat_quartile = ntile(threat_con_fin_nat, 4))
simdata <- simdata %>% mutate(threat_soc_pred_quartile = ntile(threat_soc_pred, 4))
simdata <- simdata %>% mutate(sociographic_prudery_quartile = ntile(sociographic_prudery, 4))
simdata <- simdata %>% mutate(anthropomorphic_promiscuity_quartile = ntile(anthropomorphic_promiscuity, 4))





tiff("anti_immigrant_highNationalismXsocialconservativism_highNationalism.tiff", units="in", width=6, height=5, res=300)
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$social_conservativism[simdata$nationalism_quartile==1],
     main="Anti-immigrant sentiment X conservativism 
     (non-nationalists)", 
     xlab="Anti-Immigrant Sentiment",
     ylab="Social Conservativism")
dev.off()

plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$social_conservativism[simdata$nationalism_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$economic_conservativism[simdata$nationalism_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$economic_conservativism[simdata$nationalism_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$anthropomorphic_promiscuity[simdata$nationalism_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$sociographic_prudery[simdata$nationalism_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$anthropomorphic_promiscuity[simdata$nationalism_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$sociographic_prudery[simdata$nationalism_quartile==1])



#plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==1],simdata$social_conservativism[simdata$social_media_quartile==1])
tiff("anti_immigrant_highNationalismXsocialconservativism_lowsocialmedia.tiff", units="in", width=6, height=5, res=300)
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==1],simdata$social_conservativism[simdata$social_media_quartile==1],
     main="Anti-immigrant sentiment X conservativism 
     (low social media use)", 
     xlab="Anti-Immigrant Sentiment",
     ylab="Social Conservativism")
dev.off()


tiff("anti_immigrant_highNationalismXsocialconservativism_highsocialmedia.tiff", units="in", width=6, height=5, res=300)
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==4],simdata$social_conservativism[simdata$social_media_quartile==4],
     main="Anti-immigrant sentiment X conservativism 
     (high social media use)", 
     xlab="Anti-Immigrant Sentiment",
     ylab="Social Conservativism")
dev.off()

plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==1],simdata$economic_conservativism[simdata$social_media_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==4],simdata$economic_conservativism[simdata$social_media_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==4],simdata$anthropomorphic_promiscuity[simdata$social_media_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==4],simdata$sociographic_prudery[simdata$social_media_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==1],simdata$anthropomorphic_promiscuity[simdata$social_media_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$social_media_quartile==1],simdata$sociographic_prudery[simdata$social_media_quartile==1])



plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==1],simdata$social_conservativism[simdata$tvMediaUse_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==4],simdata$social_conservativism[simdata$tvMediaUse_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==1],simdata$economic_conservativism[simdata$tvMediaUse_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==4],simdata$economic_conservativism[simdata$tvMediaUse_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==4],simdata$anthropomorphic_promiscuity[simdata$tvMediaUse_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==4],simdata$sociographic_prudery[simdata$tvMediaUse_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==1],simdata$anthropomorphic_promiscuity[simdata$tvMediaUse_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$tvMediaUse_quartile==1],simdata$sociographic_prudery[simdata$tvMediaUse_quartile==1])


plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==1],simdata$social_conservativism[simdata$threat_con_fin_nat_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==4],simdata$social_conservativism[simdata$threat_con_fin_nat_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==1],simdata$economic_conservativism[simdata$threat_con_fin_nat_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==4],simdata$economic_conservativism[simdata$threat_con_fin_nat_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==4],simdata$anthropomorphic_promiscuity[simdata$threat_con_fin_nat_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==4],simdata$sociographic_prudery[simdata$threat_con_fin_nat_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==1],simdata$anthropomorphic_promiscuity[simdata$threat_con_fin_nat_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$threat_con_fin_nat_quartile==1],simdata$sociographic_prudery[simdata$threat_con_fin_nat_quartile==1])


plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==1],simdata$social_conservativism[simdata$threat_soc_pred_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==4],simdata$social_conservativism[simdata$threat_soc_pred_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==1],simdata$economic_conservativism[simdata$threat_soc_pred_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==4],simdata$economic_conservativism[simdata$threat_soc_pred_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==4],simdata$anthropomorphic_promiscuity[simdata$threat_soc_pred_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==4],simdata$sociographic_prudery[simdata$threat_soc_pred_quartile==4])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==1],simdata$anthropomorphic_promiscuity[simdata$threat_soc_pred_quartile==1])
plot(simdata$anti_immigrant_sentiment[simdata$threat_soc_pred_quartile==1],simdata$sociographic_prudery[simdata$threat_soc_pred_quartile==1])

# Plot anti-immigrant sentiment and threat for low nationalists
tiff("anti_immigrant_lowNationalismXthreat_con_fin_nat_lownationalism.tiff", units="in", width=6, height=5, res=300)
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$threat_con_fin_nat[simdata$nationalism_quartile==1],
     main="Anti-immigrant sentiment X threat 
     (low nationalism)", 
     xlab="Anti-Immigrant Sentiment",
     ylab="Threat: contagion, financial, natural")
dev.off()

# Plot anti-immigrant sentiment and threat for high nationalists
tiff("anti_immigrant_highNationalismXthreat_con_fin_nat_highnationalism.tiff", units="in", width=6, height=5, res=300)
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$threat_con_fin_nat[simdata$nationalism_quartile==4],
     main="Anti-immigrant sentiment X threat 
     (high nationalism)", 
     xlab="Anti-Immigrant Sentiment",
     ylab="Threat: contagion, financial, natural")
dev.off()



# ok, now for some added analyses to validate the simuation
# export some as tiff files for publication as requested



hist(as.numeric(simdata$economic_conservativism), 
     main="economic_conservativism", 
     xlab="economic_conservativism")
hist(as.numeric(simdata$social_conservativism), 
     main="social_conservativism", 
     xlab="social_conservativism")
hist(as.numeric(simdata$nationalism_level), 
     main="nationalism_level", 
     xlab="nationalism_level")

tiff("anthropomorphic promiscuity.tiff", units="in", width=6, height=5, res=300)
hist(as.numeric(simdata$anthropomorphic_promiscuity), 
     main="Anthropomorphic promiscuity", 
     xlab="Anthropomorphic promiscuity")
dev.off()

hist(as.numeric(simdata$sociographic_prudery), 
     main="sociographic_prudery", 
     xlab="sociographic_prudery")
hist(as.numeric(simdata$engagement_social), 
     main="engagement_social", 
     xlab="engagement_social")
hist(as.numeric(simdata$engagement_financial), 
     main="engagement_financial", 
     xlab="engagement_financial")

tiff("Engagement Contagion.tiff", units="in", width=6, height=5, res=300)
hist(as.numeric(simdata$engagement_contagion), 
     main="Engagement contagion", 
     xlab="Engagement contagion")
dev.off()


hist(as.numeric(simdata$engagement_natural), 
     main="engagement_natural", 
     xlab="engagement_natural")
hist(as.numeric(simdata$engagement_predation), 
     main="engagement_predation", 
     xlab="engagement_predation")

# and then for testing 
tiff("religiousfrequencyXnationalism.tiff", units="in", width=6, height=5, res=300)
rel_nat_plot <- ggboxplot(simdata, x = "rel_frequency", y = "nationalism_level", 
                          color = "rel_frequency", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                          order = c(7, 6, 5, 4, 3, 2, 1),
                          ylab = "Nationalism", xlab = "Religious Frequency")
rel_nat_plot + rotate_x_text(45)
dev.off()

rel_nat_aov <- aov(nationalism_level ~ rel_frequency, data = simdata)
summary(rel_nat_aov)
TukeyHSD(rel_nat_aov)



# The code below was for created correlation plots. Tried a few different methods given the
# size of the variable set we were hoping to explore to find one that was interpretable. 

important_data_num <- sapply( simdata, as.numeric )
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

library(stargazer)
## Now run a regression to see how everything contributes

biglm1 <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
              rel_frequency + initial_concern_social + initial_concern_financial + 
              initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
              hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
              hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
              big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
              hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
              hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
              anthropomorphic_promiscuity + sociographic_prudery, data = simdata)
summary(biglm1)

biglm1_highnat <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
               rel_frequency + initial_concern_social + initial_concern_financial + 
               initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
               hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
               hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
               big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
               hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
               hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
               anthropomorphic_promiscuity + sociographic_prudery, data = simdata, nationalism_quartile == 4)
summary(biglm1_highnat)

biglm1_lownat <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
                       rel_frequency + initial_concern_social + initial_concern_financial + 
                       initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
                       hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
                       hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
                       big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
                       hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
                       hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
                       anthropomorphic_promiscuity + sociographic_prudery, data = simdata, nationalism_quartile == 1)
summary(biglm1_lownat)

stargazer(biglm1, biglm1_lownat, biglm1_highnat, title="Simulated Results Regression for Anti-Immigrant Sentiment", align=TRUE,type = "html")


plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$threat_con_fin_nat[simdata$nationalism_quartile==1],
     main = "Anti-Immigrant Sentiment and threat for low nationalism simulations", xlab = "Anti-immigrant sentiment", ylab = "Contagion, financial, natural threat")
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$threat_con_fin_nat[simdata$nationalism_quartile==4],
     main = "Anti-Immigrant Sentiment and threat for high nationalism simulations", xlab = "Anti-immigrant sentiment", ylab = "Contagion, financial, natural threat")

plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==1],simdata$threat_soc_pred[simdata$nationalism_quartile==1],
     main = "Anti-Immigrant Sentiment and threat for low nationalism simulations", xlab = "Anti-immigrant sentiment", ylab = "Social, predation threat")
plot(simdata$anti_immigrant_sentiment[simdata$nationalism_quartile==4],simdata$threat_soc_pred[simdata$nationalism_quartile==4],
     main = "Anti-Immigrant Sentiment and threat for high nationalism simulations", xlab = "Anti-immigrant sentiment", ylab = "Social, predation threat")


biglm1_highrel_soc <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
                       rel_frequency + initial_concern_social + initial_concern_financial + 
                       initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
                       hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
                       hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
                       big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
                       hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
                       hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
                       anthropomorphic_promiscuity + sociographic_prudery, data = simdata, sociographic_prudery_quartile == 4)
summary(biglm1_highrel_soc)

biglm1_lowrel_soc <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
                      rel_frequency + initial_concern_social + initial_concern_financial + 
                      initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
                      hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
                      hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
                      big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
                      hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
                      hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
                      anthropomorphic_promiscuity + sociographic_prudery, data = simdata, sociographic_prudery_quartile == 1)
summary(biglm1_lowrel_soc)

biglm1_highrel_anth <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
                       rel_frequency + initial_concern_social + initial_concern_financial + 
                       initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
                       hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
                       hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
                       big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
                       hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
                       hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
                       anthropomorphic_promiscuity + sociographic_prudery, data = simdata, anthropomorphic_promiscuity_quartile == 4)
summary(biglm1_highrel_anth)

biglm1_lowrel_anth <- lm(anti_immigrant_sentiment ~ tvMediaUse + threatPctOfMedia + socialMediaUse +
                      rel_frequency + initial_concern_social + initial_concern_financial + 
                      initial_concern_contagion + initial_concern_predation + hazard_intensity_contagion +
                      hazard_intensity_financial + hazard_intensity_natural + hazard_intensity_predation + 
                      hazard_intensity_social + habituationRate + energyDecay + big_5_openness + 
                      big_5_conscientiousness + big_5_agreeableness + hazard_event_count_contagion + 
                      hazard_event_count_financial + hazard_event_count_natural + hazard_event_count_predation +
                      hazard_event_count_social + nationalism_level + economic_conservativism + social_conservativism + 
                      anthropomorphic_promiscuity + sociographic_prudery, data = simdata, anthropomorphic_promiscuity_quartile == 1)
summary(biglm1_lowrel_anth)







threatonlylm1 <- lm(anti_immigrant_sentiment ~ engagement_social + engagement_financial + 
                      engagement_contagion + engagement_natural + engagement_predation,
                    data = simdata)
summary(threatonlylm1)
stargazer(threatonlylm1, title="Simulated threat and anti-immigrant relationships", align=TRUE,type = "html")


threatonlylm1_highnat <- lm(anti_immigrant_sentiment ~ engagement_social + engagement_financial + 
                      engagement_contagion + engagement_natural + engagement_predation,
                    data = simdata, nationalism_quartile==4)
summary(threatonlylm1_highnat)

threatonlylm1_lownat <- lm(anti_immigrant_sentiment ~ engagement_social + engagement_financial + 
                      engagement_contagion + engagement_natural + engagement_predation,
                    data = simdata, nationalism_quartile==1)
summary(threatonlylm1_lownat)

threatonlylm1_highrel <- lm(anti_immigrant_sentiment ~ engagement_social + engagement_financial + 
                              engagement_contagion + engagement_natural + engagement_predation,
                            data = simdata, sociographic_prudery_quartile==4)
summary(threatonlylm1_highrel)

threatonlylm1_lowrel <- lm(anti_immigrant_sentiment ~ engagement_social + engagement_financial + 
                             engagement_contagion + engagement_natural + engagement_predation,
                           data = simdata, sociographic_prudery_quartile==1)
summary(threatonlylm1_lowrel)


##########################################################################################
# The QPlots were particularly useful in interpretation it seems. So we made one for many
# variables for validation and visualization and data exploration. Most weren't that interesting
# but they were easily interpretable 

qplot(economic_conservativism,nationalism_level,data=simdata, geom='bin2d')
qplot(economic_conservativism,social_conservativism,data=simdata, geom='bin2d')
qplot(tvMediaUse,socialMediaUse,data=simdata, geom='bin2d')
qplot(tvMediaUse,economic_conservativism,data=simdata, geom='bin2d')
qplot(socialMediaUse,economic_conservativism,data=simdata, geom='bin2d')

qplot(anti_immigrant_sentiment,economic_conservativism,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,social_conservativism,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,socialMediaUse,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,tvMediaUse,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,nationalism_level,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,threat_soc_pred,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,threat_con_fin_nat,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,engagement_social,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,engagement_financial,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,engagement_contagion,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,engagement_natural,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,engagement_predation,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_event_count_contagion,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_event_count_financial,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_event_count_natural,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_event_count_predation,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_event_count_social,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_intensity_contagion,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_intensity_financial,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_intensity_natural,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_intensity_predation,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,hazard_intensity_social,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,initial_concern_contagion,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,initial_concern_financial,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,initial_concern_natural,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,initial_concern_predation,data=simdata, geom='bin2d')
qplot(anti_immigrant_sentiment,initial_concern_social,data=simdata, geom='bin2d')

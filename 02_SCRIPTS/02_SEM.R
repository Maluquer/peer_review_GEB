##########################################
##########################################
####SCRIPT SEM MODELS
####TOP-DOWN PROCESSES MODULATE BIODIVERSITY-FUNCTIONING RELATIONSHIPS ACROSS NORTH AMERICAN FORESTS


#In this script we analyse develop an structural equation model
#using piecewiseSEM to analyse how past climate change velocities
#current climate conditions (annual precipitation and mean temperatures)
#as well as number of individuals influence species richness stand level
#basal area relationships across north america.
#we use data from the national forest inventories of Mexico, USA, Canada and Quebec
#We reproduce the analyses for all north america and for different forest
#types across the continent

#rm(list = ls()) clear environment
library(tidyverse)
library(piecewiseSEM)#using the developing version to compute standardize effects for negbinom models http://jslefche.github.io/piecewiseSEM/
library(DHARMa)
library(lme4)
library(MASS)

#import data
All_NFI <- read_csv("01_DATA/DATA_NFI_NA.csv")
summary(All_NFI)
All_NFI <- All_NFI[, -1]
length(unique(All_NFI$PLOT))

sem_nfi2_glm2 <- psem(glm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z  + Nind + log(area), family = "gaussian", All_NFI),
                      glm.nb(Nind ~ BIO1.z + BIO12.z  + log(area), All_NFI),
                      glm(logmDIA95.z ~ BIO1.z + BIO12.z + log(area), family = "gaussian", All_NFI),
                      glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z + tmpvel.z + log(area), family = "poisson", All_NFI, na.action = na.omit),
                      logmDIA95.z %~~%  Nind)

sem2_glm2 <- summary(sem_nfi2_glm2, conserve = TRUE, standardize.type = "Menard.OE")
sem2_glm2

ind.claims <- sem2_glm2[[3]]
results <-sem2_glm2[[7]]
r2 <-  sem2_glm2[[8]]
chi2 <- sem2_glm2[[4]]
fisher <- sem2_glm2[[5]]
aic <- sem2_glm2[[6]]

write.csv(r2, "03_RESULTS/r2_general_glm_plot.csv")
write.csv(results, "03_RESULTS/results_general_glm_plot.csv")
write.csv(ind.claims, "03_RESULTS/ind.claims_general_glm_plot.csv")
write.csv(chi2, "03_RESULTS/chi2_general_glm_plot.csv")
write.csv(aic, "03_RESULTS/aic_general_glm_plot.csv")
write.csv(fisher, "03_RESULTS/fisher_general_glm_plot.csv")

rm(sem2_glm2)
rm(ind.claims) 
rm(results) 
rm(r2) 
rm(chi2) 
rm(fisher) 
rm(aic)
rm(sem_nfi2_glm2)

##############################################################
#Here we separate by forest types and perform SEM at this level
##############################################################

Temperate_cont <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Temperate continental forest")
subt_humid_fot <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Subtropical humid forest")
subt_mount_sys <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Subtropical mountain system")
Temperate_mout <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Temperate mountain system")
Tropical_moist <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Tropical moist forest")
Boreal_conif_f <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Boreal coniferous forest")
rm(All_NFI)

row.names(Temperate_cont) <- NULL
row.names(subt_humid_fot) <- NULL
row.names(subt_mount_sys) <- NULL
row.names(Temperate_mout) <- NULL
row.names(Tropical_moist) <- NULL
row.names(Boreal_conif_f) <- NULL

#temperate continental##########################
sem_tem_con_glm2 <- psem(glm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z  + Nind + log(area), family = "gaussian", Temperate_cont),
                         glm.nb(Nind ~ BIO1.z + BIO12.z  + log(area), Temperate_cont),
                         glm(logmDIA95.z ~ BIO1.z + BIO12.z + log(area), family = "gaussian", Temperate_cont),
                         glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z  + tmpvel.z + log(area), family = "poisson", Temperate_cont, na.action = na.omit),
                         logmDIA95.z %~~%  Nind) 

semTC <- summary(sem_tem_con_glm2)
semTC

ind.claimsTC <- semTC[[3]]
resultsTC <-semTC[[7]]
r2TC <- semTC[[8]]
chi2TC <- semTC[[4]]
fisherTC <- semTC[[5]]
aicTC <- semTC[[6]]

write.csv(r2TC, "03_RESULTS/r2_TC_glm_plot.csv")
write.csv(resultsTC, "03_RESULTS/results_TC_glm_plot.csv")
write.csv(ind.claimsTC, "03_RESULTS/ind.claims_TC_glm_plot.csv")
write.csv(chi2TC, "03_RESULTS/chi2_TC_glm_plot.csv")
write.csv(aicTC, "03_RESULTS/aic_TC_glm_plot.csv")
write.csv(fisherTC, "03_RESULTS/fisher_TC_glm_plot.csv")

rm(semTC)
rm(ind.claimsTC)
rm(resultsTC)
rm(r2TC)
rm(chi2TC)
rm(fisherTC)
rm(aicTC)
rm(sem_tem_con_glm2)


#subtropical mountain##########
sem_sub_mon_glm2 <- psem(glm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z  + Nind + log(area), family = "gaussian", subt_mount_sys),
                         glm.nb(Nind ~ BIO1.z + BIO12.z  + log(area), subt_mount_sys),
                         glm(logmDIA95.z ~ BIO1.z + BIO12.z + log(area), family = "gaussian", subt_mount_sys),
                         glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z  + tmpvel.z + log(area), family = "poisson", subt_mount_sys, na.action = na.omit),
                         logmDIA95.z %~~%  Nind)


semSM <- summary(sem_sub_mon_glm2)
semSM 

ind.claimsSM <- semSM[[3]]
resultsSM <-semSM[[7]]
r2SM <- semSM[[8]]
chi2SM <- semSM[[4]]
fisherSM <- semSM[[5]]
aicSM <- semSM[[6]]

write.csv(r2SM, "03_RESULTS/r2_SM_glm_plot.csv")
write.csv(resultsSM, "03_RESULTS/results_SM_glm_plot.csv")
write.csv(ind.claimsSM, "03_RESULTS/ind.claims_SM_glm_plot.csv")
write.csv(chi2SM, "03_RESULTS/chi2_SM_glm_plot.csv")
write.csv(aicSM, "03_RESULTS/aic_SM_glm_plot.csv")
write.csv(fisherSM, "03_RESULTS/fisher_SM_glm_plot.csv")

rm(semSM)
rm(ind.claimsSM)
rm(resultsSM)
rm(r2SM)
rm(chi2SM)
rm(fisherSM)
rm(aicSM)
rm(sem_sub_mon_glm2)


#temperate mountain##############################
sem_tem_mon_glm2 <- psem(glm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z + Nind + log(area), family = "gaussian", Temperate_mout),
                         glm.nb(Nind ~ BIO1.z + BIO12.z  + log(area), Temperate_mout),
                         glm(logmDIA95.z ~ BIO1.z + BIO12.z +  log(area), family = "gaussian", Temperate_mout),
                         glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z  + tmpvel.z + log(area), family = "poisson", Temperate_mout, na.action = na.omit),
                         logmDIA95.z %~~%  Nind)

semTM <- summary(sem_tem_mon_glm2)
semTM

ind.claimsTM <- semTM[[3]]
resultsTM <-semTM[[7]]
r2TM <- semTM[[8]]
chi2TM <- semTM[[4]]
fisherTM <- semTM[[5]]
aicTM <- semTM[[6]]

write.csv(r2TM, "03_RESULTS/r2_TM_glm_plot.csv")
write.csv(resultsTM, "03_RESULTS/results_TM_glm_plot.csv")
write.csv(ind.claimsTM, "03_RESULTS/ind.claims_TM_glm_plot.csv")
write.csv(chi2TM, "03_RESULTS/chi2_TM_glm_plot.csv")
write.csv(aicTM, "03_RESULTS/aic_TM_glm_plot.csv")
write.csv(fisherTM, "03_RESULTS/fisher_TM_glm_plot.csv")

rm(semTM)
rm(ind.claimsTM)
rm(resultsTM)
rm(r2TM)
rm(chi2TM)
rm(fisherTM)
rm(aicTM)
rm(sem_tem_mon_glm2)


#tropical moist################
sem_tro_moi_glm2 <- psem(glm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z  + Nind + log(area), family = "gaussian", Tropical_moist),
                         glm.nb(Nind ~ BIO1.z + BIO12.z + log(area), Tropical_moist),
                         glm(logmDIA95.z ~ BIO1.z + BIO12.z +  log(area), family = "gaussian", Tropical_moist),
                         glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z + tmpvel.z + log(area), family = "poisson", Tropical_moist, na.action = na.omit),
                         logmDIA95.z %~~%  Nind)


semTP <- summary(sem_tro_moi_glm2)
semTP 

ind.claimsTP <- semTP[[3]]
resultsTP <-semTP[[7]]
r2TP <- semTP[[8]]
chi2TP <- semTP[[4]]
fisherTP <- semTP[[5]]
aicTP <- semTP[[6]]

write.csv(r2TP, "03_RESULTS/r2_TP_glm_plot.csv")
write.csv(resultsTP, "03_RESULTS/results_TP_glm_plot.csv")
write.csv(ind.claimsTP, "03_RESULTS/ind.claims_TP_glm_plot.csv")
write.csv(chi2TP, "03_RESULTS/chi2_TP_glm_plot.csv")
write.csv(aicTP, "03_RESULTS/aic_TP_glm_plot.csv")
write.csv(fisherTP, "03_RESULTS/fisher_TP_glm_plot.csv")

rm(semTP)
rm(ind.claimsTP)
rm(resultsTP)
rm(r2TP)
rm(chi2TP)
rm(fisherTP)
rm(aicTP)
rm(sem_tro_moi_glm2)


##boreal##########################
sem_bor_con_glm2 <- psem(lm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z + Nind + log(area), Boreal_conif_f),
                         glm.nb(Nind ~ BIO1.z + BIO12.z  + log(area), Boreal_conif_f),
                         lm(logmDIA95.z ~ BIO1.z + BIO12.z +  log(area), Boreal_conif_f),
                         glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z + tmpvel.z + log(area), family = "poisson", Boreal_conif_f, na.action = na.omit),
                         logmDIA95.z %~~%  Nind)

semBC <- summary(sem_bor_con_glm2)
semBC

ind.claimsBC <- semBC[[3]]
resultsBC <-semBC[[7]]
r2BC <- semBC[[8]]
chi2BC <- semBC[[4]]
fisherBC <- semBC[[5]]
aicBC <- semBC[[6]]

write.csv(r2BC, "03_RESULTS/r2_BC_glm_plot.csv")
write.csv(resultsBC, "03_RESULTS/results_BC_glm_plot.csv")
write.csv(ind.claimsBC, "03_RESULTS/ind.claims_BC_glm_plot.csv")
write.csv(chi2BC, "03_RESULTS/chi2_BC_glm_plot.csv")
write.csv(aicBC, "03_RESULTS/aic_BC_glm_plot.csv")
write.csv(fisherBC, "03_RESULTS/fisher_BC_glm_plot.csv")

rm(semBC)
rm(ind.claimsBC)
rm(resultsBC)
rm(r2BC)
rm(chi2BC)
rm(fisherBC)
rm(aicBC)
rm(sem_bor_con_glm2)


#subtropical humid#################
sub_hum_fot_glm2 <-psem(glm(logab ~ BIO1.z + BIO12.z + spR + logmDIA95.z + Nind,family = "gaussian", subt_humid_fot),
                        glm.nb(Nind ~ BIO1.z + BIO12.z, subt_humid_fot),
                        glm(logmDIA95.z ~ BIO1.z + BIO12.z, family = "gaussian", subt_humid_fot),
                        glm(spR ~ BIO1.z + BIO12.z + Nind + logmDIA95.z + tmpvel.z, family = "poisson", subt_humid_fot, na.action = na.omit),
                        logmDIA95.z %~~%  Nind) 

semSH <- summary(sub_hum_fot_glm2)
semSH

ind.claimsSH <- semSH[[3]]
resultsSH <-semSH[[7]]
r2SH <- semSH[[8]]
chi2SH <- semSH[[4]]
fisherSH <- semSH[[5]]
aicSH <- semSH[[6]]

write.csv(r2SH, "03_RESULTS/r2_SH_glm_plot.csv")
write.csv(resultsSH, "03_RESULTS/results_SH_glm_plot.csv")
write.csv(ind.claimsSH, "03_RESULTS/ind.claims_SH_glm_plot.csv")
write.csv(chi2SH, "03_RESULTS/chi2_SH_glm_plot.csv")
write.csv(aicSH, "03_RESULTS/aic_SH_glm_plot.csv")
write.csv(fisherSH, "03_RESULTS/fisher_SH_glm_plot.csv")

rm(semSH)
rm(ind.claimsSH)
rm(resultsSH)
rm(r2SH)
rm(chi2SH)
rm(fisherSH)
rm(aicSH)
rm(sub_hum_fot_glm2)


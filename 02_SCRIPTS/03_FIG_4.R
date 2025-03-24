##########################################
##SCRIPT FIG 4
#TOP-DOWN PROCESSES MODULATE BIODIVERSITY-FUNCTIONING RELATIONSHIPS ACROSS NORTH AMERICAN FORESTS

#In this script we plot the std.estimates of the relationships
#species richness and climate change velocities
#species richness and temperature
#species richness and annual precipitation
#species richness and number of individuals
#stand level basal area and species richness


#call libraries  
library(tidyverse)
library(patchwork)
library(ggdist)

################
####lower panels
################

#call data, results from script SEM.R (forest type)
semBC <- read.csv("03_RESULTS/results_BC_glm_plot.csv")
semTC <- read.csv("03_RESULTS/results_TC_glm_plot.csv")
semTM <- read.csv("03_RESULTS/results_TM_glm_plot.csv")
semSH <- read.csv("03_RESULTS/results_SH_glm_plot.csv")
semSM <- read.csv("03_RESULTS/results_SM_glm_plot.csv")
semTP <- read.csv("03_RESULTS/results_TP_glm_plot.csv")

semBC <- semBC |> mutate(f_type = rep("1.BC", dim(semBC)[1]))
semTM <- semTM |> mutate(f_type = rep("2.TM", dim(semTM)[1]))
semTC <- semTC |> mutate(f_type = rep("3.TC", dim(semTC)[1]))
semSH <- semSH |> mutate(f_type = rep("4.SH", dim(semSH)[1]))
semSM <- semSM |> mutate(f_type = rep("5.SM", dim(semSM)[1]))
semTP <- semTP |> mutate(f_type = rep("6.TH", dim(semTP)[1]))

results <- rbind(semBC, semTC, semTM, semSH, semSM, semTP)

results <- results |> unite("relation", Response, Predictor, sep = "_", remove = FALSE, na.rm = FALSE)
tmpvel <- subset(results, relation == "spR_tmpvel.z")
MAT <- subset(results, relation == "spR_BIO1.z")
MAP <- subset(results, relation == "spR_BIO12.z")
MIH <- subset(results, relation == "spR_Nind")
spR <- subset(results, relation == "logab_spR")

p1 <- ggplot(data = tmpvel, aes(x = f_type, y = Std.Estimate)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "Darkgreen", alpha = 0.5) +
  ggtitle("(A)") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() + 
  theme(title = element_text(size = 20)) +
  coord_flip(ylim = c(-0.3, 0.5)) + 
  theme(axis.text.x = element_text(face="bold",  size = 16),
        axis.text.y = element_blank())

p2 <- ggplot(data = MAT, aes(x = f_type, y = Std.Estimate)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "Darkblue", alpha = 0.5) +
  ggtitle("(B)") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() + 
  theme(title = element_text(size = 20)) +
  coord_flip(ylim = c(-0.3, 0.5)) + 
  theme(axis.text.x = element_text(face = "bold",  size = 16),
        axis.text.y = element_blank())

p3 <- ggplot(data = MAP, aes(x = f_type, y = Std.Estimate)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "Darkblue", alpha = 0.5) +
  ggtitle("(C)") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() + 
  theme(title = element_text(size = 20)) +
  coord_flip(ylim = c(-0.3, 0.5)) + 
  theme(axis.text.x = element_text(face = "bold",  size = 16),
        axis.text.y = element_blank())

p4 <- ggplot(data = MIH, aes(x = f_type, y = Std.Estimate)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "goldenrod1", alpha = 0.5) +
  ggtitle("(D)") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() + 
  theme(title = element_text(size = 20)) +
  coord_flip(ylim = c(-0.3, 0.5)) + 
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_blank())

p5 <- ggplot(data = spR, aes(x = f_type, y = Std.Estimate)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "red", alpha = 0.5) +
  ggtitle("(E)") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, size = 2) +
  theme_bw() + 
  theme(title = element_text(size = 20)) +
  coord_flip(ylim = c(-0.3, 0.5)) + 
  theme(axis.text.x = element_text(face="bold", size = 16),
        axis.text.y = element_blank())

fig4a <- (p1 | p2 | p3 | p4 | p5)



###############
###Lower panels
###############

#call data
All_NFI <- read.csv("01_DATA/DATA_NFI_NA.csv")
summary(All_NFI)
All_NFI <- All_NFI[, -1]
length(unique(All_NFI$PLOT))

Temperate_cont <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Temperate continental forest")
subt_humid_fot <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Subtropical humid forest")
subt_mount_sys <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Subtropical mountain system")
Temperate_mout <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Temperate mountain system")
Tropical_moist <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Tropical moist forest")
Boreal_conif_f <- subset(All_NFI, All_NFI$EZMAP_ZONE == "Boreal coniferous forest")
rm(All_NFI)

Boreal_conif_f <- Boreal_conif_f |> mutate(f_type = rep("1.BC", dim(Boreal_conif_f)[1]))
Temperate_mout <- Temperate_mout |> mutate(f_type = rep("2.TM", dim(Temperate_mout)[1]))
Temperate_cont <- Temperate_cont |> mutate(f_type = rep("3.TC", dim(Temperate_cont)[1]))
subt_humid_fot <- subt_humid_fot |> mutate(f_type = rep("4.SH", dim(subt_humid_fot)[1]))
subt_mount_sys <- subt_mount_sys |> mutate(f_type = rep("5.SM", dim(subt_mount_sys)[1]))
Tropical_moist <- Tropical_moist |> mutate(f_type = rep("6.TH", dim(Tropical_moist)[1]))

row.names(Temperate_cont) <- NULL
row.names(subt_humid_fot) <- NULL
row.names(subt_mount_sys) <- NULL
row.names(Temperate_mout) <- NULL
row.names(Tropical_moist) <- NULL
row.names(Boreal_conif_f) <- NULL

F_TYPES <- rbind(Temperate_mout, Temperate_cont, subt_humid_fot, subt_mount_sys, Tropical_moist, Boreal_conif_f)

p1 <- ggplot(F_TYPES, aes(x = f_type, y = tmpvel)) +
  ggtitle("(F)") +
  stat_halfeye(fill = "Darkgreen", alpha = 0.5) +
  theme_bw() +
  coord_flip() + 
  theme(axis.text.x = element_text(face="bold", size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 20))

p2 <- ggplot(F_TYPES, aes(x = f_type, y = BIO1)) +
  ggtitle("(G)") +
  stat_halfeye(fill = "Darkblue", alpha = 0.5) +
  theme_bw() +
  coord_flip() + 
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 20))

p3 <- ggplot(F_TYPES, aes(x = f_type, y = BIO12)) +
  ggtitle("(H)") +
  stat_halfeye(fill = "Darkblue", alpha = 0.5) +
  theme_bw() +
  coord_flip() + 
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 20))

F_TYPES <- F_TYPES |> mutate(stem.dens = Nind / area)
p4 <- ggplot(F_TYPES, aes(x = f_type, y = stem.dens)) +
  ggtitle("(I)") +
  stat_halfeye(fill = "goldenrod1", alpha = 0.5) +
  theme_bw() +
  coord_flip() + 
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.text = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 20))

fig4b <- (p1 | p2 | p3 | p4 | plot_spacer())

fig4ab <- fig4a /
  fig4b

ggsave(
  plot = fig4ab,
  "03_RESULTS/fig4.png",
  width = 15, height = 8,
  dpi = 600
  )

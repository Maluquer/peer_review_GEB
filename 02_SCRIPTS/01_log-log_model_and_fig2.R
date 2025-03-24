##########################################
##SCRIPT MODELS LOG-LOG + FIG. 2
#TOP-DOWN PROCESSES MODULATE BIODIVERSITY-FUNCTIONING RELATIONSHIPS ACROSS NORTH AMERICAN FORESTS

#In this script we perform models evaluating the relationship between
#log(stand level basal area) and log(richness)
#to look for the commonly found positive monotonic relationship.
#furthermore we include four more models were we test the interaction of species richness
#with climate chnage velocities, mat and map and number of individuals
#finally using the ggefect package we plot this results
#this plots are the panels included in fig 2 of the manuscript

#clear workspace and call libraries
library(tidyverse)
library(lme4)
library(patchwork)
library(sjPlot)
library(ggResidpanel)
library(MASS)
library(lmerTest)
library(ggeffects)

## import data
All_NFI <- read_csv("01_DATA/DATA_NFI_NA.csv")
summary(All_NFI)
All_NFI <- All_NFI[, -1]
length(unique(All_NFI$PLOT))

#models
#Models of logAB logSpr 
#-model general
#-model temperature change velocities (h1)
#-model interaccion clima (h2)
#-model nind (h3)

#####all
BEF2 <- glm(logab ~ log(spR) + area, family = "gaussian", All_NFI) #glm

summary(BEF2)
resid_panel(BEF2)

#####tmpchange velocities H3
BEF3 <- glm(logab ~ log(spR) * tmpvel.z + area, family = "gaussian", All_NFI)

summary(BEF3)
resid_panel(BEF3)

#current clima H2
BEF4 <- glm(logab ~ log(spR) * (BIO1 + BIO12) + area, family = "gaussian", All_NFI)

summary(BEF4)
resid_panel(BEF4)

#Nindividuals H3
BEF5 <- glm(logab ~ log(spR) * Nind + area, family = "gaussian", All_NFI)

summary(BEF5)
resid_panel(BEF5)

unique(All_NFI$area) # plot areas

me_df_usa <- ggpredict(BEF2,
                       terms = c("spR [all]"),
                       condition = c(area = 0.0668)) #USA
me_df_mex <- ggpredict(BEF2,
                       terms = c("spR [all]"),
                       condition = c(area = 0.16)) #mexico
me_df_que <- ggpredict(BEF2,
                       terms = c("spR [all]"),
                       condition = c(area = 0.04)) # quebec

me_df_usa
me_df_mex

#PANEL PRINCIPALlA DE LA FIGURA 
fig2 <-  ggplot() +
  geom_point(data = All_NFI,
             aes(x = spR, y = logab, color = NFI),
             alpha = 0.1) +
  geom_line(data = me_df_usa, aes(x, predicted)) +
  geom_ribbon(data = me_df_usa,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.5) +
  ggtitle("BEF relationship (Basal Area ~ Species Richness)") +
  xlab("Species Richness") +
  ylab("log(Plot Basal Area)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(1, 37))  + guides(color = guide_legend(override.aes = list(size = 8)))
ggMarginal(fig2, type = "histogram")

fig2


#OTHER PANELS
me_df_tcv <- ggpredict(BEF3, terms = c("spR [all]", "tmpvel.z"), condition = c(area = 0.0668)) #USA 
me_df_tmp <- ggpredict(BEF4, terms = c("spR [all]", "BIO1"), condition = c(area = 0.0668)) #USA 
me_df_ind <- ggpredict(BEF5, terms = c("spR [all]", "Nind"), condition = c(area = 0.0668)) #USA 
me_df_pre <- ggpredict(BEF4, terms = c("spR [all]", "BIO12"), condition = c(area = 0.0668)) #USA 

me_df_tcv
me_df_tmp
me_df_ind

me_df_ind <- ggpredict(BEF5, terms = c("spR [all]", "Nind"), condition = c(area = 0.0668)) #USA 

#PLOTS
plot(me_df_tcv, show_ci = TRUE, show_data = TRUE,  colors = c("darkgreen", "olivedrab", "green"), dot_alpha = 0.05, jitter = 0.15) +
  labs(
    x = "Species Richness",
    y = "log (BA)",
    title = "H1: Past Climate Instability"
  )  + 
  labs(colour = "Tcv")

plot(me_df_tmp, show_ci = TRUE, show_data = TRUE, colors = c("navy", "steelblue", "lightblue"), dot_alpha = 0.05, jitter = 0.15) +
  labs(
    x = "Species Richness",
    y = "log (BA)",
    title = "H2: Current Climate (MAT)"
  ) + 
  labs(colour = "MAT")

plot(me_df_pre, show_ci = TRUE, show_data = TRUE, colors = c("navy", "steelblue", "lightblue"), dot_alpha = 0.05, jitter = 0.15) +
  labs(
    x = "Species Richness",
    y = "log (BA)",
    title = "H2: Current Climate (MAP)"
  ) + 
  labs(colour = "MAP")

plot(me_df_ind, show_ci = TRUE, show_data = TRUE, colors = c("orange4", "yellow3", "khaki"), dot_alpha = 0.05, jitter = 0.15)+
  labs(
    x = "Species Richness",
    y = "log (BA)",
    title = "H3: Climate - driven abundance"
  ) 

# Christopher Jenney
# Fossil Creek Data Analysis
#2021-05-08

# Logistic Regression with all variables included. 
dir.create("LOGIT_5_9")

data <- read.csv("FishAboveandBelow.csv")
data$Above <- factor(data$Above)
data$Location <-factor(data$Location)
data$Velo <- abs(data$Velo)
data$InstreamCover <- data$InstreamCover/100
library(ggplot2)
library(dplyr)
#Roundtail Chub
#Make a dataset of just RTC
Roundtail.Data.Abund <- data %>% 
  select(Location, Above, Depth, Velo, Substrate,
         PercentCover, InstreamCover, MacroHab, RoundtailChub.Abund) %>% 
  filter(RoundtailChub.Abund > 0)


#Run the GLM with the full model
library(MASS)
RTC_glm.nb <- glm.nb(RoundtailChub.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Roundtail.Data.Abund)
summary(RTC_glm.nb)
confint(RTC_glm.nb)

# Create a GLM Prediction Figure


Cov1_pred_data_1 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Roundtail.Data.Abund$Depth),max(Roundtail.Data.Abund$Depth),
                                           length.out=30))

Cov1_pred_data_1$Above<-factor(Cov1_pred_data_1$Above)

Cov1_pred_data_1$Velo <- mean(Roundtail.Data.Abund$Velo)

Cov1_pred_data_1$Substrate <- mean(Roundtail.Data.Abund$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Roundtail.Data.Abund$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Roundtail.Data.Abund$InstreamCover)


Preds_Cov1_1 <- predict(RTC_glm.nb, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale



Cov1_pred_data_1$Pred <-exp(Preds_Cov1_1$fit)


plt <- ggplot(data = Cov1_pred_data_1, aes(x = Depth, y = Pred, 
                                           color = Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Number of Fish") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <- plot_depth + theme_bw() + ggtitle("Roundtail Chub")   


## RTC Flow Velocity
#
#
#

RTC_glm.nb <- glm.nb(RoundtailChub.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Roundtail.Data.Abund)

# Create a GLM Prediction Figure

Cov1_pred_data_2 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Roundtail.Data.Abund$Velo),max(Roundtail.Data.Abund$Velo),
                                          length.out=30))

Cov1_pred_data_2$Above<-factor(Cov1_pred_data_2$Above)

Cov1_pred_data_2$Depth <- mean(Roundtail.Data.Abund$Depth)

Cov1_pred_data_2$Substrate <- mean(Roundtail.Data.Abund$Substrate)

Cov1_pred_data_2$PercentCover <- mean(Roundtail.Data.Abund$PercentCover)

Cov1_pred_data_2$InstreamCover <- mean(Roundtail.Data.Abund$InstreamCover)


Preds_Cov1_2 <- predict(RTC_glm.nb, newdata=Cov1_pred_data_2,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_2$Pred <-exp(Preds_Cov1_2$fit)


Cov1_pred_data_2$Covariate <- "Velo"


plt_vel <- ggplot(data = Cov1_pred_data_2, aes(x = Velo, y = Pred, 
                                               color = Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_velocity <- plt_vel + xlab("Flow Velocity (m^3/s") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_velocity <-plt_velocity + theme_bw()+ ggtitle("Roundtail Chub") 


#
#
###Substrate
RTC_glm.nb <- glm.nb(RoundtailChub.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Roundtail.Data.Abund)
# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Roundtail.Data.Abund$Substrate),max(Roundtail.Data.Abund$Substrate),
                                               length.out=30))


Cov1_pred_data_3$Above<-factor(Cov1_pred_data_3$Above)

Cov1_pred_data_3$Depth <- mean(Roundtail.Data.Abund$Depth)

Cov1_pred_data_3$Velo <- mean(Roundtail.Data.Abund$Velo)

Cov1_pred_data_3$PercentCover <- mean(Roundtail.Data.Abund$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Roundtail.Data.Abund$InstreamCover)


Preds_Cov1_3 <- predict(RTC_glm.nb, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-exp(Preds_Cov1_3$fit)



plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Substrate, y = Pred, 
                                               color = Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) +
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_substrate <- plt_substrate + theme_bw()  + ggtitle("Roundtail Chub") 





###
## Percent Overhead Cover
RTC_glm.nb <- glm.nb(RoundtailChub.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Roundtail.Data.Abund)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Roundtail.Data.Abund$PercentCover),max(Roundtail.Data.Abund$PercentCover),
                                                  length.out=30))

Cov1_pred_data_4$Above<-factor(Cov1_pred_data_4$Above)


Cov1_pred_data_4$Depth <- mean(Roundtail.Data.Abund$Depth)

Cov1_pred_data_4$Velo <- mean(Roundtail.Data.Abund$Velo)

Cov1_pred_data_4$Substrate <- mean(Roundtail.Data.Abund$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Roundtail.Data.Abund$InstreamCover)


Preds_Cov1_4 <- predict(RTC_glm.nb, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-exp(Preds_Cov1_4$fit)

plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = PercentCover, y = Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw()  + ggtitle("Roundtail Chub") 



###
#Plot Instream Cover
RTC_glm.nb <- glm.nb(RoundtailChub.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Roundtail.Data.Abund)
# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Roundtail.Data.Abund$InstreamCover),max(Roundtail.Data.Abund$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Above<-factor(Cov1_pred_data_5$Above)

Cov1_pred_data_5$Depth <- mean(Roundtail.Data.Abund$Depth)

Cov1_pred_data_5$Velo <- mean(Roundtail.Data.Abund$Velo)

Cov1_pred_data_5$Substrate <- mean(Roundtail.Data.Abund$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Roundtail.Data.Abund$PercentCover)


Preds_Cov1_5 <- predict(RTC_glm.nb, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-exp(Preds_Cov1_5$fit)


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = InstreamCover,y= Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <- plt_instream + theme_bw()+ ggtitle("Roundtail Chub")



library(ggpubr)
Roundtail_Plots <- ggarrange(plot_depth, plt_velocity, plt_substrate, plt_cover,plt_instream,
                             nrow = 3, ncol = 2)
Roundtail_Plots + ggtitle("Roundtail Chub")

#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/RTC_GLM_FULL.txt")
summary(RTC_GLM)
confint(RTC_GLM)
sink()


## DESERT SUCKER ANALYSIS
###YOU MADE IT!
## I THINK IT LOOKS..... OKAY

#Desert Sucker
#Make a dataset of just D.S.
Desert.Data.Abund <- data %>% 
  select(Location, Above, Depth, Velo, Substrate,
         PercentCover, InstreamCover, MacroHab, Desert.Sucker.Abund) %>% 
  filter(Desert.Sucker.Abund > 0)


#Run the GLM with the full model
library(MASS)
DS_glm.nb <- glm.nb(Desert.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Desert.Data.Abund)
summary(DS_glm.nb)
confint(DS_glm.nb)

# Create a GLM Prediction Figure


Cov1_pred_data_1 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Desert.Data.Abund$Depth),max(Desert.Data.Abund$Depth),
                                           length.out=30))

Cov1_pred_data_1$Above<-factor(Cov1_pred_data_1$Above)

Cov1_pred_data_1$Velo <- mean(Desert.Data.Abund$Velo)

Cov1_pred_data_1$Substrate <- mean(Desert.Data.Abund$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Desert.Data.Abund$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Desert.Data.Abund$InstreamCover)


Preds_Cov1_1 <- predict(DS_glm.nb, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale



Cov1_pred_data_1$Pred <-exp(Preds_Cov1_1$fit)



plt <- ggplot(data = Cov1_pred_data_1, aes(x = Depth, y = Pred, 
                                           color = Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <- plot_depth + theme_bw() + ggtitle("Desert Sucker")  


## Desert Flow Velocity
#
#
#


DS_glm.nb <- glm.nb(Desert.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab,
                    data = Desert.Data.Abund)
summary(DS_glm.nb)
confint(DS_glm.nb)

# Create a GLM Prediction Figure


Cov1_pred_data_6 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Desert.Data.Abund$Velo),max(Desert.Data.Abund$Velo),
                                           length.out=30))

Cov1_pred_data_6$Above<-factor(Cov1_pred_data_6$Above)

Cov1_pred_data_6$Depth <- mean(Desert.Data.Abund$Depth)

Cov1_pred_data_6$Substrate <- mean(Desert.Data.Abund$Substrate)

Cov1_pred_data_6$PercentCover <- mean(Desert.Data.Abund$PercentCover)

Cov1_pred_data_6$InstreamCover <- mean(Desert.Data.Abund$InstreamCover)


Preds_Cov1_6 <- predict(DS_glm.nb, newdata=Cov1_pred_data_6,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale



Cov1_pred_data_6$Pred <-exp(Preds_Cov1_6$fit)


plt_v <- ggplot(data = Cov1_pred_data_6, aes(x = Velo, y = Pred, 
                                           color = Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plot_velocity <- plt_v + xlab("Velocity (m^3/s)") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_velocity <- plot_velocity + theme_bw() + ggtitle("Desert Sucker")   


#
#
###Substrate
DS_glm.nb <- glm.nb(Desert.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Desert.Data.Abund)
# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Desert.Data.Abund$Substrate),max(Desert.Data.Abund$Substrate),
                                               length.out=30))


Cov1_pred_data_3$Above<-factor(Cov1_pred_data_3$Above)

Cov1_pred_data_3$Depth <- mean(Desert.Data.Abund$Depth)

Cov1_pred_data_3$Velo <- mean(Desert.Data.Abund$Velo)

Cov1_pred_data_3$PercentCover <- mean(Desert.Data.Abund$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Desert.Data.Abund$InstreamCover)


Preds_Cov1_3 <- predict(DS_glm.nb, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-exp(Preds_Cov1_3$fit)


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Substrate, y = Pred, 
                                               color = Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_substrate <- plt_substrate + theme_bw()  + ggtitle("Desert Sucker") 





###
## Percent Overhead Cover
DS_glm.nb <- glm.nb(Desert.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Desert.Data.Abund)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Desert.Data.Abund$PercentCover),max(Desert.Data.Abund$PercentCover),
                                                  length.out=30))

Cov1_pred_data_4$Above<-factor(Cov1_pred_data_4$Above)


Cov1_pred_data_4$Depth <- mean(Desert.Data.Abund$Depth)

Cov1_pred_data_4$Velo <- mean(Desert.Data.Abund$Velo)

Cov1_pred_data_4$Substrate <- mean(Desert.Data.Abund$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Desert.Data.Abund$InstreamCover)


Preds_Cov1_4 <- predict(DS_glm.nb, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-exp(Preds_Cov1_4$fit)



plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = PercentCover, y = Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw()  + ggtitle("Desert Sucker") 



###
DS_glm.nb <- glm.nb(Desert.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above,
                     data = Desert.Data.Abund)
# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Desert.Data.Abund$InstreamCover),max(Desert.Data.Abund$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Above<-factor(Cov1_pred_data_5$Above)

Cov1_pred_data_5$Depth <- mean(Desert.Data.Abund$Depth)

Cov1_pred_data_5$Velo <- mean(Desert.Data.Abund$Velo)

Cov1_pred_data_5$Substrate <- mean(Desert.Data.Abund$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Desert.Data.Abund$PercentCover)


Preds_Cov1_5 <- predict(DS_glm.nb, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-exp(Preds_Cov1_5$fit)


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = InstreamCover, y = Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <- plt_instream + theme_bw()+ ggtitle("Desert Sucker")



library(ggpubr)
Desert_Plots <- ggarrange(plot_depth, plot_velocity, plt_substrate, plt_cover,plt_instream,
                             nrow = 3, ncol = 2)
Desert_Plots + ggtitle("Desert Sucker")



#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/Desert_GLM_FULL.txt")
summary(Desert_GLM)
confint(Desert_GLM)
sink()
#
#
#
#
####
#
# Sonora Sucker----
Sonora.Data.Abund <- data %>% 
  select(Location, Above, Depth, Velo, Substrate,
         PercentCover, InstreamCover, MacroHab, Sonora.Sucker.Abund) %>% 
  filter(Sonora.Sucker.Abund > 0)
#Run the GLM with the full model
library(MASS)
Sonora.glm.nb <- glm.nb(Sonora.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above
                        , data = Sonora.Data.Abund)
summary(Sonora.glm.nb)
confint(Sonora.glm.nb)


# Create a GLM Prediction Figure


Cov1_pred_data_1 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Sonora.Data.Abund$Depth),max(Sonora.Data.Abund$Depth),
                                           length.out=30))

Cov1_pred_data_1$Above<-factor(Cov1_pred_data_1$Above)

Cov1_pred_data_1$Velo <- mean(Sonora.Data.Abund$Velo)

Cov1_pred_data_1$Substrate <- mean(Sonora.Data.Abund$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Sonora.Data.Abund$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Sonora.Data.Abund$InstreamCover)


Preds_Cov1_1 <- predict(Sonora.glm.nb, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale



Cov1_pred_data_1$Pred <-exp(Preds_Cov1_1$fit)



plt <- ggplot(data = Cov1_pred_data_1, aes(x = Depth, y = Pred, 
                                           color = Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <- plot_depth + theme_bw() + ggtitle("Sonora Sucker")  


## Desert Flow Velocity
#
#
#


Sonora.glm.nb <- glm.nb(Sonora.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab,
                    data = Sonora.Data.Abund)
summary(Sonora.glm.nb)
confint(Sonora.glm.nb)

# Create a GLM Prediction Figure


Cov1_pred_data_6 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Sonora.Data.Abund$Velo),max(Sonora.Data.Abund$Velo),
                                          length.out=30))

Cov1_pred_data_6$Above<-factor(Cov1_pred_data_6$Above)

Cov1_pred_data_6$Depth <- mean(Sonora.Data.Abund$Depth)

Cov1_pred_data_6$Substrate <- mean(Sonora.Data.Abund$Substrate)

Cov1_pred_data_6$PercentCover <- mean(Sonora.Data.Abund$PercentCover)

Cov1_pred_data_6$InstreamCover <- mean(Sonora.Data.Abund$InstreamCover)


Preds_Cov1_6 <- predict(Sonora.glm.nb, newdata=Cov1_pred_data_6,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale



Cov1_pred_data_6$Pred <-exp(Preds_Cov1_6$fit)


plt_v <- ggplot(data = Cov1_pred_data_6, aes(x = Velo, y = Pred, 
                                             color = Above,
                                             shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plot_velocity <- plt_v + xlab("Velocity (m^3/s)") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_velocity <- plot_velocity + theme_bw() + ggtitle("Sonora Sucker")   


#
#
###Substrate
Sonora.glm.nb <- glm.nb(Sonora.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab,
                        data = Sonora.Data.Abund)
# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Sonora.Data.Abund$Substrate),max(Sonora.Data.Abund$Substrate),
                                               length.out=30))


Cov1_pred_data_3$Above<-factor(Cov1_pred_data_3$Above)

Cov1_pred_data_3$Depth <- mean(Sonora.Data.Abund$Depth)

Cov1_pred_data_3$Velo <- mean(Sonora.Data.Abund$Velo)

Cov1_pred_data_3$PercentCover <- mean(Sonora.Data.Abund$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Sonora.Data.Abund$InstreamCover)


Preds_Cov1_3 <- predict(Sonora.glm.nb, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-exp(Preds_Cov1_3$fit)


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Substrate, y = Pred, 
                                               color = Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_substrate <- plt_substrate + theme_bw()  + ggtitle("Sonora Sucker") 





###
## Percent Overhead Cover
Sonora.glm.nb <- glm.nb(Sonora.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab,
                        data = Sonora.Data.Abund)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Sonora.Data.Abund$PercentCover),max(Sonora.Data.Abund$PercentCover),
                                                  length.out=30))

Cov1_pred_data_4$Above<-factor(Cov1_pred_data_4$Above)


Cov1_pred_data_4$Depth <- mean(Sonora.Data.Abund$Depth)

Cov1_pred_data_4$Velo <- mean(Sonora.Data.Abund$Velo)

Cov1_pred_data_4$Substrate <- mean(Sonora.Data.Abund$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Sonora.Data.Abund$InstreamCover)


Preds_Cov1_4 <- predict(Sonora.glm.nb, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-exp(Preds_Cov1_4$fit)



plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = PercentCover, y = Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw()  + ggtitle("Sonora Sucker") 



###
Sonora.glm.nb <- glm.nb(Sonora.Sucker.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab,
                        data = Sonora.Data.Abund)
# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Sonora.Data.Abund$InstreamCover),max(Sonora.Data.Abund$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Above<-factor(Cov1_pred_data_5$Above)

Cov1_pred_data_5$Depth <- mean(Sonora.Data.Abund$Depth)

Cov1_pred_data_5$Velo <- mean(Sonora.Data.Abund$Velo)

Cov1_pred_data_5$Substrate <- mean(Sonora.Data.Abund$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Sonora.Data.Abund$PercentCover)


Preds_Cov1_5 <- predict(Sonora.glm.nb, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-exp(Preds_Cov1_5$fit)


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = InstreamCover, y = Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Fish Counts") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <- plt_instream + theme_bw()+ ggtitle("Sonora Sucker")



library(ggpubr)
Sonora_Plots <- ggarrange(plot_depth, plot_velocity, plt_substrate, plt_cover,plt_instream,
                          nrow = 3, ncol = 2)
Sonora_Plots




#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/Sonora.data_GLM_FULL.txt")
summary(Sonora.data_GLM)
confint(Sonora.data_GLM)
sink()



###
###
### Bass ----
#Black Bass
Bass.Data.Abund <- data %>% 
  select(Location,Depth, Velo, Substrate,
         PercentCover, InstreamCover, MacroHab, Bass.Abund) %>% 
  filter(Bass.Abund > 0)

#Run the GLM with the full model
library(MASS)
Bass_glm.nb <- glm.nb(Bass.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab , 
                      data = Bass.Data.Abund)
summary(Bass_glm.nb)
confint(Bass_glm.nb)

# Create a GLM Prediction Figure

## Sonora Sucker Depth

Cov1_pred_data_1 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Bass.Data.Abund$Depth),max(Bass.Data.Abund$Depth),
                                           length.out=30))


Cov1_pred_data_1$Velo <- mean(Bass.Data.Abund$Velo)

Cov1_pred_data_1$Substrate <- mean(Bass.Data.Abund$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Bass.Data.Abund$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Bass.Data.Abund$InstreamCover)


Preds_Cov1_1 <- predict(Bass_glm.nb, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale


Cov1_pred_data_1$Pred <-exp(Preds_Cov1_1$fit)


plt <- ggplot(data = Cov1_pred_data_1, aes(x = Depth, y = Pred,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <-  plot_depth + theme_bw() + ggtitle("Black Bass")



### Black Bass Velocity



Bass_glm.nb <- glm.nb(Bass.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab , 
                      data = Bass.Data.Abund)
summary(Bass_glm.nb)
confint(Bass_glm.nb)
# Create a GLM Prediction Figure

Cov1_pred_data_2 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Bass.Data.Abund$Velo),max(Bass.Data.Abund$Velo),
                                          length.out=30))


Cov1_pred_data_2$Depth <- mean(Bass.Data.Abund$Depth)

Cov1_pred_data_2$Substrate <- mean(Bass.Data.Abund$Substrate)

Cov1_pred_data_2$PercentCover <- mean(Bass.Data.Abund$PercentCover)

Cov1_pred_data_2$InstreamCover <- mean(Bass.Data.Abund$InstreamCover)


Preds_Cov1_2 <- predict(Bass_glm.nb, newdata=Cov1_pred_data_2,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_2$Pred <-exp(Preds_Cov1_2$fit)


plt_vel <- ggplot(data = Cov1_pred_data_2, aes(x = Velo, y = Pred,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_velocity <- plt_vel + xlab("Flow Velocity (m^3/s") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_velocity <- plt_velocity + theme_bw() + ggtitle("Black Bass")
plt_velocity

# Black Bass
# SUbstrate
###Substrate
Bass_glm.nb <- glm.nb(Bass.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab , 
                      data = Bass.Data.Abund)
summary(Bass_glm.nb)
confint(Bass_glm.nb)
# Create a GLM Prediction Figure

Cov1_pred_data_3 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Bass.Data.Abund$Substrate),max(Bass.Data.Abund$Substrate),
                                          length.out=30))


Cov1_pred_data_3$Depth <- mean(Bass.Data.Abund$Depth)

Cov1_pred_data_3$Velo <- mean(Bass.Data.Abund$Velo)

Cov1_pred_data_3$PercentCover <- mean(Bass.Data.Abund$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Bass.Data.Abund$InstreamCover)


Preds_Cov1_3 <- predict(Bass_glm.nb, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-exp(Preds_Cov1_3$fit)


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Substrate, y = Pred,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_substrate <- plt_sub + xlab("Substrate") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_substrate <- plt_substrate + theme_bw() + ggtitle("Black Bass")
plt_substrate
##
#
##
##
###
## Percent Overhead Cover
Bass_glm.nb <- glm.nb(Bass.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab , 
                      data = Bass.Data.Abund)
summary(Bass_glm.nb)
confint(Bass_glm.nb)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Bass.Data.Abund$PercentCover),max(Bass.Data.Abund$PercentCover),
                                                  length.out=30))


Cov1_pred_data_4$Depth <- mean(Bass.Data.Abund$Depth)

Cov1_pred_data_4$Velo <- mean(Bass.Data.Abund$Velo)

Cov1_pred_data_4$Substrate <- mean(Bass.Data.Abund$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Bass.Data.Abund$InstreamCover)


Preds_Cov1_4 <- predict(Bass_glm.nb, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-exp(Preds_Cov1_4$fit)





plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = PercentCover, y = Pred,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Fish Count") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw() + ggtitle ("Black Bass") 

plt_cover



###
#Plot Instream Cover
Bass_glm.nb <- glm.nb(Bass.Abund ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab , 
                      data = Bass.Data.Abund)

# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Bass.Data.Abund$InstreamCover),max(Bass.Data.Abund$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Depth <- mean(Bass.Data.Abund$Depth)

Cov1_pred_data_5$Velo <- mean(Bass.Data.Abund$Velo)

Cov1_pred_data_5$Substrate <- mean(Bass.Data.Abund$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Bass.Data.Abund$PercentCover)


Preds_Cov1_5 <- predict(Bass_glm.nb, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-exp(Preds_Cov1_5$fit)



plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = InstreamCover, y = Pred,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "poisson"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Fish Counts") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <-plt_instream + theme_bw() + ggtitle("Black Bass") 



library(ggpubr)
Bass_Plots <- ggarrange(plot_depth, plt_velocity, plt_substrate, plt_cover,plt_instream,
                        nrow = 3, ncol = 2)
Bass_Plots


#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/Bass_GLM_FULL.txt")
summary(Bass_GLM)
confint(Bass_GLM)
sink()

#Speckled Dace
Speckled <- data %>% 
  filter(SpeckedDace == 1) %>% 
  filter(Above == 1) %>% 
  select(Location, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, SpeckedDace)
#make an available dataset
available<-data %>% 
  filter(Species == "Available") %>% 
  filter(Above == 1) %>% 
  select(Location, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, SpeckedDace)
available<-na.omit(available)

#combine the two datasets
Speckled.data <- rbind(Speckled, available)

#Run the GLM with the full model
Specked_GLM <- glm(SpeckedDace ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab 
                   , family = "binomial", data = Speckled.data)
summary(Specked_GLM)
confint(Specked_GLM)

#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/Speckled_GLM_FULL.txt")
summary(Specked_GLM)
confint(Specked_GLM)
sink()

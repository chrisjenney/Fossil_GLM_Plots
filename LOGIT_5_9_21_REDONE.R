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
Roundtail <- data %>% 
  filter(RoundtailChub == 1) %>% 
  select(Location, Above, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, RoundtailChub)
#make an available dataset
available<-data %>% 
  filter(Species == "Available") %>% 
  select(Location, Above, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, RoundtailChub)
available<-na.omit(available)

#combine the two datasets
Roundtail.data <- rbind(Roundtail, available)
na.omit(Roundtail.data)
#Run the GLM with the full model
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above
               + Above*Depth + Above*Velo + Above*Substrate + Above*PercentCover + Above*InstreamCover +
                 Above*MacroHab, family = "binomial", data = Roundtail.data)
summary(RTC_GLM)
confint(RTC_GLM)


RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure


Cov1_pred_data_1 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Roundtail.data$Depth),max(Roundtail.data$Depth),
                                           length.out=30))

Cov1_pred_data_1$Above<-factor(Cov1_pred_data_1$Above)

Cov1_pred_data_1$Velo <- mean(Roundtail.data$Velo)

Cov1_pred_data_1$Substrate <- mean(Roundtail.data$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Roundtail.data$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Roundtail.data$InstreamCover)


Preds_Cov1_1 <- predict(RTC_GLM, newdata=Cov1_pred_data_1,
                      
                      type='link',se.fit=T) # To get the SE’s on the link scale


Cov1_pred_data_1$Pred <-plogis(Preds_Cov1_1$fit)

Cov1_pred_data_1$LCI <- plogis(Preds_Cov1_1$fit-1.96*Preds_Cov1_1$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_1$UCI <- plogis(Preds_Cov1_1$fit+1.96*Preds_Cov1_1$se)

Cov1_pred_data_1$Covariate <- "Depth"



plt <- ggplot(data = Cov1_pred_data_1, aes(x = Cov1_pred_data_1$Depth, y = Cov1_pred_data_1$Pred, 
                                           color = Cov1_pred_data_1$Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <- plot_depth + theme_bw() + ggtitle("Roundtail Chub") + ylim(0, 1.0)   


## RTC Flow Velocity
#
#
#

RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure

Cov1_pred_data_2 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Roundtail.data$Velo),max(Roundtail.data$Velo),
                                           length.out=30))

Cov1_pred_data_2$Above<-factor(Cov1_pred_data_2$Above)

Cov1_pred_data_2$Depth <- mean(Roundtail.data$Depth)

Cov1_pred_data_2$Substrate <- mean(Roundtail.data$Substrate)

Cov1_pred_data_2$PercentCover <- mean(Roundtail.data$PercentCover)

Cov1_pred_data_2$InstreamCover <- mean(Roundtail.data$InstreamCover)


Preds_Cov1_2 <- predict(RTC_GLM, newdata=Cov1_pred_data_2,
                      
                      type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_2$Pred <-plogis(Preds_Cov1_2$fit)

Cov1_pred_data_2$LCI <- plogis(Preds_Cov1_2$fit-1.96*Preds_Cov1_2$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_2$UCI <- plogis(Preds_Cov1_2$fit+1.96*Preds_Cov1_2$se)

Cov1_pred_data_2$Covariate <- "Velo"


plt_vel <- ggplot(data = Cov1_pred_data_2, aes(x = Cov1_pred_data_2$Velo, y = Cov1_pred_data_2$Pred, 
                                           color = Cov1_pred_data_2$Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_velocity <- plt_vel + xlab("Flow Velocity (m^3/s") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_velocity <-plt_velocity + theme_bw()+ ggtitle("Roundtail Chub") + ylim(0, 1.0)    


#
#
###Substrate
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + 
                 Above + MacroHab
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Roundtail.data$Substrate),max(Roundtail.data$Substrate),
                                          length.out=30))


Cov1_pred_data_3$Above<-factor(Cov1_pred_data_3$Above)

Cov1_pred_data_3$Depth <- mean(Roundtail.data$Depth)

Cov1_pred_data_3$Velo <- mean(Roundtail.data$Velo)

Cov1_pred_data_3$PercentCover <- mean(Roundtail.data$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Roundtail.data$InstreamCover)


Preds_Cov1_3 <- predict(RTC_GLM, newdata=Cov1_pred_data_3,
                      
                      type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-plogis(Preds_Cov1_3$fit)

Cov1_pred_data_3$LCI <- plogis(Preds_Cov1_3$fit-1.96*Preds_Cov1_3$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_3$UCI <- plogis(Preds_Cov1_3$fit+1.96*Preds_Cov1_3$se)

Cov1_pred_data_3$Covariate <- "Substrate"


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Cov1_pred_data_3$Substrate, y = Cov1_pred_data_3$Pred, 
                                               color = Cov1_pred_data_3$Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_substrate <- plt_substrate + theme_bw()  + ggtitle("Roundtail Chub") + ylim(0, 1.0)





###
## Percent Overhead Cover
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Roundtail.data$PercentCover),max(Roundtail.data$PercentCover),
                                               length.out=30))

Cov1_pred_data_4$Above<-factor(Cov1_pred_data_4$Above)


Cov1_pred_data_4$Depth <- mean(Roundtail.data$Depth)

Cov1_pred_data_4$Velo <- mean(Roundtail.data$Velo)

Cov1_pred_data_4$Substrate <- mean(Roundtail.data$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Roundtail.data$InstreamCover)


Preds_Cov1_4 <- predict(RTC_GLM, newdata=Cov1_pred_data_4,
                      
                      type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-plogis(Preds_Cov1_4$fit)

Cov1_pred_data_4$LCI <- plogis(Preds_Cov1_4$fit-1.96*Preds_Cov1_4$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_4$UCI <- plogis(Preds_Cov1_4$fit+1.96*Preds_Cov1_4$se)

Cov1_pred_data_4$Covariate <- "Percent Cover"




plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = Cov1_pred_data_4$PercentCover, y = Cov1_pred_data_4$Pred, 
                                               color = Cov1_pred_data_4$Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw()  + ggtitle("Roundtail Chub") + ylim(0, 1.0)





###
#Plot Instream Cover
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Roundtail.data$InstreamCover),max(Roundtail.data$InstreamCover),
                                                  length.out=30))

Cov1_pred_data_5$Above<-factor(Cov1_pred_data_5$Above)

Cov1_pred_data_5$Depth <- mean(Roundtail.data$Depth)

Cov1_pred_data_5$Velo <- mean(Roundtail.data$Velo)

Cov1_pred_data_5$Substrate <- mean(Roundtail.data$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Roundtail.data$PercentCover)


Preds_Cov1_5 <- predict(RTC_GLM, newdata=Cov1_pred_data_5,
                      
                      type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-plogis(Preds_Cov1_5$fit)

Cov1_pred_data_5$LCI <- plogis(Preds_Cov1_5$fit-1.96*Preds_Cov1_5$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_5$UCI <- plogis(Preds_Cov1_5$fit+1.96*Preds_Cov1_5$se)

Cov1_pred_data_5$Covariate <- "Instream Cover"


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = Cov1_pred_data_5$InstreamCover, y = Cov1_pred_data_5$Pred, 
                                              color = Cov1_pred_data_5$Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <- plt_instream + theme_bw()+ ggtitle("Roundtail Chub") + ylim(0, 1.0)



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
Desert <- data %>% 
  filter(Desert.Sucker == 1) %>% 
  select(Location, Above, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, Desert.Sucker)
#make an available dataset
available<-data %>% 
  filter(Species == "Available") %>% 
  select(Location, Above, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, Desert.Sucker)
available<-na.omit(available)

#combine the two datasets
Desert.data <- rbind(Desert, available)

#Run the GLM with the full model
Desert_GLM <- glm(Desert.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above
               + Above*Depth + Above*Velo + Above*Substrate + Above*PercentCover + Above*InstreamCover +
                 Above*MacroHab, family = "binomial", data = Desert.data)
summary(Desert_GLM)
confint(Desert_GLM)
###
#####
#####
#Plots for DS

DS_GLM <- glm(Desert.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab
               , family = "binomial", data = Desert.data)

# Create a GLM Prediction Figure

## Desert Sucker Depth

Cov1_pred_data_1 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Desert.data$Depth),max(Desert.data$Depth),
                                           length.out=30))

Cov1_pred_data_1$Above<-factor(Cov1_pred_data_1$Above)

Cov1_pred_data_1$Velo <- mean(Desert.data$Velo)

Cov1_pred_data_1$Substrate <- mean(Desert.data$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Desert.data$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Desert.data$InstreamCover)


Preds_Cov1_1 <- predict(DS_GLM, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale


Cov1_pred_data_1$Pred <-plogis(Preds_Cov1_1$fit)

Cov1_pred_data_1$LCI <- plogis(Preds_Cov1_1$fit-1.96*Preds_Cov1_1$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_1$UCI <- plogis(Preds_Cov1_1$fit+1.96*Preds_Cov1_1$se)

Cov1_pred_data_1$Covariate <- "Depth"



plt <- ggplot(data = Cov1_pred_data_1, aes(x = Cov1_pred_data_1$Depth, y = Cov1_pred_data_1$Pred, 
                                           color = Cov1_pred_data_1$Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <- plot_depth + theme_bw() + ggtitle("Desert Sucker") + ylim(0, 1.0)



### Desert Sucker Velocity



DS_GLM <- glm(Desert.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab
               , family = "binomial", data = Desert.data)

# Create a GLM Prediction Figure

Cov1_pred_data_2 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Desert.data$Velo),max(Desert.data$Velo),
                                          length.out=30))

Cov1_pred_data_2$Above<-factor(Cov1_pred_data_2$Above)

Cov1_pred_data_2$Depth <- mean(Desert.data$Depth)

Cov1_pred_data_2$Substrate <- mean(Desert.data$Substrate)

Cov1_pred_data_2$PercentCover <- mean(Desert.data$PercentCover)

Cov1_pred_data_2$InstreamCover <- mean(Desert.data$InstreamCover)


Preds_Cov1_2 <- predict(DS_GLM, newdata=Cov1_pred_data_2,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_2$Pred <-plogis(Preds_Cov1_2$fit)

Cov1_pred_data_2$LCI <- plogis(Preds_Cov1_2$fit-1.96*Preds_Cov1_2$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_2$UCI <- plogis(Preds_Cov1_2$fit+1.96*Preds_Cov1_2$se)

Cov1_pred_data_2$Covariate <- "Velo"


plt_vel <- ggplot(data = Cov1_pred_data_2, aes(x = Cov1_pred_data_2$Velo, y = Cov1_pred_data_2$Pred, 
                                               color = Cov1_pred_data_2$Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_velocity <- plt_vel + xlab("Flow Velocity (m^3/s") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_velocity <- plt_velocity + theme_bw() + ggtitle("Desert Sucker") + ylim(0, 1.0)


# Desert Sucker
# SUbstrate
###Substrate
DS_GLM <- glm(Desert.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + 
                 Above + MacroHab
               , family = "binomial", data = Desert.data)

# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Desert.data$Substrate),max(Desert.data$Substrate),
                                               length.out=30))


Cov1_pred_data_3$Above<-factor(Cov1_pred_data_3$Above)

Cov1_pred_data_3$Depth <- mean(Desert.data$Depth)

Cov1_pred_data_3$Velo <- mean(Desert.data$Velo)

Cov1_pred_data_3$PercentCover <- mean(Desert.data$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Desert.data$InstreamCover)


Preds_Cov1_3 <- predict(DS_GLM, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-plogis(Preds_Cov1_3$fit)

Cov1_pred_data_3$LCI <- plogis(Preds_Cov1_3$fit-1.96*Preds_Cov1_3$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_3$UCI <- plogis(Preds_Cov1_3$fit+1.96*Preds_Cov1_3$se)

Cov1_pred_data_3$Covariate <- "Substrate"


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Cov1_pred_data_3$Substrate, y = Cov1_pred_data_3$Pred, 
                                               color = Cov1_pred_data_3$Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_substrate <-plt_substrate + theme_bw() + ggtitle ("Desert Sucker") + ylim(0, 1.0)


##
#
##
##
###
## Percent Overhead Cover
DS_GLM <- glm(Desert.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Desert.data)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Desert.data$PercentCover),max(Desert.data$PercentCover),
                                                  length.out=30))

Cov1_pred_data_4$Above<-factor(Cov1_pred_data_4$Above)


Cov1_pred_data_4$Depth <- mean(Desert.data$Depth)

Cov1_pred_data_4$Velo <- mean(Desert.data$Velo)

Cov1_pred_data_4$Substrate <- mean(Desert.data$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Desert.data$InstreamCover)


Preds_Cov1_4 <- predict(DS_GLM, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-plogis(Preds_Cov1_4$fit)

Cov1_pred_data_4$LCI <- plogis(Preds_Cov1_4$fit-1.96*Preds_Cov1_4$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_4$UCI <- plogis(Preds_Cov1_4$fit+1.96*Preds_Cov1_4$se)

Cov1_pred_data_4$Covariate <- "Percent Cover"




plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = Cov1_pred_data_4$PercentCover, y = Cov1_pred_data_4$Pred, 
                                              color = Cov1_pred_data_4$Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <-plt_cover + theme_bw() + ggtitle ("Desert Sucker") + ylim(0, 1.0)





###
#Plot Instream Cover
DS_GLM <- glm(Desert.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Desert.data)

# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Desert.data$InstreamCover),max(Desert.data$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Above<-factor(Cov1_pred_data_5$Above)

Cov1_pred_data_5$Depth <- mean(Desert.data$Depth)

Cov1_pred_data_5$Velo <- mean(Desert.data$Velo)

Cov1_pred_data_5$Substrate <- mean(Desert.data$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Desert.data$PercentCover)


Preds_Cov1_5 <- predict(DS_GLM, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-plogis(Preds_Cov1_5$fit)

Cov1_pred_data_5$LCI <- plogis(Preds_Cov1_5$fit-1.96*Preds_Cov1_5$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_5$UCI <- plogis(Preds_Cov1_5$fit+1.96*Preds_Cov1_5$se)

Cov1_pred_data_5$Covariate <- "Instream Cover"


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = Cov1_pred_data_5$InstreamCover, y = Cov1_pred_data_5$Pred, 
                                              color = Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <-plt_instream + theme_bw() + ggtitle("Desert Sucker") + ylim(0, 1.0)



library(ggpubr)
Desert_Plots <- ggarrange(plot_depth, plt_velocity, plt_substrate, plt_cover,plt_instream,
                             nrow = 3, ncol = 2)
Desert_Plots



#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/Desert_GLM_FULL.txt")
summary(Desert_GLM)
confint(Desert_GLM)
sink()

# Sonora Sucker
Sonora <- data %>% 
  filter(Sonora.Sucker == 1) %>% 
  select(Location, Above, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, Sonora.Sucker)
#make an available dataset
available<-data %>% 
  filter(Species == "Available") %>% 
  select(Location, Above, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, Sonora.Sucker)
available<-na.omit(available)

#combine the two datasets
Sonora.data <- rbind(Sonora, available)

#Run the GLM with the full model
Sonora.data_GLM <- glm(Sonora.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab + Above
               + Above*Depth + Above*Velo + Above*Substrate + Above*PercentCover + Above*InstreamCover +
                 Above*MacroHab, family = "binomial", data = Sonora.data)
summary(Sonora.data_GLM)
confint(Sonora.data_GLM)


###
#####
#####
#Plots for SS
# SS Depth
SS_GLM <- glm(Sonora.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab
              , family = "binomial", data = Sonora.data)

# Create a GLM Prediction Figure

## Sonora Sucker Depth

Cov1_pred_data_1 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Sonora.data$Depth),max(Sonora.data$Depth),
                                           length.out=30))

Cov1_pred_data_1$Above<-factor(Cov1_pred_data_1$Above)

Cov1_pred_data_1$Velo <- mean(Sonora.data$Velo)

Cov1_pred_data_1$Substrate <- mean(Sonora.data$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Sonora.data$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Sonora.data$InstreamCover)


Preds_Cov1_1 <- predict(SS_GLM, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale


Cov1_pred_data_1$Pred <-plogis(Preds_Cov1_1$fit)

Cov1_pred_data_1$LCI <- plogis(Preds_Cov1_1$fit-1.96*Preds_Cov1_1$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_1$UCI <- plogis(Preds_Cov1_1$fit+1.96*Preds_Cov1_1$se)

Cov1_pred_data_1$Covariate <- "Depth"



plt <- ggplot(data = Cov1_pred_data_1, aes(x = Cov1_pred_data_1$Depth, y = Cov1_pred_data_1$Pred, 
                                           color = Cov1_pred_data_1$Above,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <-  plot_depth + theme_bw() + ggtitle("Sonora Sucker") + ylim(0, 1.0)



### Sonora Sucker Velocity



SS_GLM <- glm(Sonora.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above + MacroHab
              , family = "binomial", data = Sonora.data)

# Create a GLM Prediction Figure

Cov1_pred_data_2 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Sonora.data$Velo),max(Sonora.data$Velo),
                                          length.out=30))

Cov1_pred_data_2$Above<-factor(Cov1_pred_data_2$Above)

Cov1_pred_data_2$Depth <- mean(Sonora.data$Depth)

Cov1_pred_data_2$Substrate <- mean(Sonora.data$Substrate)

Cov1_pred_data_2$PercentCover <- mean(Sonora.data$PercentCover)

Cov1_pred_data_2$InstreamCover <- mean(Sonora.data$InstreamCover)


Preds_Cov1_2 <- predict(SS_GLM, newdata=Cov1_pred_data_2,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_2$Pred <-plogis(Preds_Cov1_2$fit)

Cov1_pred_data_2$LCI <- plogis(Preds_Cov1_2$fit-1.96*Preds_Cov1_2$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_2$UCI <- plogis(Preds_Cov1_2$fit+1.96*Preds_Cov1_2$se)

Cov1_pred_data_2$Covariate <- "Velo"


plt_vel <- ggplot(data = Cov1_pred_data_2, aes(x = Cov1_pred_data_2$Velo, y = Cov1_pred_data_2$Pred, 
                                               color = Cov1_pred_data_2$Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_velocity <- plt_vel + xlab("Flow Velocity (m^3/s") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_velocity <- plt_velocity + theme_bw() + ggtitle("Sonora Sucker") + ylim(0, 1.0)


# Desert Sucker
# SUbstrate
###Substrate
SS_GLM <- glm(Sonora.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + 
                Above + MacroHab
              , family = "binomial", data = Sonora.data)

# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Sonora.data$Substrate),max(Sonora.data$Substrate),
                                               length.out=30))


Cov1_pred_data_3$Above<-factor(Cov1_pred_data_3$Above)

Cov1_pred_data_3$Depth <- mean(Sonora.data$Depth)

Cov1_pred_data_3$Velo <- mean(Sonora.data$Velo)

Cov1_pred_data_3$PercentCover <- mean(Sonora.data$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Sonora.data$InstreamCover)


Preds_Cov1_3 <- predict(SS_GLM, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-plogis(Preds_Cov1_3$fit)

Cov1_pred_data_3$LCI <- plogis(Preds_Cov1_3$fit-1.96*Preds_Cov1_3$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_3$UCI <- plogis(Preds_Cov1_3$fit+1.96*Preds_Cov1_3$se)

Cov1_pred_data_3$Covariate <- "Substrate"


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Cov1_pred_data_3$Substrate, y = Cov1_pred_data_3$Pred, 
                                               color = Cov1_pred_data_3$Above,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))


plt_substrate <-plt_substrate + theme_bw() + ggtitle ("Sonora Sucker") + ylim(0, 1.0)


##
#
##
##
###
## Percent Overhead Cover
SS_GLM <- glm(Sonora.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
              , family = "binomial", data = Sonora.data)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Sonora.data$PercentCover),max(Sonora.data$PercentCover),
                                                  length.out=30))

Cov1_pred_data_4$Above<-factor(Cov1_pred_data_4$Above)


Cov1_pred_data_4$Depth <- mean(Sonora.data$Depth)

Cov1_pred_data_4$Velo <- mean(Sonora.data$Velo)

Cov1_pred_data_4$Substrate <- mean(Sonora.data$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Sonora.data$InstreamCover)


Preds_Cov1_4 <- predict(SS_GLM, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-plogis(Preds_Cov1_4$fit)

Cov1_pred_data_4$LCI <- plogis(Preds_Cov1_4$fit-1.96*Preds_Cov1_4$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_4$UCI <- plogis(Preds_Cov1_4$fit+1.96*Preds_Cov1_4$se)

Cov1_pred_data_4$Covariate <- "Percent Cover"




plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = Cov1_pred_data_4$PercentCover, y = Cov1_pred_data_4$Pred, 
                                              color = Cov1_pred_data_4$Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw() + ggtitle ("Sonora Sucker") + ylim(0, 1.0)





###
#Plot Instream Cover
SS_GLM <- glm(Sonora.Sucker ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
              , family = "binomial", data = Sonora.data)

# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(Above=c(0,1),MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Sonora.data$InstreamCover),max(Sonora.data$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Above<-factor(Cov1_pred_data_5$Above)

Cov1_pred_data_5$Depth <- mean(Sonora.data$Depth)

Cov1_pred_data_5$Velo <- mean(Sonora.data$Velo)

Cov1_pred_data_5$Substrate <- mean(Sonora.data$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Sonora.data$PercentCover)


Preds_Cov1_5 <- predict(SS_GLM, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-plogis(Preds_Cov1_5$fit)

Cov1_pred_data_5$LCI <- plogis(Preds_Cov1_5$fit-1.96*Preds_Cov1_5$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_5$UCI <- plogis(Preds_Cov1_5$fit+1.96*Preds_Cov1_5$se)

Cov1_pred_data_5$Covariate <- "Instream Cover"


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = Cov1_pred_data_5$InstreamCover, y = Cov1_pred_data_5$Pred, 
                                              color = Cov1_pred_data_5$Above,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <-plt_instream + theme_bw() + ggtitle("Sonora Sucker") + ylim(0, 1.0)



library(ggpubr)
Sonora_Plots <- ggarrange(plot_depth, plt_velocity, plt_substrate, plt_cover,plt_instream,
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
Bass <- data %>% 
  filter(Bass == 1) %>% 
  filter(Above == 0) %>% 
  select(Location, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, Bass)
#make an available dataset
available<-data %>% 
  filter(Species == "Available") %>% 
  filter(Above == 0) %>% 
  select(Location, Depth, Velo, Substrate, PercentCover, InstreamCover, MacroHab, Bass)
available<-na.omit(available)

#combine the two datasets
Bass.data <- rbind(Bass, available)

#Run the GLM with the full model
Bass_GLM <- glm(Bass ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab ,
                family = "binomial", data = Bass.data)
summary(Bass_GLM)
confint(Bass_GLM)

# Create a GLM Prediction Figure

## Sonora Sucker Depth

Cov1_pred_data_1 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                Depth= seq(min(Bass.data$Depth),max(Bass.data$Depth),
                                           length.out=30))


Cov1_pred_data_1$Velo <- mean(Bass.data$Velo)

Cov1_pred_data_1$Substrate <- mean(Bass.data$Substrate)

Cov1_pred_data_1$PercentCover <- mean(Bass.data$PercentCover)

Cov1_pred_data_1$InstreamCover <- mean(Bass.data$InstreamCover)


Preds_Cov1_1 <- predict(Bass_GLM, newdata=Cov1_pred_data_1,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale


Cov1_pred_data_1$Pred <-plogis(Preds_Cov1_1$fit)

Cov1_pred_data_1$LCI <- plogis(Preds_Cov1_1$fit-1.96*Preds_Cov1_1$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_1$UCI <- plogis(Preds_Cov1_1$fit+1.96*Preds_Cov1_1$se)

Cov1_pred_data_1$Covariate <- "Depth"



plt <- ggplot(data = Cov1_pred_data_1, aes(x = Cov1_pred_data_1$Depth, y = Cov1_pred_data_1$Pred,
                                           shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth <-  plot_depth + theme_bw() + ggtitle("Black Bass") + ylim(0, 1.0)



### Black Bass Velocity



Bass_GLM <- glm(Bass ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab ,
                 family = "binomial", data = Bass.data)

# Create a GLM Prediction Figure

Cov1_pred_data_2 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                Velo= seq(min(Bass.data$Velo),max(Bass.data$Velo),
                                          length.out=30))


Cov1_pred_data_2$Depth <- mean(Bass.data$Depth)

Cov1_pred_data_2$Substrate <- mean(Bass.data$Substrate)

Cov1_pred_data_2$PercentCover <- mean(Bass.data$PercentCover)

Cov1_pred_data_2$InstreamCover <- mean(Bass.data$InstreamCover)


Preds_Cov1_2 <- predict(Bass_GLM, newdata=Cov1_pred_data_2,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_2$Pred <-plogis(Preds_Cov1_2$fit)

Cov1_pred_data_2$LCI <- plogis(Preds_Cov1_2$fit-1.96*Preds_Cov1_2$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_2$UCI <- plogis(Preds_Cov1_2$fit+1.96*Preds_Cov1_2$se)

Cov1_pred_data_2$Covariate <- "Velo"


plt_vel <- ggplot(data = Cov1_pred_data_2, aes(x = Cov1_pred_data_2$Velo, y = Cov1_pred_data_2$Pred,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_velocity <- plt_vel + xlab("Flow Velocity (m^3/s") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_velocity <- plt_velocity + theme_bw() + ggtitle("Black Bass") + ylim(0, 1.0)
plt_velocity

# Black Bass
# SUbstrate
###Substrate
Bass_GLM <- glm(Bass ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab ,
                family = "binomial", data = Bass.data)

# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                Substrate= seq(min(Bass.data$Substrate),max(Bass.data$Substrate),
                                               length.out=30))




Cov1_pred_data_3$Depth <- mean(Bass.data$Depth)

Cov1_pred_data_3$Velo <- mean(Bass.data$Velo)

Cov1_pred_data_3$PercentCover <- mean(Bass.data$PercentCover)

Cov1_pred_data_3$InstreamCover <- mean(Bass.data$InstreamCover)


Preds_Cov1_3 <- predict(Bass_GLM, newdata=Cov1_pred_data_3,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_3$Pred <-plogis(Preds_Cov1_3$fit)

Cov1_pred_data_3$LCI <- plogis(Preds_Cov1_3$fit-1.96*Preds_Cov1_3$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_3$UCI <- plogis(Preds_Cov1_3$fit+1.96*Preds_Cov1_3$se)

Cov1_pred_data_3$Covariate <- "Substrate"


plt_sub <- ggplot(data = Cov1_pred_data_3, aes(x = Cov1_pred_data_3$Substrate, y = Cov1_pred_data_3$Pred,
                                               shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))


plt_substrate <- plt_sub + xlab("Substrate Composition") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))


plt_substrate <-plt_substrate + theme_bw() + ggtitle ("Black Bass") + ylim(0, 1.0)
plt_substrate

##
#
##
##
###
## Percent Overhead Cover
Bass_GLM <- glm(Bass ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab ,
                family = "binomial", data = Bass.data)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                PercentCover= seq(min(Bass.data$PercentCover),max(Bass.data$PercentCover),
                                                  length.out=30))


Cov1_pred_data_4$Depth <- mean(Bass.data$Depth)

Cov1_pred_data_4$Velo <- mean(Bass.data$Velo)

Cov1_pred_data_4$Substrate <- mean(Bass.data$Substrate)

Cov1_pred_data_4$InstreamCover <- mean(Bass.data$InstreamCover)


Preds_Cov1_4 <- predict(Bass_GLM, newdata=Cov1_pred_data_4,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_4$Pred <-plogis(Preds_Cov1_4$fit)

Cov1_pred_data_4$LCI <- plogis(Preds_Cov1_4$fit-1.96*Preds_Cov1_4$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_4$UCI <- plogis(Preds_Cov1_4$fit+1.96*Preds_Cov1_4$se)

Cov1_pred_data_4$Covariate <- "Percent Cover"




plt_cv <- ggplot(data = Cov1_pred_data_4, aes(x = Cov1_pred_data_4$PercentCover, y = Cov1_pred_data_4$Pred,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_cover <- plt_cv + xlab("Percent Canopy Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_cover <- plt_cover + theme_bw() + ggtitle ("Black Bass") + ylim(0, 1.0)

plt_cover



###
#Plot Instream Cover
Bass_GLM <- glm(Bass ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab ,
                family = "binomial", data = Bass.data)

# Create a GLM Prediction Figure

Cov1_pred_data_5 <- expand.grid(MacroHab=c("pool","riffle", "run"),
                                InstreamCover= seq(min(Bass.data$InstreamCover),max(Bass.data$InstreamCover),
                                                   length.out=30))

Cov1_pred_data_5$Depth <- mean(Bass.data$Depth)

Cov1_pred_data_5$Velo <- mean(Bass.data$Velo)

Cov1_pred_data_5$Substrate <- mean(Bass.data$Substrate)

Cov1_pred_data_5$PercentCover <- mean(Bass.data$PercentCover)


Preds_Cov1_5 <- predict(Bass_GLM, newdata=Cov1_pred_data_5,
                        
                        type='link',se.fit=T) # To get the SE’s on the link scale

Cov1_pred_data_5$Pred <-plogis(Preds_Cov1_5$fit)

Cov1_pred_data_5$LCI <- plogis(Preds_Cov1_5$fit-1.96*Preds_Cov1_5$se) # Not sure if “se” is where the SE’s are in Preds_Cov1

Cov1_pred_data_5$UCI <- plogis(Preds_Cov1_5$fit+1.96*Preds_Cov1_5$se)

Cov1_pred_data_5$Covariate <- "Instream Cover"


plt_is <- ggplot(data = Cov1_pred_data_5, aes(x = Cov1_pred_data_5$InstreamCover, y = Cov1_pred_data_5$Pred,
                                              shape = MacroHab)) + 
  geom_point(size = 2.5) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))



plt_instream <- plt_is + xlab("Instream Cover") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plt_instream <-plt_instream + theme_bw() + ggtitle("Black Bass") + ylim(0, 1.0)



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
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


RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_1 <- expand.grid(Depth= seq(min(Roundtail.data$Depth),max(Roundtail.data$Depth),length.out=100),
                              
                              Above=c(0,1))

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


plt <- ggplot(Cov1_pred_data_1, aes(Cov1_pred_data_1$Depth, Cov1_pred_data_1$Pred, col = Cov1_pred_data_1$Above)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, 
              method.args = list(family = "binomial"))
plot_depth <- plt + xlab("Depth(m)") + ylab("Predicted Probs. From GLM") + scale_color_discrete(name = "Bass", labels = c("present", "not present"))

plot_depth       


## RTC Flow Velocity
#
#
#

RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_2 <- expand.grid(Velo= seq(min(Roundtail.data$Velo),max(Roundtail.data$Velo),length.out=100),
                              
                              Above=c(0,1))

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


plt_velo <- ggplot(Cov1_pred_data_2, aes(Cov1_pred_data_2$Velo, Cov1_pred_data_2$Pred, col = Cov1_pred_data_2$Above)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, 
              method.args = list(family = "binomial"))
plot_velo <- plt_velo + scale_color_discrete(name = "Bass", labels = c("present", "not present")) +
  xlab("Velocity (m^3/s)") + ylab("Predicted Probs. From GLM")
plot_velo
#
#
###Substrate
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_3 <- expand.grid(Substrate= seq(min(Roundtail.data$Substrate),max(Roundtail.data$Substrate),length.out=100),
                              
                              Above=c(0,1))

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


plt_sub <- ggplot(Cov1_pred_data_3, aes(Cov1_pred_data_3$Substrate, Cov1_pred_data_3$Pred, col = Cov1_pred_data_3$Above)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, 
              method.args = list(family = "binomial"))
plot_sub <- plt_sub  + scale_color_discrete(name = "Bass", labels = c("present", "not present"))+
  xlab("Substrate Composition") + ylab("Predicted Probs. From GLM")
plot_sub
###
## Percent Overhead Cover
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_4 <- expand.grid(PercentCover= seq(min(Roundtail.data$PercentCover),max(Roundtail.data$PercentCover),length.out=100),
                              
                              Above=c(0,1))

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


plt_cover <- ggplot(Cov1_pred_data_4, aes(Cov1_pred_data_4$PercentCover, Cov1_pred_data_4$Pred, col = Cov1_pred_data_4$Above)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, 
              method.args = list(family = "binomial"))
plot_cover <- plt_cover + scale_color_discrete(name = "Bass", labels = c("present", "not present"))+
  xlab("Percent Overhead Cover") + ylab("Predicted Probs. From GLM")
plot_cover
###
#Plot Instream Cover
RTC_GLM <- glm(RoundtailChub ~ Depth + Velo + Substrate + PercentCover + InstreamCover + Above
               , family = "binomial", data = Roundtail.data)

# Create a GLM Prediction Figure
Cov1_pred_data_5 <- expand.grid(InstreamCover= seq(min(Roundtail.data$InstreamCover),max(Roundtail.data$InstreamCover),length.out=100),
                              
                              Above=c(0,1))

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


plt_instreamcover <- ggplot(Cov1_pred_data_5, aes(Cov1_pred_data_5$InstreamCover, Cov1_pred_data_5$Pred, col = Cov1_pred_data_5$Above)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = T, 
              method.args = list(family = "binomial"))
Plot_Instream <- plt_instreamcover + scale_color_discrete(name = "Bass", labels = c("present", "not present"))+
  xlab("Percent Instream") + ylab("Predicted Probs. From GLM")
Plot_Instream

library(ggpubr)
Roundtail_Plots <- ggarrange(plot_depth, plot_velo, plot_sub, plot_cover,Plot_Instream,
                        nrow = 3, ncol = 2)
Roundtail_Plots

#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/RTC_GLM_FULL.txt")
summary(RTC_GLM)
confint(RTC_GLM)
sink()




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

#sink the file into whereever the fuck i put it. 
sink(file = "LOGIT_5_9/Sonora.data_GLM_FULL.txt")
summary(Sonora.data_GLM)
confint(Sonora.data_GLM)
sink()

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
Bass_GLM <- glm(Bass ~ Depth + Velo + Substrate + PercentCover + InstreamCover + MacroHab , family = "binomial", data = Bass.data)
summary(Bass_GLM)
confint(Bass_GLM)

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


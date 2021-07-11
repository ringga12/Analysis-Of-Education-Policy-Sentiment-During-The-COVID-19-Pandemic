


#-- Set Data---33
analisis3 <- within(read.csv("Sentimen.csv"), {
  Page.category <- as.factor(Page.category)
  Type <- as.factor(Type)
  Content <- as.factor(Content)
})


#--- Library --##
library(readr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library (car)
library(ggpubr)

##---Desriptive statistik-----#


analisis3 <- within(read.csv("Official full.csv"), {
  Page.category <- as.factor(Page.category)
})





analisis3 <- within(read.csv("Official positif.csv"), {
  Page.category <- as.factor(Page.category)
})

summary(analisis3)


analisis1 <- within(read.csv("Official negatif.csv"), {
  Page.category <- as.factor(Page.category)
})

analisis2 <- within(read.csv("portal full.csv"), {
  Page.category <- as.factor(Page.category)
  Type <- as.factor(Type)
})

analisis3 <- within(read.csv("Portal positif.csv"), {
  Page.category <- as.factor(Page.category)
  Type <- as.factor(Type)
})

analisis4 <- within(read.csv("Portal negatif.csv"), {
  Page.category <- as.factor(Page.category)
  Type <- as.factor(Type)
})

summary(analisis1)
summary(analisis2)
summary(analisis3)
summary(analisis4)

#_-- TWO WAY MANOVA---####
DV = cbind(analisis3$Total.Interactions, analisis3$Overperforming.Score , analisis3$Positive, analisis3$Negative)
output = lm(DV ~ Page.category*Type, data = analisis3, contrasts = list(Page.category= contr.sum, Type =contr.sum))
manova_out = Manova(output, type = "III")
summary(manova_out,multivariate = T)


#--- Post hoc test page category---#

aov4 <- aov(analisis3$Positive ~ analisis3$Page.category)
TukeyHSD(aov4)

aov4 <- aov(analisis3$Total.Interactions ~ analisis3$Page.category)
TukeyHSD(aov4)


aov4 <- aov(analisis3$Overperforming.Score ~ analisis3$Page.category)
TukeyHSD(aov4)

aov4 <- aov(analisis3$Negative ~ analisis3$Page.category)
TukeyHSD(aov4)


#--- Post hoc test Type---#
ov4 <- aov(analisis3$Positive ~ analisis3$Type)
TukeyHSD(aov4)

aov4 <- aov(analisis3$Total.Interactions ~ analisis3$Type)
TukeyHSD(aov4)


aov4 <- aov(analisis3$Overperforming.Score ~ analisis3$Type)
TukeyHSD(aov4)

aov4 <- aov(analisis3$Negative ~ analisis3$Type)
TukeyHSD(aov4)


#---- check means ---###

##get the means
tapply(analisis3$Total.Interactions, list(analisis3$Page.category), mean)
tapply(noout$ESTEEM, list(noout$FEM), sd)

tapply(analisis3$Overperforming.Score, list(analisis3$Page.category), mean)

tapply(analisis3$Positive, list(analisis3$Page.category), mean)

tapply(analisis3$Negative, list(analisis3$Page.category), mean)

tapply(analisis3$Total.Interactions, list(analisis3$Type), mean)
tapply(analisis3$Overperforming.Score, list(analisis3$Type), mean)
tapply(analisis3$Positive, list(analisis3$Type), mean)
tapply(analisis3$Negative, list(analisis3$Type), mean)


#---- Check visual plot for Link + TYPE----#
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))
#bargraph = ggplot(analisis3, aes(Page.category, Overperforming.Score, fill = Type))
#bargraph = ggplot(analisis3, aes(Page.category, Positive , fill = Type))
#bargraph = ggplot(analisis3, aes(Page.category, Negative, fill = Type))
bargraph = ggplot(analisis3, aes(Page.category, Total.Interactions, fill = Type))
bargraph +
  stat_summary(fun = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Page category") +
  ylab("Total interaction") + 
  scale_fill_manual(name="Type", 
                    values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                               "#660099", "#CC0066", "#FF9999", "#FF9900", 
                               "black", "black", "black", "black", "black"))

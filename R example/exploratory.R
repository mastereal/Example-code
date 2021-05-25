#Use psych library PCA
library(psych)

df <- read.csv("Data1.csv", header = T)
dfNoMiss <- na.omit(df)

dfLikert <- dfNoMiss[6:ncol(dfNoMiss)]
#kmo test for dataset
KMO(dfLikert)

fit <- princomp(dfLikert)
#According to the scree plot, we should retain 2 PCs

screeplot(fit, type = "line")
#Let's try parallel analysis

library(paran)
paran(dfLikert)
#Parallel analysis says 2. let's say we keep 2.


fit <- pca(dfLikert, 2)
fit
#Loadings > 0.4 load into a component

#Let's get the scores
fit$scores[,1]
fit$scores[,2]

dfNoMiss$RC1 <- fit$scores[,1]
dfNoMiss$RC2 <- fit$scores[,2]


# Now we have two more continuous variables
# save the used dataset
write.csv(dfNoMiss ,'exploratory.csv')

#ANOVA on RC2 and RC1 for instructor and difficulty
fita1<-aov(RC2~as.factor(instr),data=dfNoMiss)
summary(fita1)
fita2<-aov(RC2~as.factor(difficulty),data=dfNoMiss)
summary(fita2)
fitb1<-aov(RC1~as.factor(instr),data=dfNoMiss)
summary(fitb1)
fitb2<-aov(RC2~as.factor(difficulty),data=dfNoMiss)
summary(fitb2)

#tukey test
TukeyHSD(fita1)
TukeyHSD(fita2)
TukeyHSD(fitb1)
TukeyHSD(fitb2)

#plot
library(sjPlot)
fita1_plot<- lm(RC2~as.factor(instr),data=dfNoMiss)
plot_model(fita1_plot,type="pred")
fita2_plot<- lm(RC2~as.factor(difficulty),data=dfNoMiss)
plot_model(fita2_plot,type="pred")
fitb1_plot<- lm(RC1~as.factor(instr),data=dfNoMiss)
plot_model(fitb1_plot,type="pred")
fitb2_plot<- lm(RC1~as.factor(difficulty),data=dfNoMiss)
plot_model(fitb2_plot,type="pred")



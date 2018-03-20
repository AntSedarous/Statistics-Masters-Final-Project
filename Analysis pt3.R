### Libraries
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(car)
library(dyn)
library(lme4)
library(nlme)
library(pROC)
lg <- function(x)c(NA, x[1:(length(x)-1)])
asTheme <- theme(
  panel.background = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5)
)
### Data Prep ###
ds <- read.csv("~/Downloads/R-Friendly Study Data/AppendedDataSet.csv")
dsPt2 <- read.csv('~/Downloads/R-Friendly Study Data/EmotionsDSv2.csv')
dsPt3 <- read.csv('~/Downloads/BiographicInfo.csv')
names(dsPt3) <- c('Subject', 'Age', 'Gender')
ds$Subject <- as.character(ds$Subject)
dsPt2$Subject <- as.character(dsPt2$Subject)
dsPt3$Subject <- as.character(dsPt3$Subject)
dsTmp1 <- merge(ds, dsPt2, by=c("Subject", "Drive", "Time"), all.x=TRUE)
dsTmp2 <- merge(dsTmp1, dsPt3, by='Subject', all.x=TRUE)
###


### Data Saving ###
write.csv(dsTmp2, '~/Documents/Homework/Stat Final Project/DataSetFNL.csv')
###

### Clean workspace and load data ###
rm(list=ls())
ds <- read.csv('~/Documents/Homework/Stat Final Project/DataSetFNL.csv')
ds$EmotionFelt <- names(ds)[24:31][max.col(ds[,24:31])]
ds$DistFromCenter <- sqrt((ds$Gaze.Y.Pos - 750)^2 + (ds$Gaze.X.Pos - 750)^2)
dsPt3 <- read.csv('~/Downloads/BiographicInfo.csv')
names(dsPt3) <- c('Subject', 'Age', 'Gender')
###
ds3 %>%
  filter(Drive>=4, !is.na(Emo)) %>%
  dplyr::group_by(EmotionFelt) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(pct=n/sum(n)) %>%
  dplyr::arrange(-pct)

ds3Model %>%
  filter(Drive==4, Subject=='T001') %>%
  ggplot(aes(abs(positionShift), group=emotionCategory))+geom_density(aes(colour=emotionCategory)) + asTheme +
  labs(title="Drive 4 for Subject 1")

ds3Model %>%
  group_by(emotionCategory) %>%
### Questions; ###
### 1. Do the stimuli change the person's emotional state? ###
### 2. Does the emotional state have any impact on the subject's driving? ###

### Data Work ###

## Time of the Stimuli per subj
ds %>%
  group_by(Subject, Drive, Stimulus) %>%
  summarise(max.Time = max(Time), min.Time = min(Time)) -> dsStimuliTime

## Time when the stimulus ended
dsStimuliTime %>%
  unite(SubjDrive,Subject, Drive) %>%
  select(-min.Time) %>%
  spread(Stimulus, max.Time) -> maxTimes

## Time when the stimulus began
dsStimuliTime %>%
  unite(SubjDrive,Subject, Drive) %>%
  select(-max.Time) %>%
  spread(Stimulus, min.Time) -> minTimes

names(minTimes) <- c('SubjDrive', 'min0', 'min1', 'min2', 'min3', 'min4', 'min5')
names(maxTimes) <- c('SubjDrive', 'max0', 'max1', 'max2', 'max3', 'max4', 'max5')

##Convert to chars/ints from factor/char
minTimes$Subject <- as.character(substr(minTimes$SubjDrive, 1, 4))
minTimes$Drive <- as.integer(substr(minTimes$SubjDrive, 6,7))
maxTimes$Subject <- as.character(substr(maxTimes$SubjDrive, 1, 4))
maxTimes$Drive <- as.integer(substr(maxTimes$SubjDrive, 6,7))

## Join back to original dataset
ds %>%
  left_join(dsStimuliTime, by = c("Subject", "Drive", "Stimulus")) -> ds2

## Finish the join
ds2 %>%
  left_join(minTimes, c("Subject", "Drive")) %>%
  left_join(maxTimes, c("Subject", "Drive")) -> ds3

## Find which state a subject is in (0 base, 1/3/5 stimulus, 2/4/6 between stimuli)
ds3$StimulusState = ifelse(ds3$Drive <= 4, ds3$Stimulus,
                           ifelse(ds3$Drive==5,
                                  ifelse(ds3$Time < ds3$min1, 0,
                                         ifelse(ds3$Time<=ds3$max1, 1, 
                                                ifelse(ds3$Time<ds3$min2, 2,
                                                       ifelse(ds3$Time<=ds3$max2, 3, 4)))),
                                  ifelse(ds3$Drive==6,
                                         ifelse(ds3$Time<ds3$min3, 0,
                                                ifelse(ds3$Time <= ds3$max3, 5, 6)),
                                         ifelse(ds3$Drive==7,
                                                ifelse(ds3$Time < ds3$min4, 0,
                                                       ifelse(ds3$Time <= ds3$max4, 7, 8)),
                                                ifelse(ds3$Drive==8,
                                                       ifelse(ds3$Time < ds3$min5, 0,
                                                              ifelse(ds3$Time <= ds3$max5, 9, 10)),-1))))
)

ds3 %>%
  mutate(groupVar = paste(Subject, Drive, StimulusState)) -> ds3



ddply(ds3, ~groupVar, transform, lvar = lg(Lane.Position)) -> ds3
ddply(ds3, ~groupVar, transform, lAnger = lg(Anger)) -> ds3
ddply(ds3, ~groupVar, transform, lContempt = lg(Contempt)) -> ds3
ddply(ds3, ~groupVar, transform, lDisgust = lg(Disgust)) -> ds3
ddply(ds3, ~groupVar, transform, lFear = lg(Fear)) -> ds3
ddply(ds3, ~groupVar, transform, lJoy = lg(Joy)) -> ds3
ddply(ds3, ~groupVar, transform, lSad = lg(Sad)) -> ds3
ddply(ds3, ~groupVar, transform, lSurprise = lg(Surprise)) -> ds3
ddply(ds3, ~groupVar, transform, lNeutral = lg(Neutral)) -> ds3
ddply(ds3, ~groupVar, transform, lAccelerate = lg(Acceleration)) -> ds3
ddply(ds3, ~groupVar, transform, lSpeed = lg(Speed)) -> ds3
ddply(ds3, ~groupVar, transform, lBrake = lg(Brake)) -> ds3
ddply(ds3, ~groupVar, transform, lHeartRate = lg(Heart.Rate)) -> ds3
ddply(ds3, ~groupVar, transform, lPerinasal = lg(Perinasal.Perspiration)) -> ds3
ddply(ds3, ~groupVar, transform, lBreathing = lg(Breathing.Rate)) -> ds3
ddply(ds3, ~groupVar, transform, lDist = lg(DistFromCenter)) -> ds3
str(ds3)

ds3 %>%
  mutate(positionShift = Lane.Position - lvar,
         angerShift = Anger - lAnger,
         contemptShift = Contempt - lContempt,
         disgustShift = Disgust - lDisgust,
         fearShift = Fear - lFear,
         joyShift = Joy - lJoy,
         sadShift = Sad - lSad,
         surpriseShift = Surprise - lSurprise,
         neutralShift = Neutral - lNeutral,
         accelShift = Acceleration - lAccelerate,
         speedShift = Speed - lSpeed,
         brakeShift = Brake - lBrake,
         heartRateShift = Heart.Rate - lHeartRate,
         perinasalShift = Perinasal.Perspiration-lPerinasal,
         breathingShift = Breathing.Rate - lBreathing,
         distShift = DistFromCenter - lDist) -> ds3
gsub("Blah", "", "BlahHehe")

dim(ds3)
head()
ds3$domEmotionShift <- gsub('Shift', '', names(ds3[,64:71])[max.col(ds3[,64:71])])

## Modeling ds3


ds3 %>%
  filter(Drive >= 4, !is.na(domEmotionShift)) -> ds3Model




normEmotions2 <- c('neutral', 'joy', 'sad', 'disgust')
distractedEmotions2 <- c( 'anger', 'fear', 'contempt')
ds3Model %>%
  mutate(emotionCategory = ifelse(domEmotionShift %in% normEmotions2, 'Normal', 'Distracted')) -> ds3Model

ds3Model %>%
  mutate(emotionCategory = factor(emotionCategory, c('Normal', 'Distracted'))) -> ds3Model

ds3Model %>%
  mutate(emotionStateCategory = ifelse(EmotionFelt %in% c('Disgust', 'Neutral'), 'Neutral', 'Distracted')) -> ds3Model

ds3Model %>%
  mutate(emotionStateScore = ifelse(ds3Model$emotionStateCategory == 'Distracted', 1, 0)) -> ds3Model

ds3Model$emotionScore <- ifelse(ds3Model$emotionCategory == 'Distracted', 1, 0)
ds3Model$noShift = ifelse(ds3Model$positionShift==0,1,0)
str(ds3Model)

ds3Model$GazeOffScreen <- ifelse(ds3Model$Gaze.X.Pos==0 & ds3Model$Gaze.Y.Pos==0,1,0)
ds3Model$BrakewhileAccelerating <- ifelse(ds3Model$Brake > 0 & ds3Model$Acceleration >0, log(1+ds3Model$Brake), 0)
ds3Model$BrakeCelleration = ds3Model$Brake+ds3Model$Acceleration
ds3Model$isBraking = ifelse(!is.na(ds3Model$Brake) & ds3Model$Brake > 0, 1, 0 )
ds3Model$isAccelerating = ifelse(!is.na(ds3Model$Acceleration) & ds3Model$Acceleration > 0, 1, 0 )

ds3Model %>%
  filter(accelShift != 0) %>%
  ggplot(aes(factor(emotionScore), accelShift)) + geom_boxplot()


ds3Model %>%
  filter(accelShift != 0) %>%
  ggplot(aes(accelShift, group=factor(emotionScore))) + geom_density(aes(colour=factor(emotionScore)))


ds3Model %>%
  filter(Brake>0, Acceleration>0) %>%
  ggplot(aes(log(1+Brake), group=factor(emotionScore)))+geom_density(aes(colour=factor(emotionScore)))

ds3Model %>%
  filter(!is.na(Heart.Rate),!is.na(Steering), !is.na(Perinasal.Perspiration), !is.na(positionShift), !is.na(Breathing.Rate), !is.na(Palm.EDA), !is.na(perinasalShift), !is.na(Gender), !is.na(Age..Y.Young..O.Old.), !is.na(Subject), !is.na(Stimulus), !is.na(Drive)) -> ds3ModelNoNa
#Gender + Age..Y.Young..O.Old. + Heart.Rate + Steering + Perinasal.Perspiration + Breathing.Rate + Palm.EDA

ds3ModelNoNa %>%
  mutate(rselect=ifelse(runif(dim(ds3ModelNoNa)[1])>.8,1,0)) -> ds3ModelNoNa



# Heart.Rate + Steering + Perinasal.Perspiration + Breathing.Rate + Palm.EDA

ds3ModelNoNa$dumb <- 1

ds3ModelNoNa %>%
  mutate(HRNorm = (Heart.Rate - min(Heart.Rate) / (max(Heart.Rate)-min(Heart.Rate))),
         SteeringNorm = (Steering - min(Steering)) / max(Steering),
         PerinasalNorm = (Perinasal.Perspiration - min(Perinasal.Perspiration)) / max(Perinasal.Perspiration),
         BreathingNorm = (Breathing.Rate -min(Breathing.Rate))/max(Breathing.Rate),
         edaNorm = (Palm.EDA - min(Palm.EDA))/max(Palm.EDA)) -> ds3ModelNoNa2

ds3ModelNoNa2$HRNorm <- (ds3ModelNoNa2$Heart.Rate - min(ds3ModelNoNa2$Heart.Rate))/(max(ds3ModelNoNa2$Heart.Rate)-min(ds3ModelNoNa2$Heart.Rate))
ds3ModelNoNa2$SteeringNorm <- (ds3ModelNoNa2$Steering - min(ds3ModelNoNa2$Steering))/(max(ds3ModelNoNa2$Steering)-min(ds3ModelNoNa2$Steering))
ds3ModelNoNa2$PerinasalNorm <- (ds3ModelNoNa2$Perinasal.Perspiration - min(ds3ModelNoNa2$Perinasal.Perspiration))/(max(ds3ModelNoNa2$Perinasal.Perspiration)-min(ds3ModelNoNa2$Perinasal.Perspiration))
ds3ModelNoNa2$BreathingNorm <- (ds3ModelNoNa2$Breathing.Rate - min(ds3ModelNoNa2$Breathing.Rate))/(max(ds3ModelNoNa2$Breathing.Rate)-min(ds3ModelNoNa2$Breathing.Rate))
ds3ModelNoNa2$edaNorm <- (ds3ModelNoNa2$Palm.EDA - min(ds3ModelNoNa2$Palm.EDA))/(max(ds3ModelNoNa2$Palm.EDA)-min(ds3ModelNoNa2$Palm.EDA))
ds3ModelNoNa2$laneShiftNorm <- (abs(ds3ModelNoNa2$positionShift) - min(abs(ds3ModelNoNa2$positionShift)))/(max(abs(ds3ModelNoNa2$positionShift))-min(abs(ds3ModelNoNa2$positionShift)))
ds3ModelNoNa2$Gender <- factor(ds3ModelNoNa2$Gender)
ds3ModelNoNa2$Age..Y.Young..O.Old. <- factor(ds3ModelNoNa2$Age..Y.Young..O.Old.)



ds3ModelNoNa2 %>%
  filter(rselect == 1) -> ds3Test

ds3ModelNoNa2 %>%
  filter(rselect==0) -> ds3Train


##The model
lmfnltrain <- glm(emotionScore ~   Heart.Rate + Steering + Perinasal.Perspiration + perinasalShift + log(abs(positionShift)+1) + factor(Gender) + factor(Age..Y.Young..O.Old.) + Subject + factor(Drive) + factor(Stimulus), data=ds3Train, family='binomial', maxit=100, na.action = na.omit)
summary(lmfnltrain)
#model.backward.selection = step(lmds3trial, direction="backward", trace=1, k=log(dim(testmeds)[1]))


## The model v2
lmfnltrain <- glm(emotionScore ~   Heart.Rate + Steering + Perinasal.Perspiration + perinasalShift + log(abs(positionShift)+1) + factor(Gender) + factor(Age..Y.Young..O.Old.) + Subject + factor(Drive) + factor(Stimulus), data=ds3Train, family='binomial', maxit=100, na.action = na.omit)

lmfnltrain <- glm(emotionStateScore ~   Heart.Rate + Steering + Perinasal.Perspiration + perinasalShift + log(abs(positionShift)+1) + factor(Gender) + factor(Age..Y.Young..O.Old.) + Subject + factor(Drive) + factor(Stimulus), data=ds3Train, family='binomial', maxit=100, na.action = na.omit)


## Total full model v2

lmfnlfulltrain <- glm(emotionStateScore ~ Heart.Rate + GazeOffScreen + Steering + Perinasal.Perspiration + Breathing.Rate + Palm.EDA + heartRateShift + breathingShift + BrakewhileAccelerating+ log(abs(positionShift)+1)  + Gender +  Age..Y.Young..O.Old. + Subject + factor(Drive) + factor(Stimulus) + factor(Failure), data=ds3Train, family='binomial', maxit=100, na.action = na.omit)
drop1(update(lmfnlfulltrain, ~.-heartRateShift), test="Chisq")
search <- step(lmfnlfulltrain)
summary(update(search, ~.-Palm.EDA))
summary(update(search, ~.+Gender + Age..Y.Young..O.Old.))



lmfnltrainv2 <- glm(emotionStateScore ~ Gender*Age..Y.Young..O.Old. + Heart.Rate + Steering + Perinasal.Perspiration + Breathing.Rate + Palm.EDA  + factor(Subject) + factor(Drive) + factor(Stimulus) , data=ds3Train, family='binomial', maxit=100, na.action = na.omit)

## Random Effects Model

ds3TrainRF <- ds3Train
ds3TrainRF$Subject <- as.factor(ds3TrainRF$Subject)

dim(ds3TrainRF[is.na(ds3TrainRF$GazeOffScreen),])
ds3TrainRF[is.na(ds3TrainRF$GazeOffScreen),]$GazeOffScreen <- 0
ds3Test[is.na(ds3Test$GazeOffScreen),]$GazeOffScreen <- 0

lmfnltrainv2ref <- glmer(formula=emotionStateScore ~ Gender + Age..Y.Young..O.Old.  + HRNorm + SteeringNorm + PerinasalNorm + BreathingNorm + edaNorm   + factor(Drive) + factor(Stimulus) + (1|Subject), data=ds3TrainRF, family='binomial', na.action = na.omit)
lmfnltrainv2refv2 <- glmer(formula=emotionStateScore ~ Gender + Age..Y.Young..O.Old. + HRNorm + SteeringNorm + PerinasalNorm + BreathingNorm  + factor(Drive) + factor(Stimulus) + (1|Subject), data=ds3TrainRF, family='binomial', na.action = na.omit)

## Full Model for Analysis
randomEffectsModelReduced <- glmer(formula=emotionStateScore ~  isBraking + HRNorm + SteeringNorm + PerinasalNorm + BreathingNorm + GazeOffScreen   + factor(Drive) + factor(Stimulus) + (1|Subject) + Gender + Age..Y.Young..O.Old., data=ds3TrainRF, family='binomial', na.action = na.omit)
randomEffectsModelReduced2 <- update(randomEffectsModelReduced, ~. - Age..Y.Young..O.Old.)
summary(randomEffectsModelReduced2)

randomEffectsModelComplexAgain <- glmer(formula=emotionStateScore ~  isBraking + Gender*HRNorm + Gender:Age..Y.Young..O.Old. + SteeringNorm + Gender*PerinasalNorm + Gender*BreathingNorm + GazeOffScreen   + factor(Drive) + factor(Stimulus) + (1|Subject) + Age..Y.Young..O.Old., data=ds3TrainRF, family='binomial', na.action = na.omit)

### THE FINAL MODEL; REREDUCED2
randomEffectsModelReReduced <- glmer(formula=emotionStateScore ~  isBraking + HRNorm +  + SteeringNorm + Gender*PerinasalNorm + Gender*BreathingNorm + GazeOffScreen   + factor(Drive) + factor(Stimulus) + (1|Subject) + Age..Y.Young..O.Old., data=ds3TrainRF, family='binomial', na.action = na.omit)
randomEffectsModelReReduced2 <- update(randomEffectsModelReReduced, ~. - Age..Y.Young..O.Old.)

thingy <- update(randomEffectsModelReReduced2, ~.-(1|Subject))
randomEffectsModelReReduced2ControlModel <- glm(formula=emotionStateScore ~  isBraking + HRNorm +  + SteeringNorm + Gender*PerinasalNorm + Gender*BreathingNorm + GazeOffScreen   + factor(Drive) + factor(Stimulus), data=ds3TrainRF, family='binomial', na.action = na.omit)

AIC(randomEffectsModelReduced2)

summary(randomEffectsModelReReduced2, randomEffectsModelReReduced2ControlModel)

anova(randomEffectsModelReReduced2, randomEffectsModelReReduced2ControlModel)

## test the significance of the Random effect
library(RLRsim)
exactLRT(randomEffectsModelReReduced2, randomEffectsModelReReduced2ControlModel)
library(pbkrtest)
PBmodcomp(randomEffectsModelReReduced2, randomEffectsModelReReduced2ControlModel)


### Test version with new interaction effects

randomEffectsModelReReducedv3 <- glmer(formula=emotionStateScore ~  isBraking + HRNorm +  + SteeringNorm + Gender*PerinasalNorm + Gender*BreathingNorm + GazeOffScreen   + factor(Drive) + factor(Stimulus) + (1|Subject) + Age..Y.Young..O.Old.:HRNorm + Age..Y.Young..O.Old.:BreathingNorm + Age..Y.Young..O.Old., data=ds3TrainRF, family='binomial', na.action = na.omit)

#library(rstanarm)
#options(mc.cores = parallel::detectCores())
#randomEffectsModelReducedOther <- stan_glmer(formula=emotionStateScore ~  isBraking + isAccelerating + HRNorm + SteeringNorm + Gender*PerinasalNorm + Gender*BreathingNorm + GazeOffScreen   + factor(Drive) + factor(Stimulus) + (1|Subject) + (1|Subject:Gender) + (1|Subject:Age..Y.Young..O.Old.), data=ds3TrainRF, family='binomial', na.action = na.omit )
# Will use Reduced Model

summary(randomEffectsModelReReducedv3)
Anova(randomEffectsModelReReducedv3, type='III')
## Diagnostics
## Residuals
plot(randomEffectsModelReduced)
## Normality

qqnorm(residuals(randomEffectsModelReduced))

## Leverage
ggplot(data.frame(lev=hatvalues(randomEffectsModelReduced),pearson=residuals(randomEffectsModelReduced,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  asTheme

## Anova for our Reduced Model
Anova(randomEffectsModelReReduced2, type="III")

## Effects
summary(randomEffectsModelReduced) #AIC; 84129 / BIC; 84311

summary(randomEffectsModelReduced2) #AIC; 84128 / BIC; 84290
## Somer's D Computation
#Train
somersd(predict(randomEffectsModelFull),ds3TrainRF$emotionStateScore)

somersd(predict(randomEffectsModelReReducedv3),ds3TrainRF$emotionStateScore)

somersd(predict(randomEffectsModelReReduced2),ds3TrainRF$emotionStateScore)

#Test
somersd(predict(randomEffectsModelReduced2, newdata=ds3Test),ds3Test$emotionStateScore)
somersd(predict(randomEffectsModelReReduced, newdata=ds3Test),ds3Test$emotionStateScore)

somersd(predict(randomEffectsModelReReduced2, newdata=ds3Test),ds3Test$emotionStateScore)



## EDA Graphs

ds3ModelNoNa2 %>%
  filter(Drive==4) %>%
  ggplot(aes(Heart.Rate, group=emotionStateCategory))+geom_density(aes(colour=emotionStateCategory))+asTheme +
  labs(title="Heart Rate Distribution by Emotional State [Drive 4]")

ds3ModelNoNa2 %>%
  filter(Drive==4) %>%
  ggplot(aes(Breathing.Rate, group=emotionStateCategory))+geom_density(aes(colour=emotionStateCategory))+asTheme +
  labs(title="Breathing Rate Distribution by Emotional State [Drive 4]")

ds3ModelNoNa2 %>%
  filter(Drive==4) %>%
  group_by(GazeOffScreen, emotionStateCategory) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(GazeOffScreen, pct, group=emotionStateCategory))+geom_bar(stat='identity', aes(fill=emotionStateCategory))+asTheme +
  labs(title="Gazing Off Screen by Emotional State [Drive 4]")

ds3ModelNoNa2 %>%
  filter(Drive==4) %>%
  group_by(isAccelerating, emotionStateCategory) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(isAccelerating, pct, group=emotionStateCategory))+geom_bar(stat='identity', aes(fill=emotionStateCategory))+asTheme +
  labs(title="Gazing Off Screen by Emotional State [Drive 4]")

ds3ModelNoNa2 %>%
  filter(Drive==4) %>%
  group_by(isBraking, emotionStateCategory) %>%
  summarise(n=n()) %>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(is, pct, group=emotionStateCategory))+geom_bar(stat='identity', aes(fill=emotionStateCategory))+asTheme +
  labs(title="Gazing Off Screen by Emotional State [Drive 4]")


ds3ModelNoNa2 %>%
  filter(Drive==4) %>%
  ggplot(aes(Perinasal.Perspiration, group=emotionStateCategory))+geom_density(aes(colour=emotionStateCategory))+asTheme +
  labs(title="Perinasal Perspiration Distribution by Emotional State [Drive 4]")

## SomersD Function

somersd<-function(predit,target) # my somer's D function
{
  data<-data.frame(predit,target);
  data_sort<-data[order(predit),]
  
  ttl_num<-length(target);
  bin<-20;
  n<-ttl_num%/%bin;
  somersd_bin<-rep(0,times=bin);
  event<-rep(0,times=bin);
  total<-rep(0,times=bin);
  cum_event<-rep(0,times=bin);
  cum_total<-rep(0,times=bin);
  for (i in 1:bin) # calculate PSI for ith bin
  {
    if(i!=bin) {event[i]<-sum(data_sort$target[((i-1)*n+1):(n*i)]);total[i]<-n} else
    {event[i]<-sum(data_sort$target[((i-1)*n+1):ttl_num]);total[i]<-ttl_num-n*(i-1)}
  }
  cum_event<-cumsum(event)
  cum_total<-cumsum(total)
  
  cum_event_pct<-cum_event/cum_event[bin]
  cum_noevent_pct<-(cum_total-cum_event)/(cum_total[bin]-cum_event[bin])
  for (i in 1:bin)
  {
    if(i==1) {somersd_bin[i]<-cum_event_pct[i]*cum_noevent_pct[i]} else
    {somersd_bin[i]<-(cum_event_pct[i]+cum_event_pct[i-1])*(cum_noevent_pct[i]-cum_noevent_pct[i-1])}
  }
  
  somersd=1-sum(somersd_bin)
  return (somersd)
}

lmfnltrainv2ref
somersd(predict(lmfnltrainv2ref),ds3TrainRF$emotionStateScore)
somersd(predict(lmfnltrainv2ref),ds3TrainRF$emotionStateScore)
somersd(predict(lmfnltrainv2refv2),ds3TrainRF$emotionStateScore)
#somersd(predict(lmfnltrainv2),ds3Train$emotionStateScore)
somersd(predict(lmfnltrainv2re),ds3Train$emotionStateScore)

somersd(predict(lmfnltrainv2, newdata=ds3Test),ds3Test$emotionStateScore)


## Prediction / cutoff analysis

#Train
ds3TrainRF$prediction <- predict(randomEffectsModelReduced, type='response')
ds3TrainRF$prediction2 <- predict(randomEffectsModelReduced2, type='response')
ds3TrainRF$prediction3 <- predict(randomEffectsModelReReduced, type='response')
ds3TrainRF$prediction4 <- predict(randomEffectsModelReReduced2, type='response')

#Test
ds3Test$prediction <- predict(randomEffectsModelReduced, newdata=ds3Test, type='response')
ds3Test$prediction2 <- predict(randomEffectsModelReduced2, newdata=ds3Test, type='response')
ds3Test$prediction4 <- predict(randomEffectsModelReReduced2, newdata=ds3Test, type='response')
ds3Test$prediction5 <- predict(randomEffectsModelReReducedv3, newdata=ds3Test, type='response')
sum(ds3TrainRF$prediction4 > .5) / sum(ds3TrainRF$prediction4 >=0)
## Analysis on cutoff

prdctnQuants <- quantile(ds3TrainRF$prediction4, probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
prdctnQuants
## Bucket by .10
ds3TrainRF %>%
  mutate(modelQuantile = ifelse(prediction4 <= prdctnQuants[2], .1,
                         ifelse(prediction4 <= prdctnQuants[3], .2,
                         ifelse(prediction4 <= prdctnQuants[4], .3,
                         ifelse(prediction4 <= prdctnQuants[5], .4,
                         ifelse(prediction4 <= prdctnQuants[6], .5,
                                ifelse(prediction4 <= prdctnQuants[7], .6,
                                       ifelse(prediction4 <= prdctnQuants[8], .7,
                                              ifelse(prediction4 <= prdctnQuants[9], .8,
                                                     ifelse(prediction4 <= prdctnQuants[10], .9,
                                                            ifelse(prediction4 <= prdctnQuants[11], 1, -10)))))
         
                                                                                 )))))) -> ds3TrainRF
ds3TrainRF %>%
  filter(modelQuantile > 0) %>%
  group_by(modelQuantile, emotionStateCategory) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(pct=n/sum(n)) %>%
  select(modelQuantile, pct, emotionStateCategory) %>%
  spread(emotionStateCategory, pct)

ds3TrainRF %>%
  filter(modelQuantile > 0) %>%
  group_by(modelQuantile, emotionStateCategory) %>%
  dplyr::summarise(n = n()) %>%
  select(modelQuantile, n, emotionStateCategory) %>%
  spread(emotionStateCategory, n)


ds3Test %>%
  mutate(modelQuantile = ifelse(prediction <= .038816938, .1,
                                ifelse(prediction <= .062793044, .2,
                                       ifelse(prediction <= .101396300, .3,
                                              ifelse(prediction <= .178687165, .4,
                                                     ifelse(prediction <= .344806846, .5,
                                                            ifelse(prediction <= .529094636, .6,
                                                                   ifelse(prediction <= .751993505, .7,
                                                                          ifelse(prediction <= .808200523, .8,
                                                                                 ifelse(prediction <= .880347638, .9,
                                                                                        ifelse(prediction <= 1, 1, -10)))))
                                                            
                                                     )))))) -> ds3Test


## Assessing Accuracy, Sensitivity, Specificity

ds3TrainRF$firingInd = ifelse(ds3TrainRF$prediction > .5, 1, 0)
ds3TrainRF$firingInd2 = ifelse(ds3TrainRF$prediction2 > .5, 1, 0)
ds3TrainRF$firingInd3 = ifelse(ds3TrainRF$prediction3 > .5, 1, 0)
ds3TrainRF$firingInd4 = ifelse(ds3TrainRF$prediction4 > .5, 1, 0)
ds3Test$firingInd = ifelse(ds3Test$prediction > .5, 1, 0)
ds3Test$firingInd2 = ifelse(ds3Test$prediction2 > .5, 1, 0)
ds3Test$firingInd4 = ifelse(ds3Test$prediction4 > .5, 1, 0)
ds3Test$firingInd5 = ifelse(ds3Test$prediction5 > .5, 1, 0)

table(ds3TrainRF[,names(ds3TrainRF) %in% c('emotionStateScore', 'firingInd')])

table(ds3TrainRF[,names(ds3TrainRF) %in% c('emotionStateScore', 'firingInd2')])
table(ds3TrainRF[,names(ds3TrainRF) %in% c('emotionStateScore', 'firingInd3')])
table(ds3TrainRF[,names(ds3TrainRF) %in% c('emotionStateScore', 'firingInd4')])

table(ds3Test[,names(ds3Test) %in% c('emotionStateScore', 'firingInd')])

table(ds3Test[,names(ds3Test) %in% c('emotionStateScore', 'firingInd2')])
table(ds3Test[,names(ds3Test) %in% c('emotionStateScore', 'firingInd4')])

table(ds3Test[,names(ds3Test) %in% c('emotionStateScore', 'firingInd5')])

#AccuracyTrain:82.8%
(51809+34958) /(51809+34958+8757+9189)
#Sensitivity (TP/P):76.9%
34958/(34958+9189)
#Specificity (TN/N): 84.9%
51809/(51809+8757)

#AccuracyTest: 82.5%
(13189+8627)/(13189+8627+2254+2179)
(13178+8627)/(13178+8627+2190+2254)

#Sensitivity:76.9%
(8627)/(8627+2254)
#Specificity: 86.6%
(13189)/(13189+2179)

## Quintiles for ROC Curve

ScaledQuintiles <- function(x, buckets) {
  brks <- quantile(x, seq(1/buckets, 1, by=1/buckets), na.rm=TRUE)
  rvects <- ifelse(is.na(x), -10, 0)
  for(cut in rev(brks)) {
    rvects <- ifelse(x < cut , cut, rvects) 
  }
  return(rvects)
  #
  
}

## ROC 20-ciles
ds3Test$modelQuantile4 <- ScaledQuintiles(ds3Test$prediction4, 20)

ds3TrainRF$modelQuantile4 <- ScaledQuintiles(ds3TrainRF$prediction4, 20)

## ROC Dataset
ds3Test %>%
  group_by(modelQuantile4, emotionStateCategory) %>%
  dplyr::summarise(n=n()) %>%
  spread(emotionStateCategory, n) -> testRocOut


ds3ModelNoNa2 %>%
  group_by(Subject, Gender, Age..Y.Young..O.Old.) %>%
  summarise(n=n()) %>%
  ungroup %>%
  select(Gender, Age..Y.Young..O.Old.) %>%
  table()

ds3TrainRF %>%
  group_by(modelQuantile4, emotionStateCategory) %>%
  dplyr::summarise(n=n()) %>%
  spread(emotionStateCategory, n) -> trainRocOut

## ROC CSV
write.csv(testRocOut, '~/testRocOut.csv')
write.csv(trainRocOut, '~/trainRocOut.csv')


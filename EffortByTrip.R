library(ggplot2)
library(RPostgres)
library(tidyr)
library(dplyr)
library(zoo)
library(caret)
library(ranger)
library(ggpubr)
library(forcats)
rm(list=ls())

model_tested='Multiclass model'
model_tested='Binary model'
model_tested = 'Speed filter'

if (model_tested == 'Multiclass model'){
  print(model_tested)
  setwd('./results/multiclassModel/')
  
} else if (model_tested == 'Binary model'){ 
  print(model_tested)
  setwd('./results/multiclassModel/')
} else if (model_tested == 'Speed filter') {
  print(model_tested)
  setwd('./results/multiclassModel/')
  
}

meanFtimePoint = 7.6

d= read.csv('vmsOp2_pred.csv')

dobs = d %>% group_by(TrackCode, Operation = ObservedOp) %>% summarize(n=n())  %>% mutate(ObsPred = 'Observed')
dobs$Operation = as.factor(dobs$Operation)
dobs$TrackCode = as.factor(dobs$TrackCode)
dobsEx = expand.grid(TrackCode = levels(dobs$TrackCode), Operation = levels(dobs$Operation))
dobsEx = left_join(dobsEx, dobs, by=c('TrackCode', 'Operation')) %>% arrange(TrackCode)  %>% mutate(ObsPred = 'Observed')
dobsEx[is.na(dobsEx)] = 0

dpred = d %>% group_by(TrackCode, Operation = OperationPred) %>% summarize(n=n()) %>% mutate(ObsPred = 'Predicted')
dpred$Operation = as.factor(dpred$Operation)
dpred$TrackCode = as.factor(dpred$TrackCode)
dpredEx = expand.grid(TrackCode = levels(dpred$TrackCode), Operation = levels(dpred$Operation))
dpredEx = left_join(dpredEx, dpred, by=c('TrackCode', 'Operation')) %>% arrange(TrackCode)  %>% mutate(ObsPred = 'Predicted')
dpredEx[is.na(dpredEx)] = 0


dd = rbind(dobsEx, dpredEx)
if (model_tested == 'Multiclass model' ){dd$Operation = factor(dd$Operation, levels=c('C','A','N'))
} else if (model_tested == 'Binary model' | model_tested == 'Speed filter'){dd$Operation = factor(dd$Operation, levels=c('C','AN'))}

ggplot(dd) + 
  geom_boxplot(aes(x=Operation, y=(n*meanFtimePoint)/60, fill=ObsPred))+
  theme_bw()+
  theme(legend.title = element_blank())+
  ggtitle(model_tested)+
  ylab('Daily vessel operation time (h/day)')+
  theme(axis.text.x = element_text(size=13))
ggsave('Effort_by_trip_SpeedFilter.jpeg', units='px', height = 1000, width = 1200)


#speed histograms
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=PredictedOp), binwidth = 0.2)
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=ObservedOp), binwidth = 0.2)

ggplot(d)+
  geom_point(aes(x=Speed, y=bufferCount, color=OperationPred))
#stats
summary(aov((n*meanFtimePoint)/60 ~ Operation * ObsPred, data=dd))

#speedFilter
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=Fishing), binwidth = 0.2)

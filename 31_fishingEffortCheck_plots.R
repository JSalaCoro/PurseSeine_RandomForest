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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
model_tested='multiclassModel'
model_tested='binaryModel'
model_tested = 'speedFilter'

#1. Kmsq fishing estimates by grid. VMS observed vs VMS predicted
Vms1km = read.csv(paste0('results/', model_tested,'/fishingEffortCheck/1km_VMSobsVMSpred.csv'))
Vms2km = read.csv(paste0('results/', model_tested,'/fishingEffortCheck/2km_VMSobsVMSpred.csv'))
Vms3km = read.csv(paste0('results/', model_tested,'/fishingEffortCheck/3km_VMSobsVMSpred.csv'))
Vms4km = read.csv(paste0('results/', model_tested,'/fishingEffortCheck/4km_VMSobsVMSpred.csv'))
Vms5km = read.csv(paste0('results/', model_tested,'/fishingEffortCheck/5km_VMSobsVMSpred.csv'))

Vms1km$Grid = '1km2'
Vms2km$Grid = '2km2'
Vms3km$Grid = '3km2'
Vms4km$Grid = '4km2'
Vms5km$Grid = '5km2'

df = rbind(Vms1km, Vms2km, Vms3km, Vms4km, Vms5km)
max_y=max(df$VMSOpTime.min._PredictedOp)
df$Grid = as.factor(df$Grid)
if (model_tested == 'multiclassModel' ){df$Operation = factor(df$Operation, levels=c('C','A','N'))
} else if (model_tested == 'binaryModel' | model_tested == 'speedFilter'){df$Operation = factor(df$Operation, levels=c('C','AN'))}

#VMS observed Vs VMS predicted
ggplot(df, aes(x=VMSOpTime.min._ObservedOp, y=VMSOpTime.min._PredictedOp, color=Operation))+
  geom_point()+
  facet_grid(rows=vars(Operation), cols=vars(Grid), scales='free')+
  theme_bw()+
  geom_smooth(method='lm', se=TRUE)+
  geom_abline(slope=1, linetype='dashed')+
  stat_cor(label.y = max_y-150, color='black')+ 
  stat_regline_equation(label.y = max_y-250, color='black')+
  theme(legend.position = 'none')+
  xlab('OBSERVED VMS operation time (min/km2)')+
  ylab('PREDICTED VMS operation time (min/km2)')+
  ggtitle(model_tested)
ggsave(paste0('results/', model_tested,'/fishingEffortCheck/resultsVMSobsVMSpred_kmsq_corrplots.jpeg'), units='px', height = 2000, width = 4000)


#2. Fishing trip operation time. observed vs predicted
meanFtimePoint = 8.6

d= read.csv(paste0('results/', model_tested, '/fishingEffortCheck/vmsOp2_pred.csv'))
d$ObservedOp = as.factor(d$ObservedOp)
d$PredictedOp = as.factor(d$OperationPred)

dobs = d %>% group_by(TrackCode, Operation = ObservedOp) %>% summarize(n=n())  %>% mutate(ObsPred = 'Observed')
dobs$Operation = as.factor(dobs$Operation)
dobs$TrackCode = as.factor(dobs$TrackCode)
dobsEx = expand.grid(TrackCode = levels(dobs$TrackCode), Operation = levels(dobs$Operation)) #to get values for all operations and fishing trips (trackCode)
dobsEx = left_join(dobsEx, dobs, by=c('TrackCode', 'Operation')) %>% arrange(TrackCode)  %>% mutate(ObsPred = 'Observed')
dobsEx[is.na(dobsEx)] = 0 #fill with zero operation time for values with no data

dpred = d %>% group_by(TrackCode, Operation = OperationPred) %>% summarize(n=n()) %>% mutate(ObsPred = 'Predicted')
dpred$Operation = as.factor(dpred$Operation)
dpred$TrackCode = as.factor(dpred$TrackCode)
dpredEx = expand.grid(TrackCode = levels(dpred$TrackCode), Operation = levels(dpred$Operation)) #to get values for all operations and fishing trips (trackCode)
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
ggsave(paste0('results/', model_tested, '/fishingEffortCheck/Effort_by_trip_SpeedFilter.jpeg'), units='px', height = 1000, width = 1200)


#3. Some extra plots
#speed histograms
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=PredictedOp), binwidth = 0.2)
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=ObservedOp), binwidth = 0.2)

#stats
summary(aov((n*meanFtimePoint)/60 ~ Operation * ObsPred, data=dd))

#speedFilter
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=Fishing), binwidth = 0.2)

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
  #setwd('C:/Users/jsala/ownCloud/papers/paperPeixBlau/dataAnalysis/ModelFishingEffortCheck/multiClass/')
  setwd('/home/joan/Desktop/ICM_SAP/papers/paperPeixBlau/dataAnalysis/ModelFishingEffortCheck/multiClass/')
  
} else if (model_tested == 'Binary model'){
  print(model_tested)
  setwd('/home/joan/Desktop/ICM_SAP/papers/paperPeixBlau/dataAnalysis/ModelFishingEffortCheck/binary_C_AN/')
  #setwd('C:/Users/jsala/ownCloud/papers/paperPeixBlau/dataAnalysis/ModelFishingEffortCheck/binary_C_AN/')
} else if (model_tested == 'Speed filter') {
  print(model_tested)
  setwd('/home/joan/Desktop/ICM_SAP/papers/paperPeixBlau/dataAnalysis/ModelFishingEffortCheck/speedFilter/')
}


#VMS observed, VMS predicted
Vms1km = read.csv('1km_VMSobsVMSpred.csv')
Vms2km = read.csv('2km_VMSobsVMSpred.csv')
Vms3km = read.csv('3km_VMSobsVMSpred.csv')
Vms4km = read.csv('4km_VMSobsVMSpred.csv')
Vms5km = read.csv('5km_VMSobsVMSpred.csv')

Vms1km$Grid = '1km2'
Vms2km$Grid = '2km2'
Vms3km$Grid = '3km2'
Vms4km$Grid = '4km2'
Vms5km$Grid = '5km2'


df = rbind(Vms1km, Vms2km, Vms3km, Vms4km, Vms5km)
max_y=max(df$VMSOpTime.min._PredictedOp)
df$Grid = as.factor(df$Grid)
if (model_tested == 'Multiclass model' ){df$Operation = factor(df$Operation, levels=c('C','A','N'))
} else if (model_tested == 'Binary model' | model_tested == 'Speed filter'){df$Operation = factor(df$Operation, levels=c('C','AN'))}

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
ggsave( 'VMSobsVMSpred_kmsq_corrplots_4.jpeg', units='px', height = 2000, width = 4000)




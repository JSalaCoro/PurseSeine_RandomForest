library(ggplot2)
library(RPostgres)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)
library(ggh4x)
library(ggpubr)
library(ggpmisc)

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

Vms1km$Grid = '1 km²'
Vms2km$Grid = '2 km²'
Vms3km$Grid = '3 km²'
Vms4km$Grid = '4 km²'
Vms5km$Grid = '5 km²'

df = rbind(Vms1km, Vms2km, Vms3km, Vms4km, Vms5km)
max_y=max(df$VMSOpTime.min._PredictedOp)
df$Grid = as.factor(df$Grid)
if (model_tested == 'multiclassModel' ){
  df$Operation = factor(df$Operation, levels=c('F','T','C'))
  df$Operation = revalue(df$Operation, c('F'='Fishing', 'T'='Tracking', 'C'='Cruising'))
  colourPaletteC = c('Cruising'='#2283C8', 'Tracking'='#ff7f0e', 'Fishing'='#2ca02c')
  figureHeight = 1600
  title='Multiclass model'
} else if (model_tested == 'binaryModel' | model_tested == 'speedFilter'){
  df$Operation = factor(df$Operation, levels=c('F','TC'))
  df$Operation = revalue(df$Operation, c('F'='Fishing', 'TC'='Tracking-Cruising'))
  colourPaletteC = c('Tracking-Cruising'='#C82937', 'Fishing'='#2ca02c')
  figureHeight = 1300
  if (model_tested == 'binaryModel'){title='Binary model'}
  else if (model_tested == 'speedFilter'){title='Speed filter'}
}

#VMS observed Vs VMS predicted
#for facet_wrap labelling: https://stackoverflow.com/questions/52706599/how-to-position-strip-labels-in-facet-wrap-like-in-facet-grid
#https://www.tutorialspoint.com/how-to-display-regression-slope-using-model-in-a-plot-created-by-ggplot2-in-r
ggplot(df, aes(x=VMSOpTime.min._ObservedOp, y=VMSOpTime.min._PredictedOp, color=Operation))+
  geom_point(size=1)+
  facet_grid2(rows = vars(Operation), cols=vars(Grid), axes='all', scales='free', independent='all')+
  theme_bw()+
  geom_abline(slope=1, linetype='dashed')+
  stat_poly_line(formula=y~x)+
  stat_poly_eq(mapping=use_label('eq'), label.y=0.9,
               size=3.5, color='black')+
  stat_correlation(use_label('R'), label.y=0.75,
               size=3.5, color='black')+
  theme(legend.position = 'none',
        axis.text = element_text(size=9),
        axis.title = element_text(size=13),
        strip.background = element_rect(fill='white'),
        strip.text = element_text(face='bold', size=12),
        plot.title = element_text(size=14, face='bold'),
        )+
  scale_colour_manual(values = colourPaletteC)+
  xlab('Observed VMS operation time (min/km²)')+
  ylab('Predicted VMS operation time (min/km²)')+
  ggtitle(title)

ggsave(paste0('results/', model_tested,'/fishingEffortCheck/resultsVMSobsVMSpred_kmsq_corrplots.png'), 
       units='px', 
       height = figureHeight, width = 3000, dpi=300)


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
dd$ObsPred = as.factor(dd$ObsPred)
if (model_tested == 'multiclassModel' ){
  dd$Operation = factor(dd$Operation, levels=c('F','T','C'))
  dd$Operation = revalue(dd$Operation, c('F'='Fishing', 'T'='Tracking', 'C'='Cruising'))
} else if (model_tested == 'binaryModel' | model_tested == 'speedFilter'){
  dd$Operation = factor(dd$Operation, levels=c('F','TC'))
  dd$Operation = revalue(dd$Operation, c('F'='Fishing', 'TC'='Tracking-Cruising'))
}

ggplot(dd, aes(x=Operation, y=(n*meanFtimePoint)/60, fill=ObsPred)) + 
  geom_boxplot()+
  scale_fill_manual(values=c('Observed'='darkgrey', 'Predicted'='white'))+
  theme_bw()+
  theme(legend.title = element_blank())+
  ggtitle(title)+
  ylab('Daily vessel operation time (h/day)')+
  theme(axis.text.x = element_text(size=10),
        axis.title.x = element_blank())
ggsave(paste0('results/', model_tested, '/fishingEffortCheck/Effort_by_trip.jpeg'), units='px', height = 1000, width = 1200)

#stats
summary(aov((n*meanFtimePoint)/60 ~ Operation * ObsPred, data=dd))



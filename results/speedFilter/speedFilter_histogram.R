library(ggplot2)
library(multimode)
library(RPostgreSQL)
library(tidyr)
library(dplyr)
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d = read.csv(paste0('../../results/speedFilter/fishingEffortCheck/vmsOp2_pred.csv'))

#3. Speed histogram analysis
#speed histograms
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=PredictedOp), binwidth = 0.2)
ggplot(d )+
  geom_histogram(aes(x=Speed, fill=ObservedOp), binwidth = 0.2)



speeddf = d %>% filter(Speed < 12 & Speed > 0.1) %>% select(Speed)
speed = speeddf$Speed
loc = locmodes(speed, mod0=nmodes(speed, bw=bw.nrd0(speed)), display=TRUE, xlab='Speed (kn)')
loc$locations

ggplot(d %>% filter(Speed < 12))+
  geom_histogram(aes(x=Speed), binwidth = bw.nrd0(speed), color='#5e5e5e', fill='#6464644d')+
  geom_vline(aes(xintercept = loc$locations[1], color='Mode'), linetype='dashed', size=1)+
  geom_vline(aes(xintercept = loc$locations[2], color='Antimode'), size=1)+
  geom_vline(aes(xintercept = loc$locations[3], color='Mode'), linetype='dashed', size=1)+
    geom_vline(aes(xintercept = loc$locations[4], color='Antimode'), size=1)+
  geom_vline(aes(xintercept = loc$locations[5], color='Mode'), linetype='dashed', size=1)+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0), name='Speed (Kn)') + 
  scale_y_continuous(limits=c(0,300), expand=c(0,0), name = 'VMS points count')+
  scale_color_manual(name = "", values = c(Mode = "#a76b6b", Antimode = "#3e2599"))+
  theme_bw()+
  theme(legend.position = c(0.905,0.895),
  legend.title = element_blank(),
  legend.background = element_blank(),
   legend.box.background = element_rect(colour = "black"))

ggsave('Figure2.png', units='cm', width = 15, height = 10)

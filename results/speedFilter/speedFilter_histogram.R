library(ggplot2)
library(multimode)
library(tidyr)
library(dplyr)
rm(list=ls())

d = read.csv(paste0('results/speedFilter/fishingEffortCheck/vmsOp2_pred.csv'))

#3. Speed histogram analysis
#speed histograms
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=PredictedOp), binwidth = 0.2)
ggplot(d)+
  geom_histogram(aes(x=Speed, fill=ObservedOp), binwidth = 0.2)


speeddf = d %>% filter(Speed < 12) %>% select(Speed)
speed = speeddf$Speed
loc = locmodes(speed, mod0=nmodes(speed, bw=bw.nrd0(speed)), display=TRUE, xlab='Speed (kn)')

library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(ggpubr)
library(forcats)
library(factoextra)
rm(list=ls())

d = read.csv('results/multiclassModel/gridSearchCV_results.csv')

params=list('param_bootstrap', 'param_max_depth', 'param_max_features', 'param_max_leaf_nodes', 'param_max_samples',
            'param_min_impurity_decrease', 'param_min_samples_leaf', 'param_min_samples_split', 'param_n_estimators')
for (param in params){
  print(param)
  
  ggarrange(
    ggplot(d)+
      geom_boxplot(aes(y=mean_test_precision_score, x=as.factor(d[[param]])))+ ggtitle(param)
    ,
    ggplot(d)+
      geom_boxplot(aes(y=mean_test_recall_score, x=as.factor(d[[param]])))
    ,
    ggplot(d)+
      geom_boxplot(aes(y=mean_test_accuracy_score, x=as.factor(d[[param]])))
    ,
    ggplot(d)+
      geom_boxplot(aes(y=mean_test_f1_score, x=as.factor(d[[param]])))
    ,
    nrow=1)
  ggsave(paste0('results/multiclassModel/gridSearchCVresultsPlots/', param,'.png'), units='cm', width = 20, height = 10)
  
}

ggplot(d)+
  geom_line(aes(x=mean_test_recall_score, y=mean_test_precision_score))

##PROBABILITY THRESHOLD
predProva = read.csv('predict_proba.csv')
predProva$X0.1 = as.factor(predProva$X0.1)
ggplot(predProva)+
  geom_boxplot(aes(x=X0.1, y=X2))

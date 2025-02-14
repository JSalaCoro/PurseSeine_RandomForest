# PurseSeine_RandomForest
Machine learning models training for purse seine fleet fishing operation identification. In this readme file you'll find some information to be able to reuse the code with your own data or download the trained models to make predictions. All original data is also available so you can run the scripts on the same study datasets.

![alt text](data/pic2.jpeg)

Four main scripts are defined for data analysis:

## 0_dataPreprocessing
Gets observed-classified data from 'data/vmsOp2.csv' and preprocesses it to train ML model. Some of the predicitve variables such as bufferGeom or speed and cog diffs will be calculated. 
The result will be stored in 'data/vmsOp.csv'

## 1_randomForest
Machine learning model training. Both binary and multiclass model will be trained with gridsearchCV from sckit-lean package. 

Results on the hyperparameter tunning are stored in 'results/model/gridSearchCV_results.csv'. 'gridSearchCVresults_plots.R' script can be used to make plots summarizing the effect of hyperparameters on model performance. Plots are stored in 'results/model/gridSearchCVresultsPlots/'.

Trained models are stored in 'results/multiClassModel/fittedModel_multiClass.pkl' and 'results/binaryModel/fittedModel_binaryModel.pkl'

## 2_shapValues
Shap values for both models are analysed. Two type of plots are made for each trained model:
- Boxplots for each predictive variable (features)
- Scatterplots of shapValues Vs featureValues

## 3_fishingEffortCheck
Trained models are used to calculate predicted fishing effort estimates (fishing time per operation) by spatial grid cells (from 1 to 5 km^2). With '31_fishingEffortCheck_plots.R'  some nice plots can be made to compare predicted Vs observed fishing effort estimates.
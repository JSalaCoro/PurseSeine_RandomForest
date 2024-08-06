from sklearn.metrics import confusion_matrix, accuracy_score, precision_recall_fscore_support
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sn

def model_scoring_and_confusion_matrix_multiclassModel(y_test_dataset, y_pred_dataset):
    
    y_test_dataset.value_counts() #show number of truth values by category
    ##accuracy and Fscore
    print('Accuracy of the final model is: ' + str(accuracy_score(y_test_dataset, y_pred_dataset)))
    print(pd.DataFrame(precision_recall_fscore_support(y_test_dataset, y_pred_dataset), index=['precision','recall','F-score','totals']).rename(columns={0:'A', 1:'C', 2:'N'}))

    cm = confusion_matrix(y_test_dataset, y_pred_dataset)
    #cm = confusion_matrix(y_train, y_pred_train)
    cmdf = pd.DataFrame({'A': cm[:,0], 'C': cm[:,1], 'N': cm[:,2]}, index=['A', 'C', 'N'])

    plt.figure(figsize=(8,5))
    sn.heatmap(cmdf, annot=True, fmt='g')
    plt.xlabel('Predicted')
    plt.ylabel('Truth')
    #plt.savefig('ConfMatrixP_vmsModel.png', dpi=400)
    truth = y_test_dataset.reset_index(drop=True)
    predicted = pd.Series(y_pred_dataset).reset_index(drop=True)
    frame = {'truth':truth, 'predicted':predicted}
    
def model_scoring_and_confusion_matrix_binaryModel(y_test_dataset, y_pred_dataset):
    
    y_test_dataset.value_counts() #show number of truth values by category
    ##accuracy and Fscore
    print('Accuracy of the final model is: ' + str(accuracy_score(y_test_dataset, y_pred_dataset)))
    print(pd.DataFrame(precision_recall_fscore_support(y_test_dataset, y_pred_dataset), index=['precision','recall','F-score','totals']).rename(columns={0:'AN', 1:'C'}))

    cm = confusion_matrix(y_test_dataset, y_pred_dataset)
    #cm = confusion_matrix(y_train, y_pred_train)
    cmdf = pd.DataFrame({'AN=0': cm[:,0], 'C=1': cm[:,1]}, index=['AN', 'C'])

    plt.figure(figsize=(8,5))
    sn.heatmap(cmdf, annot=True, fmt='g')
    plt.xlabel('Predicted')
    plt.ylabel('Truth')
    #plt.savefig('ConfMatrixP_vmsModel.png', dpi=400)
    truth = y_test_dataset.reset_index(drop=True)
    predicted = pd.Series(y_pred_dataset).reset_index(drop=True)
    frame = {'truth':truth, 'predicted':predicted}

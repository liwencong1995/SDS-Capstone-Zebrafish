import pandas as pd
import numpy as np
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.metrics import f1_score, precision_score, recall_score

'''
A function that builds a SVM model with linear kernel to classify points to two classes.

Inputs:
training_landmarks - a pandas dataframe containing all training landmark data.
index              - a perticular landmark id of interest. eg. '101'
x_names            - a list of explanatory variable names. eg. ['pts', 'r']
y_name             - a string representing response variable name. eg. 'stype'
class0             - name of the first class. eg. 'wt-at'
class1             - name of the second class. eg. 'mt-at'
C_values           - a list of tunning variable C (penalty parameter of the error term) that the method would grid-search on. Default value is [0.1, 1, 10].

Output:
svm                - the SVM model trained from the training dataset
ww                 - among the training samples, the number of wild type samples with chosen landmark predicted as wild type.
wm                 - among the training samples, the number of wild type samples with chosen landmark predicted as mutant type.
mm                 - among the training samples, the number of mutant type samples with chosen landmark predicted as mutant type.
mw                 - among the training samples, the number of mutant type samples with chosen landmark predicted as wild type.
'''
def svm_classification(training_landmarks, index, x_names, y_name, class0, class1, C_values = [0.1, 1, 10] ):
    # filter out the landmarks needed
    chosenLandmark = landmarks[landmarks.landmark_index==index]
    chosenLandmark = chosenLandmark[np.isfinite(chosenLandmark['r'])]
    
    # create training and testing data
    X = chosenLandmark[x_names]
    y = chosenLandmark[y_name]
    y = y.replace([class1], 1)
    y = y.replace([class0], 0)

    # check whether both classes exist
    count_1 = chosenLandmark[y_name].str.contains(class1).sum()
    count_0 = chosenLandmark[y_name].str.contains(class0).sum()

    if (count_1 < 2 or count_0 < 2):
        return None, None, None, None, None

    # find the best C value by cross-validation
    tuned_parameters = [{'C': C_values}]
    clf = GridSearchCV(SVC(kernel='linear'), tuned_parameters, cv=10, scoring='accuracy')
    clf.fit(X.values, y.values)
    best_c = clf.best_params_['C']
    
    svc = SVC(C=best_c, kernel='linear')
    svc.fit(X, y)
    
    prediction = svc.predict(X)

    # print confusion matrix
    print("confusion matrix: ")
    cm = confusion_matrix(y, prediction)
    cm_df = pd.DataFrame(cm.T, index=svc.classes_, columns=svc.classes_)
    print(cm_df)

    # Statistics of training precision:
    # number of wild type samples with this landmark predicted as wild type.
    ww =0
    # number of wild type samples with this landmark predicted as mutant type.
    wm = 0
    # number of mutant type samples with this landmark predicted as mutant type.
    mm = 0
    # number of mutant type samples with this landmark predicted as wild type.
    mw = 0
    
    for i in range (len(y)):
        _y = y.values[i]
        _p = prediction[i]

        if _y==1 and _p==1:
            mm = mm + 1
        elif _y==1 and _p==0:
            mw = mw + 1
        elif _y==0 and _p==0:
            ww = ww + 1
        elif _y==0 and _p==1:
            wm = wm + 1
    
    return svc, ww, wm, mm, mw


if __name__ == "__main__":
    # Get interested chnnel name
    channel = ''
    while (channel != 'AT' and channel != 'ZRF'):
        channel = input("Please enter 'AT' or 'ZRF' to indicate channel interested: ")
    
    class0 = 'mt-zrf' if channel == 'ZRF' else 'mt-at'
    class1 = 'wt-zrf' if channel == 'ZRF' else 'wt-at'

    # Read in landmark data
    data_type = '-1'
    while (data_type != '0' and data_type != '1'):
        data_type = input("Enter 0 for filling NaN values with median and 1 for filling with 2*median: ")
    landmarks = pd.read_csv('./data/final/landmark_AT_filled_w_median.csv') if data_type=='0' else pd.read_csv('./data/final/landmark_AT_filled_w_2median.csv')

    # Get sample id
    sample = pd.DataFrame()
    while(sample.shape[0]<2):
        sample_id = str(input("Please enter a VALID sample index: "))
        sample = landmarks[landmarks.sample_index==sample_id]

    # Get result file's name and create the file with column names
    result_file_name = str(input("Please enter result file name: "))
    result_file = open(result_file_name, 'w')
    result_file.write('sample_index, landmark_index, pred, ww, wm, mm, mw\n')
    result_file.close()

    # Get existing landmark ids
    landmark_ids = sample['landmark_index']

    leave_one_out = landmarks[landmarks.sample_index!=sample_id]
    for l in landmark_ids.values:
        print ("=======================================")
        print ("landmark: ", str(l))
        svc, ww, wm, mm, mw = svm_classification(training_landmarks = leave_one_out,
                                                 index = l,
                                                 x_names = ['pts', 'r'],
                                                 y_name = 'stype',
                                                 class0 = class0,
                                                 class1 = class1,
                                                 C_values = [0.1, 1, 10])
        if (svc is None):
            print("One of the classes have too few samples for this landmark, so skipping it.")
            continue
        prediction = svc.predict(sample[sample.landmark_index==l][['pts', 'r']])
        result = ', '.join(str(x) for x in [sample_id, l, prediction[0], ww, wm, mm, mw]) + '\n'
        print('result:', result)

        result_file = open(result_file_name, 'a')
        result_file.write(result)
        result_file.close()

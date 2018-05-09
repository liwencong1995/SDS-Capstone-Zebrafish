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
type0_0              - among the training samples, the number of class0 type samples with chosen landmark predicted as class0 type.
type0_1              - among the training samples, the number of class0 type samples with chosen landmark predicted as class1 type.
type1_1              - among the training samples, the number of class1 type samples with chosen landmark predicted as class1 type.
type1_0              - among the training samples, the number of class1 type samples with chosen landmark predicted as class0 type.
'''
def svm_classification(training_landmarks, index, x_names, y_name, class0, class1, C_values = [0.1, 1, 10] ):
    # filter out the landmarks needed
    chosenLandmark = landmarks[landmarks.landmark_index==index]
    chosenLandmark = chosenLandmark[np.isfinite(chosenLandmark['r'])]
    
    # create training and testing data
    X = chosenLandmark[x_names]
    y = chosenLandmark[y_name]

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
    type0_0 =0
    # number of wild type samples with this landmark predicted as mutant type.
    type0_1 = 0
    # number of mutant type samples with this landmark predicted as mutant type.
    type1_1 = 0
    # number of mutant type samples with this landmark predicted as wild type.
    type1_0 = 0
    
    for i in range (len(y)):
        _y = y.values[i]
        _p = prediction[i]

        if _y==class1 and _p==class1:
            type1_1 = type1_1 + 1
        elif _y==class1 and _p==class0:
            type1_0 = type1_0 + 1
        elif _y==class0 and _p==class0:
            type0_0 = type0_0 + 1
        elif _y==class0 and _p==class1:
            type0_1 = type0_1 + 1
    
    return svc, type0_0, type0_1, type1_1, type1_0


if __name__ == "__main__":
    # Get Datafile
    landmarks = pd.DataFrame()
    while(landmarks.shape[0]<2):
        filename = str(input("Please enter dataset's path: "))
        try:
            landmarks = pd.read_csv(filename)
        except Exception:
            print ("Error in reading the file. Please check whether file exists.")

    # Column names
    columns = list(landmarks)
    # Check column names
    if 'stype' not in columns:
        print("Incorrect column names: Please name your sample type's column as 'stype' ")
        exit()
    if 'sample_index' not in columns:
        print("Incorrect column names: Please name your sample index's column as 'sample_index' ")
        exit()
    if 'landmark_index' not in columns:
        print("Incorrect column names: Please name your landmark index's column as 'landmark_index' ")
        exit()

    # Get Parameters' column names
    parameters = list(set(columns) - set(['stype', 'sample_index', 'landmark_index']))

    # Get class names
    class0 = ''
    class1 = ''
    classes = list(set(landmarks['stype'].values))
    while (class0 not in classes):
        class0 = str(input("Please enter name of type 0: "))
    while (class1 not in classes):
        class1 = str(input("Please enter name of type 1: "))

    # Remove rows with NaN values
    for parameter in parameters:
        landmarks = landmarks[np.isfinite(landmarks[parameter])]

    # Get sample id
    sample = pd.DataFrame()
    while(sample.shape[0]<2):
        sample_id = str(input("Please enter a VALID sample index: "))
        sample = landmarks[landmarks.sample_index==sample_id]

    # Get result file's name and create the file with column names
    result_file_name = str(input("Please enter result file path: "))
    result_file = open(result_file_name, 'w')
    result_file.write('sample_index,type,landmark_index,pred,type0_0,type0_1,type1_1,type1_0\n')
    result_file.close()

    # Get existing landmark ids
    landmark_ids = sample['landmark_index']

    # Get Actual Type (the Label)
    stype = sample.iloc[0]['stype']

    leave_one_out = landmarks[landmarks.sample_index!=sample_id]
    for l in landmark_ids.values:
        print ("=======================================")
        print ("landmark: ", str(l))
        svc, type0_0, type0_1, type1_1, type1_0 = svm_classification(training_landmarks = leave_one_out,
                                                 index = l,
                                                 x_names = ['pts', 'r'],
                                                 y_name = 'stype',
                                                 class0 = class0,
                                                 class1 = class1,
                                                 C_values = [0.1, 1, 10])
        if (svc is None):
            print("One of the classes have too few samples for this landmark, so skipping it.")
            continue

        prediction = svc.predict(sample[sample.landmark_index==l][['pts', 'r']])[0]
        result = ','.join(str(x) for x in [sample_id, stype, l, prediction, type0_0, type0_1, type1_1, type1_0 ]) + '\n'
        print('result:', result)

        result_file = open(result_file_name, 'a')
        result_file.write(result)
        result_file.close()

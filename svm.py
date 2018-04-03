import pandas as pd
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.metrics import f1_score, precision_score, recall_score

# a function to draw a plot of an SVM
def plot_svc(svc, X, y, h=0.02, pad=0.25):
    x_min, x_max = X[:, 0].min()-pad, X[:, 0].max()+pad
    y_min, y_max = X[:, 1].min()-pad, X[:, 1].max()+pad
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    Z = svc.predict(np.c_[xx.ravel(), yy.ravel()])
    Z = Z.reshape(xx.shape)
    plt.figure(figsize=(8, 5))
    plt.contourf(xx, yy, Z, cmap=plt.cm.Paired, alpha=0.2)

    plt.scatter(X[:,0], X[:,1], s=70, c=y, cmap=mpl.cm.Paired)
    sv = svc.support_vectors_
    plt.xlim(x_min, x_max)
    plt.ylim(y_min, y_max)
    plt.xlabel('X1')
    plt.ylabel('X2')
    plt.show()

def svm_classification(landmarks, index):
    # filter out the landmarks needed
    chosenLandmark = landmarks[landmarks.landmark_index==index]
    chosenLandmark = chosenLandmark[np.isfinite(chosenLandmark['r'])]
    
    # create training and testing data
    X = chosenLandmark[['pts', 'r']]
    y = chosenLandmark['stype']
    #y = y.replace(['mt-zrf'], 1)
    #y = y.replace(['wt-zrf'], 0)
    y = y.replace(['mt-at'], 1)
    y = y.replace(['wt-at'], 0)

    # check whether both classes exist
    count_1 = chosenLandmark['stype'].str.contains('mt-at').sum()
    count_0 = chosenLandmark['stype'].str.contains('wt-at').sum()

    if (count_1 < 2 or count_0 < 2):
        return None, None, None, None, None

    # present the data
    '''plt.figure(figsize=(8, 5))
    plt.scatter(X.values[:,0], X.values[:,1], s=70, c=y, cmap=mpl.cm.Paired)
    plt.xlabel('X1')
    plt.ylabel('X2')
    plt.show()'''
    
    # find the best C value by cross-validation
    tuned_parameters = [{'C': [0.1, 1, 10]}]
    clf = GridSearchCV(SVC(kernel='linear'), tuned_parameters, cv=10, scoring='accuracy')
    clf.fit(X.values, y.values)
    best_c = clf.best_params_['C']
    
    svc = SVC(C=best_c, kernel='linear')
    svc.fit(X, y)
    
    #plot_svc(svc, X.values, y)
    
    prediction = svc.predict(X)
    # print confusion matrix
    print("confusion matrix: ")
    cm = confusion_matrix(y, prediction)
    cm_df = pd.DataFrame(cm.T, index=svc.classes_, columns=svc.classes_)
    print(cm_df)
    # print prediction results
    print('Classification Accuracy Results: ')
    ww =0
    wm = 0
    mm = 0
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

# Read data
<<<<<<< HEAD
landmarks = pd.read_csv('./data/tidyLandmarks_AT_no_na.csv')
#landmarks = pd.read_csv('../data/tidyLandmarks_ZRF_no_na.csv')
=======
landmarks = pd.read_csv('./data/landmark_AT_w_index_no_na.csv')
#landmarks = pd.read_csv('./data/landmark_ZRF_w_index_no_na.csv')
>>>>>>> 3dc917864ad11977ff05a20189eb5c2552970320

sample_id = str(input("Please enter sample index: "))

result_file_name = str(input("Please enter result file name: "))

result_file = open(result_file_name, 'w') 
result_file.write('landmark_index, pred, ww, wm, mm, mw\n')
result_file.close() 

sample = landmarks[landmarks.sample_index==sample_id]
landmark_ids = sample['landmark_index']

results = []
leave_one_out = landmarks[landmarks.sample_index!=sample_id]
for l in landmark_ids.values:
    print ("=======================================")
    print ("landmark: ", str(l))
    svc, ww, wm, mm, mw = svm_classification(leave_one_out, l)
    if (svc is None):
        print("One of the classes have too few samples for this landmark, so skipping it.")
        continue
    prediction = svc.predict(sample[sample.landmark_index==l][['pts', 'r']])
    result = ', '.join(str(x) for x in [l, prediction[0], ww, wm, mm, mw]) + '\n'
    results.append((l, prediction[0], ww, wm, mm, mw))
    print(results)

    result_file = open(result_file_name, 'a') 
    result_file.write(result)
    result_file.close() 

'''print ("=======================================")
print("SAMPLE REPORT")
r_sig = []
for svm in results:
    if svm[2]>0.5:
        r_sig.append(svm)

print("The sample has", str(len(results)), " landmarks.")
print(str(len(r_sig)), " landmarks have f1 score larger than 0.5.")

one = []
zero = []
for svm in r_sig:
    if svm[1]==1:
        one.append(svm)
    else:
        zero.append(svm)

print("One was voted ", str(len(one)), " times.")
print("Zero was voted ", str(len(zero)), " times.")'''

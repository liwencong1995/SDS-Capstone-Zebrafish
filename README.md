# SDS-Capstone-Zebrafish

`svm.py` is used for creating SVM classification matrices and models. The output of this file is in the `output` folder. 

`data` folder contains all datafiles needed for the analysis.

* `raw` folder 
    * `AT_landmarks_3-28-18.csv` and `ZRF_landmarks_3-29-18.csv` are the original data files of the two channels received from Morgan Schwartz. 
    * `AT_landmarks.csv` and `ZRF_landmarks.csv` contains the renamed duplicated sample index `112` as `112_1` and `112_2`. (SCRIPT: `svm.ipynb`)

* `tidy` folder 
    * `tidyLandmarks_AT.csv` and `tidyLandmarks_ZRF.csv` are the data that have been cleaned and rearranged into a long format. (SCRIPT: `tidyLandmark.R`)
    * In `landmark_AT_w_index.csv` and `landmark_ZRF_w_index.csv`, `landmark_index` column was added so that we can use this new variable to run SVM models. (SCRIPT: `addIndex.ipynb` and `visualization.R`)

* `median` folder 
    * In `AT_median.csv` and `ZRF_median.csv`, median and 2 * median values were computed for replacing the NA values. (SCRIPT: `median_calculation.R`)
    * `landmark_AT_median.csv` and `landmark_ZRF_median.csv` 

* `final` folder 
    * `landmark_AT_filled_w_median.csv` and `landmark_ZRF_filled_w_median.csv` are the datasets that have the NA values replaced with the median r values of each landmark by data type (wildtype or mutant). (SCRIPT: `svm.ipynb`)
    * `landmark_AT_filled_w_2median.csv` and `landmark_ZRF_filled_w_2median.csv` are the datasets that have the NA values replaced with the 2 * median r values of each landmark by data type (wildtype or mutant). (SCRIPT: `svm.ipynb`)
    
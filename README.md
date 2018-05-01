# SDS 410: SDS Capstone 
## Zebrafish

## Introduction
This repository was created to store the work that was done by Shuli Hu, Dejia Tang, Wencong (Priscilla) Li, and JiYoung (Justine) Yun in SDS 410: SDS Capstone Class in Spring 2018.

This study focuses on the classification of biological creatures’ phenotypes. In this project, we utilize support vector machine to distinguish structures of Zebrafish’s brains by using data generated from landmark analysis (cited Morgan’s paper). We create a tool for biologists to intuitively classify three-dimensional biological shapes into two groups, usually defined as wild type and mutant, and understand which part of the shapes have the most impact on the classification result. This project derives from Professor Barresi’s biological image analysis research at Smith College.

## Final Paper and Model
The `PDF` version of the final paper can be found in `8. Final Paper` folder. In addition, the source code of both the **Model** and the **User Interface** can be downloaded from `9. Final Model` folder.

## 3. Input Data
`3. Input Data` folder contains all datafiles used in the study.

* `raw` folder 
    * `AT_landmarks_3-28-18.csv` and `ZRF_landmarks_3-29-18.csv` are the original data files of the two channels received from Morgan Schwartz. 
    * `AT_landmarks.csv` and `ZRF_landmarks.csv` contains the renamed duplicated sample index `112` as `112_1` and `112_2`. (SCRIPT: `svm.ipynb`)

* `tidy` folder 
    * `tidyLandmarks_AT.csv` and `tidyLandmarks_ZRF.csv` are the data that have been cleaned and rearranged into a long format. (SCRIPT: `tidyLandmark.R`)
    * In `landmark_AT_w_index.csv` and `landmark_ZRF_w_index.csv`, `landmark_index` column was added so that we can use this new variable to run SVM models. (SCRIPT: `addIndex.ipynb` and `visualization.R`)

* `median` folder 
    * In `AT_median.csv` and `ZRF_median.csv`, median and 2 
    * median values were computed for replacing the NA values. (SCRIPT: `median_calculation.R`)
    * `landmark_AT_median.csv` and `landmark_ZRF_median.csv` 

* `final` folder 
    * `landmark_AT_filled_w_median.csv` and `landmark_ZRF_filled_w_median.csv` are the datasets that have the NA values replaced with the median r values of each landmark by data type (wildtype or mutant). (SCRIPT: `svm.ipynb`)
    * `landmark_AT_filled_w_2median.csv` and `landmark_ZRF_filled_w_2median.csv` are the datasets that have the NA values replaced with the 2 
    * median r values of each landmark by data type (wildtype or mutant). (SCRIPT: `svm.ipynb`)
    
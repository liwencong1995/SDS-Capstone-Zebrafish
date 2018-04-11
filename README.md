# SDS-Capstone-Zebrafish

`data` folder contains all datafiles needed for the analysis.

`raw` folder contains data received from Morgan Schwartz. In order to get data ready for the following steps, we renamed the duplicated sample index `112` as `112_1` and `112_2`. 

`tidy` folder contains data that have been cleaned and rearranged into long format. In this folder, we also added `landmark_index` so that we can use this variable to run SVM models.

`median` folder contains data needed to deal with the missing values in the orginal datasets. We replaced missing value with median and 2 times median `r`.

`final` folder contains data without missing value. These data are inputs of the SVM model.
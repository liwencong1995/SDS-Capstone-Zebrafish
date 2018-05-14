import os
import pandas as pd
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from sklearn.metrics import f1_score


"""
Return a list of result file pathes in the output folder

output_path: output folder name
"""
def get_result_file_pathes(output_path):
    results = os.listdir(output_path)
    results = [output_path + '/' + result for result in results]
    return results


"""
Return the result dataframe containing precision scores calculated.

input_path: raw result file's path
"""
def process_row_data(input_path):
    print(input_path)
    results = pd.read_csv(input_path) 
    results.columns = results.columns.str.strip()
    
    base=os.path.basename(input_path)
    '''sample_index = os.path.splitext(base)[0][2:]
    print(sample_index)
    results['sample_index'] = sample_index'''
    
    results['type0_num'] = (results['type0_0'] + results['type0_1'])
    results['type1_num'] = (results['type1_1'] + results['type1_0'])

    results['type0_precision'] = results['type0_0'] / (results['type0_0'] + results['type1_0'])
    results['type0_recall'] = results['type0_0'] / (results['type0_0'] + results['type0_1'])
    results['type1_precision'] = results['type1_1'] / (results['type1_1'] + results['type0_1'])
    results['type1_recall'] = results['type1_1'] / (results['type1_1'] + results['type1_0'])
    results = results.fillna(0)
    
    results['type0_f1'] = ( 2 * results['type0_precision'] * results['type0_recall'] )/ (results['type0_precision'] + results['type0_recall'])
    results['type1_f1'] = ( 2 * results['type1_precision'] * results['type1_recall'] )/ (results['type1_precision'] + results['type1_recall'])
    results = results.fillna(0)
    
    results['overall_precision'] = ((results['type0_precision'] * results['type0_num']) + (results['type1_precision'] * results['type1_num']))/ (results['type0_num'] + results['type1_num'])
    results['overall_recall'] = ((results['type0_recall'] * results['type0_num']) + (results['type1_recall'] * results['type1_num']))/ (results['type0_num'] + results['type1_num'])
    results['overall_f1'] = ((results['type0_f1'] * results['type0_num']) + (results['type1_f1'] * results['type1_num']))/ (results['type0_num'] + results['type1_num'])
    results = results.fillna(0)
    
    return results

if __name__ == "__main__":

    input_folder = str(input("Please enter raw results' folder name: "))
    output_path = str(input("Please enter result file path: "))

    # get result file names
    result_files = get_result_file_pathes(os.path.join(os.getcwd(), input_folder))

    results = []
    for result_file in result_files:
        name = result_file.split('/')[-1]
        if (name == '.DS_Store'): continue
        print('processing: ' + name)
        
        # process raw result and append the dataframe to the results list
        results.append(process_row_data(result_file))


    aggregated_result = pd.concat(results)

    landmarks = list(set(aggregated_result.landmark_index.values))

    cols = ['overall_f1', 'landmark_index', 'type1_1', 'type1_0', 'overall_precision', 'pred', 'overall_recall', 'type0_f1', 'type0_num', 'type0_precision', 'type0_recall', 'type1_f1', 'type1_num', 'type1_precision', 'type1_recall', 'type0_1', 'type0_0']

    for l in landmarks:
        df_l = aggregated_result[aggregated_result.landmark_index == l]
        new_row={'sample_index': 'aggregated'}
        for col in cols:
            new_row[col]=np.mean(df_l[col])
        aggregated_result = aggregated_result.append(new_row, ignore_index=True)
        


    print('aggregated result shape:', aggregated_result.shape)

    aggregated_result.to_csv(output_path) 

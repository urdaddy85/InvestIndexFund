import tushare as ts
import pandas as pd
import os
import time

planDf = pd.read_csv(r'X:\本地数据\输出数据\分时数据\导出计划.csv')
planDf[["minDate"]] = planDf[["minDate"]].astype(str)
planDf[["maxDate"]] = planDf[["maxDate"]].astype(str)
planDf[["maxDateReal"]] = planDf[["maxDateReal"]].astype(str)

dfPath = 'X:\\本地数据\\输出数据\\分时数据'
exisitingFile = os.listdir(dfPath)

# 循环导出数据
ts.set_token('6aa4ece07d628c8f5e80add7f40e4afefd954a0a3031d0f3d71b6966')

for index, row in planDf.iterrows():
    tsCode = row['ts_code']
    startDate = row['minDate']
    endDate = row['maxDateReal']
    endDateName = row['maxDate']

    outputName = tsCode + '_' + startDate + '_' + endDateName + '.csv'
    if outputName not in exisitingFile:
        tempDf = ts.pro_bar(ts_code = tsCode, start_date = startDate, end_date = endDate, freq = '1min')
        if tempDf is not None:
            tempDf.to_csv(os.path.join(dfPath, outputName), index = False)
            time.sleep(10)
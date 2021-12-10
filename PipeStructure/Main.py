import subprocess

import Categorization
import Combination
import Forecasting
import Preprocessing
import Sample
import os
import pandas as pd
import DatabaseManage
import FeatureCreation
import FeatureSelection


numberOfSample=5
toCSVFile="new"
def main():
    data= HandleProcess([],numberOfSample, toCSVFile,'preProcess')
    laggedData = HandleProcess(data,numberOfSample, toCSVFile,'featureCreation')
    HandleProcess(data, numberOfSample, toCSVFile, 'featureSelection')
    HandleProcess(data,numberOfSample,toCSVFile,'dataCategorization')
    HandleProcess(data, numberOfSample, toCSVFile, 'forecasting')

def HandleProcess(data,numberOfSample,toCSVFile,process):
    if process=='dataCategorization':
        categorizationType="ABC" # farklı şekilde handle edilebilir
        if os.path.isfile('./' + process + '/' + toCSVFile +categorizationType+ str(numberOfSample) + ".csv"):
            data2 = DatabaseManage.readData(process, toCSVFile +categorizationType+ str(numberOfSample))
        else:
            data2 = Categorization.dataCategorization(data, toCSVFile, categorizationType, numberOfSample)  # return null
    elif process=='forecasting':
        forecastingType = "arima"  # farklı şekilde handle edilecek
        if os.path.isfile('./' + process + '/' + toCSVFile + forecastingType + str(numberOfSample) + ".csv"):
            data2 = DatabaseManage.readData(process, toCSVFile + forecastingType + str(numberOfSample))
        else:
            data2 = Forecasting.forecast(data, toCSVFile, forecastingType,
                                                      numberOfSample)  # return null
    elif os.path.isfile('./'+process+'/'+toCSVFile+str(numberOfSample)+".csv"):
        data2 = DatabaseManage.readData(process,toCSVFile+str(numberOfSample))
    else:
        if process=='preProcess':
            data2 = Preprocessing.preprocess(numberOfSample, toCSVFile)
        elif process=='featureCreation':
            data2 = FeatureCreation.featureCreation(data, numberOfSample, toCSVFile)
        elif process=='featureSelection':
            data2 = FeatureSelection.FeatureSelection(numberOfSample) #return null

    return data2


if __name__ == "__main__":
    main()




import subprocess

import Categorization
import Clustering
import Combination
import Forecasting
import Preprocessing
import Sample
import os
import pandas as pd
import DatabaseManage
import FeatureCreation
import FeatureSelection


numberOfSample=100
toCSVFile="new"
rScript = "/usr/local/bin/Rscript"
def main():
    data= HandleProcess([],numberOfSample, toCSVFile,'preProcess',rScript)
    laggedData = HandleProcess(data,numberOfSample, toCSVFile,'featureCreation',rScript)
    HandleProcess(data, numberOfSample, toCSVFile, 'featureSelection',rScript)
    HandleProcess(data,numberOfSample,toCSVFile,'dataCategorization',rScript)
    HandleProcess(data, numberOfSample, toCSVFile, 'clustering', rScript)
    HandleProcess(data, numberOfSample, toCSVFile, 'forecasting',rScript)

def HandleProcess(data,numberOfSample,toCSVFile,process,rScript):
    data2 = None
    if process=='clustering':
        if not os.path.isfile('./' + process + '/' + toCSVFile + str(numberOfSample) + "C_Intermittent" + ".csv"):
            Clustering.callR(numberOfSample, toCSVFile, rScript)
    if process=='dataCategorization':
        categorizationTypes=["SBC","ABC"]
        for categorizationType in categorizationTypes:
            if not os.path.isfile('./' + process + '/' + toCSVFile + str(numberOfSample) +categorizationType+ ".csv"):
                data2 = Categorization.dataCategorization(data, toCSVFile, categorizationType, numberOfSample,rScript)  # return null
        Categorization.combineCategorization(categorizationTypes,process, toCSVFile,numberOfSample)
    elif process=='forecasting':
        forecastingType = "dynamicReg"  # farklı şekilde handle edilecek
        if os.path.isfile('./' + process + '/' + toCSVFile + forecastingType + str(numberOfSample) + ".csv"):
            data2 = DatabaseManage.readData(process, toCSVFile + forecastingType + str(numberOfSample))
        else:
            data2 = Forecasting.forecast(data, toCSVFile, forecastingType,
                                                      numberOfSample,rScript)  # return null
    elif os.path.isfile('./'+process+'/'+toCSVFile+str(numberOfSample)+".csv"):
        data2 = DatabaseManage.readData(process,toCSVFile+str(numberOfSample))
    else:
        if process=='preProcess':
            data2 = Preprocessing.preprocess(numberOfSample, toCSVFile)
        elif process=='featureCreation':
            data2 = FeatureCreation.featureCreation(data, numberOfSample, toCSVFile)
        elif process=='featureSelection':
            data2 = FeatureSelection.FeatureSelection(numberOfSample,rScript) #return null


    return data2


if __name__ == "__main__":
    main()




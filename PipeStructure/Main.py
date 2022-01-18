import subprocess

import Categorization
import Clustering
import Forecasting
import Preprocessing
import Sample
import os
import pandas as pd
import DatabaseManage
import FeatureCreation
import FeatureSelection


numberOfSample=25
toCSVFile="new"
rScript = "/usr/local/bin/Rscript"
def main():
    print("PREPROCESS")
    data= HandleProcess([],numberOfSample, toCSVFile,'preProcess',rScript)
    print("FEARTURE CREATION")
    laggedData = HandleProcess(data,numberOfSample, toCSVFile,'featureCreation',rScript)
    print("DATA CATEGORIZATION")
    HandleProcess(data, numberOfSample, toCSVFile, 'dataCategorization', rScript)
    print("FEARTURE SELECTION")
    HandleProcess(data, numberOfSample, toCSVFile, 'featureSelection',rScript)
    #HandleProcess(data, numberOfSample, toCSVFile, 'clustering', rScript)
    print("FORECAST")
    #HandleProcess(data, numberOfSample, toCSVFile, 'forecast',rScript)
    print("ENSEMBLE FORECAST")
    HandleProcess(data, numberOfSample, toCSVFile, 'forecast2', rScript)


def HandleProcess(data,numberOfSample,toCSVFile,process,rScript):
    data2 = None
    if process=='clustering':
        if not os.path.isfile('./' + process + '/' + toCSVFile + str(numberOfSample) + ".csv"):
            Clustering.callR(numberOfSample, toCSVFile, rScript)
    if process=='dataCategorization':
        #categorizationTypes=["SBC","ABC"]
        categorizationTypes = ["SBC"]
        for categorizationType in categorizationTypes:
            if not os.path.isfile('./' + process + '/' + toCSVFile + str(numberOfSample) +categorizationType+ ".csv"):
                data2 = Categorization.dataCategorization(data, toCSVFile, categorizationType, numberOfSample,rScript)  # return null
        #Categorization.combineCategorization(categorizationTypes,process, toCSVFile,numberOfSample)
    elif process=='forecast':
        forecastingType = "forecasting"
        if os.path.isfile('./' + process + '/' + toCSVFile + str(numberOfSample) + ".csv"):
            data2 = DatabaseManage.readData(process, toCSVFile + str(numberOfSample))
        else:
            data2 = Forecasting.forecast(forecastingType,
                                                      numberOfSample,rScript)  # return null
    elif process == 'forecast2':
        data2 = Forecasting.forecast2(numberOfSample, rScript)  # return null
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




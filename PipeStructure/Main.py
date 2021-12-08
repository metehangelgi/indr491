import Categorization
import Combination
import Preprocessing
import Sample
import FeatureSelection
import os
import pandas as pd
import DatabaseManage
import FeatureCreation
import FeatureSelection2


numberOfSample=3
toCSVFile="new"
def main():
    data= HandleProcess([],numberOfSample, toCSVFile,'preProcess')
    laggedData = HandleProcess(data,numberOfSample, toCSVFile,'featureCreation')
    HandleProcess(data, numberOfSample, toCSVFile, 'featureSelection') # her seferinde çalışmamalı handle edeceğim
    categorizedData=Categorization.dataCategorization(data,"SBC",numberOfSample) # handle edeceğim şimdilik test için burada
    print("bitti")

def HandleProcess(data,numberOfSample,toCSVFile,process):
    if os.path.isfile('./'+process+'/'+toCSVFile+str(numberOfSample)+".csv"):
        data2 = DatabaseManage.readData(process,toCSVFile+str(numberOfSample))
    else:
        if process=='preProcess':
            data2 = Preprocessing.preprocess(numberOfSample, toCSVFile)
        elif process=='featureCreation':
            data2 = FeatureCreation.featureCreation(data, numberOfSample, toCSVFile)
        elif process=='featureSelection':
            data2 = FeatureSelection2.FeatureSelection(numberOfSample)
    return data2

def handleInitialData(numberOfSample,toCSVFile):
    if os.path.isfile('./processedData/'+toCSVFile+str(numberOfSample)+".csv"):
        data = DatabaseManage.readData('processedData',toCSVFile+str(numberOfSample))
    else:
        data = Preprocessing.preprocess(numberOfSample, toCSVFile)

    return data

def handleFeatureSelection(data,numberOfSample,toCSVFile):
    if not os.path.isfile('./featureCreation/'+toCSVFile+str(numberOfSample)+".csv"):
        dataWNewColumns =  FeatureCreation.featureCreation(data,numberOfSample, toCSVFile)
    else:
        dataWNewColumns = DatabaseManage.readData('featureCreation',toCSVFile+str(numberOfSample))
    return dataWNewColumns

def handleFeatureSelection(data,numberOfSample,toCSVFile):
    if not os.path.isfile('./featureCreation/'+toCSVFile+str(numberOfSample)+".csv"):
        dataWNewColumns =  FeatureCreation.featureCreation(data,numberOfSample, toCSVFile)
    else:
        dataWNewColumns = DatabaseManage.readData('featureCreation',toCSVFile+str(numberOfSample))
    return dataWNewColumns

if __name__ == "__main__":
    main()




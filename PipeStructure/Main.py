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
    HandleProcess(data, numberOfSample, toCSVFile, 'featureSelection')
    HandleProcess(data,numberOfSample,toCSVFile,'dataCategorization')

def HandleProcess(data,numberOfSample,toCSVFile,process):
    if os.path.isfile('./'+process+'/'+toCSVFile+str(numberOfSample)+".csv"):
        data2 = DatabaseManage.readData(process,toCSVFile+str(numberOfSample))
    else:
        if process=='preProcess':
            data2 = Preprocessing.preprocess(numberOfSample, toCSVFile)
        elif process=='featureCreation':
            data2 = FeatureCreation.featureCreation(data, numberOfSample, toCSVFile)
        elif process=='featureSelection':
            data2 = FeatureSelection2.FeatureSelection(numberOfSample) #return null
        elif process=='dataCategorization':
            # farklı categorization ları handle edeceğim
            data2 = Categorization.dataCategorization(data, "SBC", numberOfSample) #return null
    return data2


if __name__ == "__main__":
    main()




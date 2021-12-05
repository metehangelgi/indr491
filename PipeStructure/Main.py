import Categorization
import Combination
import Preprocessing
import Sample
import FeatureProcess
import os
import pandas as pd
import DatabaseManage

numberOfSample=10
toCSVFile="new"
def main():
    data=handleInitialData(numberOfSample, toCSVFile)
    #FeatureProcess.featureCreationSelection(data)
    categorizedData=Categorization.dataCategorization(data,"ABC")



def handleInitialData(numberOfSample,toCSVFile):
    if os.path.isfile('./processedData/'+toCSVFile+".csv"):
        data = DatabaseManage.readData(toCSVFile)
    else:
        data = Preprocessing.preprocess(numberOfSample, toCSVFile)

    return data

if __name__ == "__main__":
    main()




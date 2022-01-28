import math
import subprocess

import DatabaseManage
import pandas as pd

#make categorization according to the specified categorization type
#defaultly categorization for SBC is prefered
def dataCategorization(data,toCSVFile,categorization,numberOfSample,rScript):
    folder = "dataCategorization"
    DatabaseManage.createFolder(folder)
    if categorization =="ABC" :
        return abc(data,toCSVFile,numberOfSample)
    if categorization == "SBC":
        return sbc(numberOfSample,rScript)
    if categorization=="Slow-Fast":
        return slow_fast_moving(data,toCSVFile,numberOfSample)

# Returns the product ids for matching category combination
# e.g. filter_for_categoty("ABC"="A", "SPC"="intermittent")
# returns the product ids that belong to A and intermittent
def filter_for_category(categories, size):
    ids = list() #list of product ids
    pass

def label_ABCValue(row):
    return row["price"]*row["sales"]

def abc(data,toCSVFile,numberOfSample):
    idSale=data[["product_id", "sales"]]
    idPrice=data[["product_id", "price"]]
    PriceMeans = idPrice.groupby(by="product_id", sort=True).mean()
    SalesSums = idSale.groupby(by="product_id", sort=True).sum()

    JoinABC = SalesSums.merge(PriceMeans, on='product_id', how='left')
    JoinABC['price'] = JoinABC['price'].fillna(0)


    JoinABC['ABCValue'] = JoinABC.apply(lambda row: label_ABCValue(row), axis=1)
    JoinABCSorted = JoinABC.sort_values(by=['ABCValue'],ascending=False)

    productIDs=list(JoinABCSorted.index)
    lenA = math.ceil(len(productIDs) * 0.1) #%10
    lenB = math.ceil(len(productIDs) * 0.2) #%20
    productIDsA =productIDs[0:lenA]
    productIDsB=productIDs[lenA:lenB+lenA]
    productIDsC=productIDs[lenB+lenA:]

    ABCList=[]
    for PID in productIDs:
        if PID in productIDsA:
            ABCList.append('A')
        elif PID in productIDsB:
            ABCList.append('B')
        else:
            ABCList.append('C')
    ABCoutput = {'product_id': productIDs, 'ABCGroup': ABCList}
    ABCoutputDF=pd.DataFrame.from_dict(ABCoutput)
    saveCSV(toCSVFile+str(numberOfSample)+"ABC",ABCoutputDF)
    return ABCoutputDF

#SBC categorization
def sbc(numberOfSample,rScript):
    # subprocess.call (["/usr/bin/Rscript", "--vanilla", "lasso.r"])
    subprocess.call([rScript, "--vanilla", "sbc.r", str(numberOfSample)])
    return None # R codes does not return, it saves to the csv

def slow_fast_moving(data):
    pass

#if multiple categorization is done to the data, this method assigns these categorical labels to products
def combineCategorization(categorizationTypes,process, toCSVFile,numberOfSample):
    categorizations=[]
    for categorizationType in categorizationTypes:
        categorizations.append(DatabaseManage.readData(process, toCSVFile + str(numberOfSample) +categorizationType))
    InitialCat=categorizations.pop()
    for categorization in categorizations:
        InitialCat=InitialCat.join(categorization.set_index('product_id'), on='product_id')
    saveCSV(toCSVFile + str(numberOfSample) + "Combined", InitialCat)

#writing categories to csv file
def saveCSV(fName,Overall):
    folder="dataCategorization"
    DatabaseManage.createFolder(folder)
    filename="dataCategorization/"+fName+".csv"
    Overall.to_csv(filename,index=False)
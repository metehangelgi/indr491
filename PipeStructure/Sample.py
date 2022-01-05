import random
import DatabaseManage
import pandas as pd
import math

def getProdID(salesData,numberOfSamples):
    ids = salesData["product_id"]
    return random.sample(list(ids), numberOfSamples)

def getASample(toCSVFile, numberOfSample):
    data = DatabaseManage.readData('preProcess', toCSVFile + str(numberOfSample) + "PRE")
    abc(data,toCSVFile, numberOfSample)

def saveCSV(fName,Overall):
    filename="preProcess/"+fName+".csv"
    Overall.to_csv(filename,index=False)

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
    productIDsA =productIDs[0:lenA]
    newData = data[data['product_id'].isin(productIDsA)]

    saveCSV(toCSVFile+str(numberOfSample),newData)

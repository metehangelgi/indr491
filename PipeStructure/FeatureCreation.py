import csv

import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV # cross_val_score ekledim
from sklearn.linear_model import Lasso
import math
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error, r2_score
import DatabaseManage
from sklearn import linear_model
from sklearn import decomposition, datasets
from sklearn.linear_model import LassoCV
from matplotlib import pyplot as plt
from sklearn.cluster import KMeans
import re



#lag the specified features and put to the columns of their lagged versions
def lagData(data,dateData,max_lag=30):
    prodDatas = []
    prodDataSales=[]
    prodIDCols=[]
    prodsOfData = data["product_id"].unique()
    boolTest=True
    for j in range(len(prodsOfData)):
        prodData = data[data.product_id == int(prodsOfData[j])]
        """
        saleAvg = np.mean(prodData['sales'])
        if saleAvg < 20:
            continue
        """
        #prodIds.append(prodsOfData[j])
        prodData = data[207 * j:207 * (j + 1)]
        # Productlasso = {}
        # productLasso=[]
        #r = re.compile("brand_ID.*")
        #brandCols = list(filter(r.match, prodData.columns))  # Read Note below
        r = re.compile("gender.*")
        genderCols = list(filter(r.match, prodData.columns))  # Read Note below
        r = re.compile("size.*")
        sizeCols = list(filter(r.match, prodData.columns))  # Read Note below
        excludeList = ['product_id', 'sales', 'price', 'Cumartesi', 'Pazar',"kampanya_1","kampanya_2","kampanya_3","kampanya_4"]
        # excludeList=excludeList+list(dateData.columns)+brandCols+genderCols+sizeCols
        #excludeList = excludeList + brandCols + genderCols + sizeCols
        excludeList = excludeList + genderCols + sizeCols

        for column in prodData.columns:
            if column in excludeList:
                continue
            for lag in range(1, max_lag + 1):
                name = f"{column}({lag})"
                prodData[name] = prodData[column].shift(lag).copy()
        prodData = prodData.iloc[max_lag:, :]
        prodDataSale=prodData[['product_id','sales']]
        prodIDCol=prodData[['product_id']]
        #dateColumns=list(dateData.columns[2:])
        dropCols=["sales","basket", "fav", "visit", "impression", "quantity", "demand", "removeFromFav", "reviewCount", "rating"]
        prodData.drop(dropCols,axis=1, inplace=True)
        if boolTest:
            prodDatas = prodData
            prodDataSales = prodDataSale
            prodIDCols=prodIDCol
            boolTest = False
        else:
            prodDatas = pd.concat([prodDatas, prodData], ignore_index=True)
            prodDataSales = pd.concat([prodDataSales, prodDataSale], ignore_index=True)
            prodIDCols = pd.concat([prodIDCols,prodIDCol],ignore_index=True)
    return prodDatas,prodDataSales,prodIDCols

#create new features specified in the specialDates.csv
def featureCreation(datas,numberOfSample, toCSVFile):
    data = datas[
        ["product_id","brand_id","gender","SIZE_NAME","sales", "price", "basket", "fav", "visit", "impression", "quantity", "demand", "removeFromFav",
         "reviewCount", "rating"]]
    dateData = pd.read_csv("specialDates.csv")
    for column in dateData.columns:
        data[column] = np.tile(dateData[column].to_numpy(), len(datas["product_id"].unique()))

    #BrandIDCols=data['brand_id'].unique()
    #for BrandIDCol in BrandIDCols:
    #    data['brand_ID_'+str(BrandIDCol)]=0
    #    data.loc[data.brand_id == BrandIDCol,'brand_ID_'+str(BrandIDCol)]=1

    GenderCols = data['gender'].unique()
    for GenderCol in GenderCols:
        data['gender_' + str(GenderCol)] = 0
        data.loc[data.gender == GenderCol, 'gender_' + str(GenderCol)] = 1

    SIZENAMECols = data['SIZE_NAME'].unique()
    for SIZENAMECol in SIZENAMECols:
        data['size_' + str(SIZENAMECol)] = 0
        data.loc[data.SIZE_NAME == SIZENAMECol, 'size_' + str(SIZENAMECol)] = 1
    data.drop(["gender", "SIZE_NAME", "brand_id"],axis=1, inplace=True)
    laggedData,prodDataSales,prodIDCols=lagData(data,dateData)
    saveCSV(toCSVFile+str(numberOfSample),laggedData)
    saveCSV(toCSVFile + str(numberOfSample)+'Y', prodDataSales)
    saveCSV(toCSVFile + str(numberOfSample) + 'PID', prodIDCols)
    return laggedData

def saveCSV(fName,Overall):
    folder="featureCreation"
    DatabaseManage.createFolder(folder)
    filename="featureCreation/"+fName+".csv"
    Overall.to_csv(filename,index=False)


import numpy as np
import csv
import pandas as pd
import Sample
import DatabaseManage
import os
import math

def generalZeroPadding(datasID,date,dataname,rowname):
    if len(datasID[dataname]) != 0 and (date in datasID[dataname]['date'].tolist()):
        return datasID[dataname][datasID[dataname].date == date][rowname].iloc[0]
    else:
        return 0

def preprocess(numberofSamples,toCSVFile):
    datas=DatabaseManage.initializing()
    dates = sorted(datas["salesData"]["order_date"].unique())
    # numberofSamples*50 to make sure there will be enough sample after deleting no price changes, ABC
    productIDs=Sample.getProdID(datas["salesData"].sort_values(by=['order_date']),numberofSamples*50)
    #datas=datas.iloc[datas['product_id'].isin(datas)]
    processedData,processedData2=doProcess(datas,dates,numberofSamples*10,productIDs)
    saveCSV(toCSVFile+str(numberofSamples),processedData,processedData2)
    Sample.getASample(toCSVFile, numberofSamples)
    return DatabaseManage.readData('preProcess',toCSVFile+str(numberofSamples))


def doProcess(datas,dates,numberofSamples,productIDs):
    MinNumSales=20
    Overall = []
    Overall2 = []
    ColumnNames = np.loadtxt("ColumnNames.txt",dtype='str')
    Overall.append(ColumnNames) # give column names to the array
    Overall2.append(ColumnNames)  # give column names to the array
    iter = 0 # to make sure we have enough data even if no price assigned products
    savedproductIDs=[]
    savedproductIDs2 = []
    while True:
        priceAssigned=0
        Lessthan15 = False
        if iter==numberofSamples: # number of productID
            break

        if len(productIDs)!=0:
            productID=productIDs.pop(0)
        else:
            productID=Sample.getProdID(datas["salesData"].sort_values(by=['order_date']), 1)[0]
        if productID in savedproductIDs or productID in savedproductIDs2:
            continue

        datasID = {"salesData": [],
                   "basket": [],
                   "fav": [],
                   "gender": [],
                   "impression": [],
                   "price": [],
                   "demand": [],
                   "quantity": [],
                   "rating": [],
                   "removeFromFav": [],
                   "sizeAtt": [],
                   "visit": []
                   }
        keys = datas.keys()
        for key in keys:
            datasID[key] = datas[key][datas[key].product_id == productID]


        if len(datasID["price"]) == 0: # if has no price value
            continue  # discard given productID
        if 0 in datasID["price"]:
            continue

        OverallProd = []


        for date in dates:
            rowArr = [productID]
            rowArr.append(date)

            #sales
            if date in datasID["salesData"]['order_date'].tolist():
                rowArr.append(datasID["salesData"][datasID["salesData"].order_date==date]['sales'].iloc[0])
            else:
                rowArr.append(0)
            rowArr.append(datasID["salesData"]["brand_id"].iloc[0])
            rowArr.append(datasID["salesData"]["current_bu_group_name"].iloc[0])
            rowArr.append(datasID["salesData"]["current_category_name"].iloc[0])

            # price
            if len(datasID["price"][datasID["price"].created_date == date]) != 0:
                rowArr.append(datasID["price"][datasID["price"].created_date == date]["price"].iloc[0])
                priceAssigned = 1
            elif priceAssigned == 0:
                currentPriceDates = datasID["price"][datasID["price"].created_date >= date]["created_date"]
                if len(currentPriceDates) == 0:
                    rowArr.append(0)
                else:
                    currentPriceDate = min(currentPriceDates)
                    rowArr.append(datasID["price"][datasID["price"].created_date == currentPriceDate]["price"].iloc[0])
            else: #priceAssigned == 1
                currentPriceDate = max(datasID["price"][datasID["price"].created_date <= date]["created_date"])
                rowArr.append(datasID["price"][datasID["price"].created_date == currentPriceDate]["price"].iloc[0])

            #basket
            rowArr.append(generalZeroPadding(datasID,date,'basket','basket'))

            #fav
            rowArr.append(generalZeroPadding(datasID, date, 'fav', 'fav'))

            #visit
            rowArr.append(generalZeroPadding(datasID, date, 'visit', 'visit'))

            # impression
            rowArr.append(generalZeroPadding(datasID, date, 'impression', 'impression'))

            # quantity
            if len(datasID["quantity"])!=0 and date in datasID["quantity"]['date'].tolist():
                rowArr.append(datasID["quantity"][datasID["quantity"].date==date]["quantity"].iloc[0])
            else:
                rowArr.append(0)

            # quantity_demand
            rowArr.append(generalZeroPadding(datasID, date, 'demand', 'quantity_demand'))

            # removefromfav
            rowArr.append(generalZeroPadding(datasID, date, 'removeFromFav', 'remove_from_fav')) # attribute ismi dikkat!

            #review count
            rowArr.append(generalZeroPadding(datasID, date, 'rating', 'reviewCount'))  # attribute ismi dikkat!

            #rating
            if len(datasID["rating"]) != 0 and date in datasID["rating"]['date'].tolist():
                rowArr.append(datasID["rating"][datasID["rating"].date == date]["rating"].iloc[0])
                ratingAssigned = 1
            else:
                rowArr.append(0)

            #gender
            if len(datasID["gender"])!=0:
                rowArr.append(datasID["gender"]["gender"].iloc[0])
            else:
                rowArr.append(None)

            #sizeAtt
            if len(datasID["sizeAtt"])!=0:
                rowArr.append(datasID["sizeAtt"]["SIZE_NAME"].iloc[0])
                rowArr.append(datasID["sizeAtt"]["first_att"].iloc[0])
                rowArr.append(datasID["sizeAtt"]["first_att_value"].iloc[0])
                rowArr.append(datasID["sizeAtt"]["second_att"].iloc[0])
                rowArr.append(datasID["sizeAtt"]["second_att_value"].iloc[0])
                rowArr.append(datasID["sizeAtt"]["third_att"].iloc[0])
                rowArr.append(datasID["sizeAtt"]["third_att_value"].iloc[0])
            else:
                (rowArr.append(None) for i in range(7))

            OverallProd.append(rowArr)

        """OverallProd
        sData = datas["salesData"].sort_values(by=['order_date'])
        salesD = sData['sales']
        if sum(list(salesD[:round(len(salesD) * 0.8)])) < MinNumSales:
            savedproductIDs2.append(productID)
            Lessthan15 = True
        else:
            savedproductIDs.append(productID)
            iter = iter + 1
        """
        sData=[row[2] for row in OverallProd]
        if sum(list(sData[:round(len(sData) * 0.8)])) < MinNumSales:
            savedproductIDs2.append(productID)
            Lessthan15 = True
        else:
            savedproductIDs.append(productID)
            iter = iter + 1

        if Lessthan15:
            for OverallProdRow in OverallProd:
                Overall2.append(OverallProdRow)
            #Overall2.append(rowArr)
        else:
            for OverallProdRow in OverallProd:
                Overall.append(OverallProdRow)
            #Overall.append(rowArr)  # row

    return Overall,Overall2

def saveCSV(fName,Overall,Overall2):
    folder="preProcess"
    DatabaseManage.createFolder(folder)
    filename="preProcess/"+fName+"PRE.csv"
    filename2 = "preProcess/" + fName + "LessSales.csv"
    with open(filename,"w+", encoding= 'utf-8') as my_csv:
        csvWriter = csv.writer(my_csv,delimiter=',')
        csvWriter.writerows(Overall)
    with open(filename2,"w+", encoding= 'utf-8') as my_csv2:
        csvWriter = csv.writer(my_csv2,delimiter=',')
        csvWriter.writerows(Overall2)



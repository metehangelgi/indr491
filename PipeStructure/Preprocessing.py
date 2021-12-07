import numpy as np
import csv
import pandas as pd
import Sample
import DatabaseManage
import os

def generalZeroPadding(datasID,date,dataname,rowname):
    if len(datasID[dataname]) != 0 and (date in datasID[dataname]['date'].tolist()):
        return datasID[dataname][datasID[dataname].date == date][rowname].iloc[0]
    else:
        return 0

def preprocess(numberofSamples,toCSVFile):
    datas=DatabaseManage.initializing()
    dates = sorted(datas["salesData"]["order_date"].unique())
    productIDs=Sample.getProdID(datas["salesData"].sort_values(by=['order_date']),numberofSamples)
    #datas=datas.iloc[datas['product_id'].isin(datas)]
    processedData=doProcess(datas,productIDs,dates)
    saveCSV(toCSVFile+str(numberofSamples),processedData) #can be commented out
    return DatabaseManage.readData('preProcess',toCSVFile+str(numberofSamples))


def doProcess(datas,productIDs,dates):
    Overall = []
    ColumnNames = np.loadtxt("ColumnNames.txt",dtype='str')
    Overall.append(ColumnNames) # give column names to the array
    iter = 0
    for productID in productIDs:
        priceAssigned=0
        if iter==len(productIDs): # number of productID
            break
        #print(iter)
        iter=iter+1

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

            Overall.append(rowArr) # row
    return Overall

def saveCSV(fName,Overall):
    folder="preProcess"
    DatabaseManage.createFolder(folder)
    filename="preProcess/"+fName+".csv"
    with open(filename,"w+") as my_csv:
        csvWriter = csv.writer(my_csv,delimiter=',')
        csvWriter.writerows(Overall)



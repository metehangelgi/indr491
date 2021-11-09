import pickle
import numpy as np
import random
import csv

def readData(datafile):
    datafileImport="data/"+datafile
    #datafileExport="data2/"+datafile
    infile = open(datafileImport, 'rb')
    new_dict = pickle.load(infile)
    #found_dict=new_dict.loc[new_dict['product_id'].isin(random_ids)]
    #file = open(datafileExport, 'wb')
    #pickle.dump(found_dict, file)
    #file.close()
    return new_dict

salesData="TY_Koc_Sales.pkl"
basket="TY_Koc_Basket.pkl"
fav="TY_Koc_Fav.pkl"
gender="TY_Koc_Gender.pkl"
impression="TY_Koc_Impression.pkl"
price="TY_Koc_Price.pkl"
demand="TY_Koc_Quantity_Demand.pkl"
quantity="TY_Koc_Quantity.pkl"
rating="TY_Koc_Rating_Review.pkl"
removeFromFav="TY_Koc_Removefromfav.pkl"
sizeAtt="TY_Koc_Size_Atts.pkl"
visit="TY_Koc_Visit.pkl"
data=[salesData,basket,fav,gender,impression,price,demand,quantity,rating,removeFromFav,sizeAtt,visit]
datas={"salesData":[],
       "basket":[],
       "fav":[],
       "gender":[],
       "impression":[],
       "price":[],
       "demand":[],
       "quantity":[],
       "rating":[],
       "removeFromFav":[],
       "sizeAtt":[],
       "visit":[]
       }
keys=list(datas.keys())
for i in range(len(data)):
    datas[keys[i]]=readData(data[i])


dates = sorted(datas["salesData"]["order_date"].unique())
#productIDs= datas["salesData"]["product_id"].unique()

#print(len(datas["price"]['product_id'].unique()))
sales= datas["salesData"].sort_values(by=['order_date'])
ids=sales["product_id"]


rules1Problems=[]
rules2Problems=[]
rules3Problems=[]
for productID in ids:
    #rule1
    """
    productDatas=datas["salesData"][datas["salesData"].product_id==productID]
    firstOrder=min(productDatas["order_date"])

    pricesData=datas["price"][datas["price"].product_id==productID]
    if len(pricesData)==0:
        #print(pricesData)
        #print(firstOrder, " ", productID)
        rules1Problems.append(productID)
        continue
    firstChange=min(pricesData["created_date"])
    if firstOrder<firstChange:
        print(firstOrder," ",firstChange, " ", productID)
        rules1Problems.append(productID)
    """
   #rule2
    """
    pricesData = datas["price"][datas["price"].product_id == productID]
    if len(pricesData) == 0:
        continue
    ChangeDates = pricesData["created_date"]
    curPrice=0
    for ChangeDate in ChangeDates:
        priceGivenID = pricesData[pricesData.created_date == ChangeDate]
        if curPrice==0:
            curPrice=priceGivenID['price'].iloc[0]
            continue
        elif priceGivenID['price'].iloc[0]==0:
            print(productID," ", curPrice, " ", priceGivenID['price'].iloc[0])
            rules2Problems.append(productID)
    """

    #rules3
    """
    StartDate=min(datas["salesData"]['order_date'])
    EndDate=min(datas["salesData"]['order_date'])
    productDatas = datas["salesData"][datas["salesData"].product_id == productID]
    pricesData = datas["price"][datas["price"].product_id == productID]
    newGap=1
    if len(pricesData) == 0:
        continue
    ChangeDates = pricesData["created_date"]

    for ChangeDate in ChangeDates:
        priceGivenID = pricesData[pricesData.created_date == ChangeDate]
        curPrice = priceGivenID['price'].iloc[0]
        startPrice=-1
        if curPrice == 0:
            StartDate = ChangeDate
            startPrice = curPrice
            newGap=1
        else:
            EndDate = ChangeDate
            if newGap==1: # price 0 olduğu alandan + olduğu aralığa bakıyor - yeni bir 0 olmayan gelince bakmaya gerek yok.
                newGap=0
            else:
                continue
        productSalesData = productDatas[(productDatas.order_date>=StartDate) & (productDatas.order_date<EndDate)]
        if len(productSalesData)!=0:
            rules3Problems.append(productID)
            print("price of sales: ", startPrice,"sales Date ",ChangeDate)
    """
#print(rules3Problems)

Overall = {}

dates = sorted(datas["salesData"]["order_date"].unique())
productIDs= datas["salesData"]["product_id"].unique()
for productID in productIDs:
    priceAssigned=0
    for date in dates:
        Overall[productID]={date:{}}
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
            datasID[key] = datas[key][datas[key].product_id==productID]

        # şu anda product_id içermediklerini eklemedim, onları da ekle
        #sales
        if date in datasID["salesData"]['order_date']:
            Overall[productID][date]["sales"]=datasID["salesData"][datasID["salesData"].order_date==date]['sales'].iloc[0]
        else:
            Overall[productID][date]["sales"]=0
        Overall[productID][date]["brand_id"]=datasID["salesData"]["brand_id"].iloc[0]
        Overall[productID][date]["current_bu_group_name"] = datasID["salesData"]["current_bu_group_name"].iloc[0]
        Overall[productID][date]["current_category_name"] = datasID["salesData"]["current_category_name"].iloc[0]
        #basket
        if len(datasID["basket"])!=0 and (date in datasID["basket"]['date']):
            Overall[productID][date]["basket"]=datasID["basket"][datasID["basket"].date==date]["basket"].iloc[0]
        else:
            Overall[productID][date]["basket"] = 0
        #fav
        if len(datasID["fav"])!=0 and date in datasID["fav"]['date']:
            Overall[productID][date]["fav"]=datasID["fav"][datasID["fav"].date==date]["fav"].iloc[0]
        else:
            Overall[productID][date]["fav"] = 0
        #visit
        if len(datasID["visit"])!=0 and date in datasID["visit"]['date']:
            Overall[productID][date]["visit"]=datasID["visit"][datasID["visit"].date==date]["visit"].iloc[0]
        else:
            Overall[productID][date]["visit"] = 0
        # impression
        if len(datasID["impression"])!=0 and date in datasID["impression"]['date']:
            Overall[productID][date]["impression"] = datasID["impression"][datasID["impression"].date==date]["impression"].iloc[0]
        else:
            Overall[productID][date]["impression"] = 0
        # quantity
        if len(datasID["quantity"])!=0 and date in datasID["quantity"]['date']:
            Overall[productID][date]["quantity"] = datasID["quantity"][datasID["quantity"].date==date]["quantity"].iloc[0]
        else:
            Overall[productID][date]["quantity"] = 0 # bu 0 olmayabilir
        # quantity_demand
        if len(datasID["demand"])!=0 and date in datasID["demand"]['date']:
            Overall[productID][date]["quantity_demand"] = datasID["demand"][datasID["demand"].date==date]["quantity_demand"].iloc[0]
        else:
            Overall[productID][date]["quantity_demand"] = 0
        # removefromfav
        if len(datasID["removeFromFav"])!=0 and date in datasID["removeFromFav"]['date']:
            Overall[productID][date]["remote_from_fav"] = datasID["removeFromFav"][datasID["removeFromFav"].date==date]["remote_from_fav"].iloc[0]
        else:
            Overall[productID][date]["remote_from_fav"] = 0
        #rating
        if len(datasID["rating"])!=0 and date in datasID["rating"]['date']:
            Overall[productID][date]["reviewCount"] = datasID["rating"][datasID["rating"].date==date]["reviewCount"].iloc[0]
            Overall[productID][date]["rating"] = datasID["rating"][datasID["rating"].date==date]["rating"].iloc[0]
        else:
            Overall[productID][date]["reviewCount"] = 0
            Overall[productID][date]["rating"] = None
        #price
        if len(datasID["price"])==0:
            Overall[productID][date]["price"] = 0
        elif len(datasID["price"][datasID["price"].created_date==date])!=0:
            Overall[productID][date]["price"]=datasID["price"][datasID["price"].created_date==date]["price"].iloc[0]
            priceAssigned=1
        if priceAssigned==0:
            currentPriceDates=datasID["price"][datasID["price"].created_date>=date]["created_date"]
            # normalde buna gerek olmaması lazım - hata verdiği için bunu yazdım şimdilik
            if len(currentPriceDates)==0:
                Overall[productID][date]["price"]=0
            else:
                currentPriceDate=min(currentPriceDates)
                Overall[productID][date]["price"]=datasID["price"][datasID["price"].created_date==currentPriceDate]["price"].iloc[0]
        if priceAssigned==1:
            currentPriceDate = max(datasID["price"][datasID["price"].created_date <= date]["created_date"])
            Overall[productID][date]["price"] = datasID["price"][datasID["price"].created_date == currentPriceDate]["price"].iloc[0]

        #gender
        if len(datasID["gender"])!=0:
            Overall[productID][date]["gender"] = datasID["gender"]["gender"].iloc[0]
        else:
            Overall[productID][date]["gender"] = None

        #sizeAtt
        if len(datasID["sizeAtt"])!=0:
            Overall[productID][date]["size_name"] = datasID["sizeAtt"]["SIZE_NAME"].iloc[0]
            Overall[productID][date]["first_att"] = datasID["sizeAtt"]["first_att"].iloc[0]
            Overall[productID][date]["first_att_value"] = datasID["sizeAtt"]["first_att_value"].iloc[0]
            Overall[productID][date]["second_att"] = datasID["sizeAtt"]["second_att"].iloc[0]
            Overall[productID][date]["second_att_value"] = datasID["sizeAtt"]["second_att_value"].iloc[0]
            Overall[productID][date]["third_att"] = datasID["sizeAtt"]["third_att"].iloc[0]
            Overall[productID][date]["third_att_value"] = datasID["sizeAtt"]["third_att_value"].iloc[0]
        else:
            Overall[productID][date]["size_name"] = None
            Overall[productID][date]["first_att"] = None
            Overall[productID][date]["first_att_value"] = None
            Overall[productID][date]["second_att"] = None
            Overall[productID][date]["second_att_value"] = None
            Overall[productID][date]["third_att"] = None
            Overall[productID][date]["third_att_value"] = None


a_file = open("test.csv", "w")

writer = csv.writer(a_file)
for productID, dateRow in Overall.items():
    for date,values in dateRow.items():
        writer.writerow([productID,date, values])

a_file.close()









import pickle
import numpy as np
import random


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

print(len(datas["price"]['product_id'].unique()))
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
print(rules3Problems)

Overall = {}

dates = sorted(datas["salesData"]["order_date"].unique())
productIDs= datas["salesData"]["product_id"].unique()
for productID in productIDs:
    for date in dates:
        Overall[productID]={"date":date}
        if date in datas["salesData"]['order_date']:
            Overall[productID]["sales"]=datas["salesData"][datas["salesData"].product_id==productID]['sales']
        else:
            Overall[productID]["sales"]=0








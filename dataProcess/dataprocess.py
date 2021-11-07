import pickle
import numpy as np
import random
import matplotlib.pyplot as plt


def readData(datafile):
    datafileImport = "data2/" + datafile
    infile = open(datafileImport, 'rb')
    new_dict = pickle.load(infile)
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

#print(datas["salesData"]['order_date'])
saleData=datas["salesData"]
#products=saleData['product_id'].unique()
ax=saleData['product_id'].value_counts().plot(kind='bar')
ax.set_xlabel("product_id")
ax.set_ylabel("Sales")
plt.show()

ax=saleData['order_date'].value_counts().plot(kind='line')
ax.set_xlabel("order_date")
ax.set_ylabel("Sales")
plt.show()


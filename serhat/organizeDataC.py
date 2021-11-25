import pickle
import numpy as np
import random
import csv
import time
import pandas as pd

#static
#attrubute = index value (for csv and read)
product_id_no = 0
date_no = 1
sales_no = 2
brand_id_no=3
current_bu_group_name_no=4
current_category_name_no=5
price_no = 6
basket_no = 7
fav_no = 8
visit_no = 9
impression_no = 10
quantity_no = 11
quantity_demand_no = 12
remove_from_fav_no = 13
reviewCount_no = 14
rating_no = 15 #None
gender_no = 16 #None
SIZE_NAME_no = 17 #None
first_att_no = 18
first_att_value_no = 19
second_att_no = 20
second_att_value_no = 21
third_att_no = 22
third_att_value_no = 23


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

def label_ABCValue(row):
    return row["price"]*row["sales"]

dates = sorted(datas["salesData"]["order_date"].unique())
#productIDs= datas["salesData"]["product_id"].unique()

def ABCTesting(datas):
    #print(len(datas["price"]['product_id'].unique()))
    sales= datas["salesData"].sort_values(by=['order_date'])
    idPrice=datas['price'][["product_id","price"]]
    idSale=sales[["product_id","sales"]]
    PriceMeans=idPrice.groupby(by="product_id",sort=True).mean()
    SalesSums=idSale.groupby(by="product_id",sort=True).sum()
    #print(PriceMeans)
    #print(SalesSums)

    JoinABC=SalesSums.merge(PriceMeans, on='product_id', how='left')
    JoinABC['price'] = JoinABC['price'].fillna(0)
    #print(JoinABC)

    JoinABC['ABCValue'] = JoinABC.apply(lambda row: label_ABCValue(row), axis=1)
    JoinABCSorted=JoinABC.sort_values(by=['ABCValue'])
    #print(JoinABC)
    #print(JoinABCSorted)
    #print(len(JoinABCSorted))
    return JoinABCSorted.index


dates = sorted(datas["salesData"]["order_date"].unique())
productIDs= ABCTesting(datas)
lenA=int(len(productIDs)*0.05)
print(lenA)
lenB=int(len(productIDs)*0.2)
print(lenB)
productIDsA=productIDs[0:lenA]
productIDsB=productIDs[lenA:lenB+lenA]
productIDsC=productIDs[lenB+lenA:]


def Process_ABC(productIDs, dates, abcType):
    prodDF = pd.DataFrame(productIDs, columns=['product_id'])
    datesDF = pd.DataFrame(dates, columns=['date'])
    prodDF['tmp'] = 1
    datesDF['tmp'] = 1
    #JoinDateProduct = pd.merge(prodDF, datesDF, how='cross')
    JoinDateProduct = pd.merge(prodDF, datesDF, on='tmp')
    JoinDateProduct = JoinDateProduct.drop(columns=['tmp'])

    # sales
    sales = datas['salesData']
    sales = sales.drop_duplicates(subset=['product_id'], keep='last')
    sales = sales.drop(columns=['order_date'])
    #sales = pd.merge(sales, datesDF, how='cross')
    sales['tmp'] = 1
    datesDF['tmp'] = 1
    sales = pd.merge(sales, datesDF, on='tmp')
    sales = sales.drop(columns=['tmp'])
    JoinDateProductSP = pd.merge(JoinDateProduct, sales, left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['sales'] = JoinDateProductSP['sales'].fillna(0)
    sales=0
    print(JoinDateProductSP)
    # price
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['price'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'created_date'], how='left')
    JoinDateProductSP = JoinDateProductSP.drop(columns=['created_date'])
    JoinDateProductSP['price'] = JoinDateProductSP['price'].fillna(method='ffill')  # forward alongation
    JoinDateProductSP['price'] = JoinDateProductSP['price'].fillna(method='bfill')  # back alongation
    print(JoinDateProductSP)
    # basket
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['basket'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['basket'] = JoinDateProductSP['basket'].fillna(0)
    print(JoinDateProductSP)
    # fav
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['fav'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['fav'] = JoinDateProductSP['fav'].fillna(0)
    print(JoinDateProductSP)
    # visit
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['visit'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['visit'] = JoinDateProductSP['visit'].fillna(0)
    print(JoinDateProductSP)
    # impression
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['impression'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['impression'] = JoinDateProductSP['impression'].fillna(0)
    print(JoinDateProductSP)
    # quantity
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['quantity'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['quantity'] = JoinDateProductSP['quantity'].fillna(0)
    print(JoinDateProductSP)
    # quantity_demand
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['demand'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['quantity_demand'] = JoinDateProductSP['quantity_demand'].fillna(0)
    print(JoinDateProductSP)
    # removeFromFav
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['removeFromFav'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['remove_from_fav'] = JoinDateProductSP['remove_from_fav'].fillna(0)
    print(JoinDateProductSP)
    # rating
    JoinDateProductSP = pd.merge(JoinDateProductSP, datas['rating'], left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    JoinDateProductSP['reviewCount'] = JoinDateProductSP['reviewCount'].fillna(0)
    JoinDateProductSP['rating'] = JoinDateProductSP['rating'].fillna(0)
    print(JoinDateProductSP)
    # gender
    #gender = pd.merge(datas['gender'], datesDF, how='cross')
    gender=datas['gender']
    gender['tmp'] = 1
    gender = pd.merge(gender, datesDF, on='tmp')
    gender = gender.drop(columns=['tmp'])


    JoinDateProductSP = pd.merge(JoinDateProductSP, gender, left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    # JoinDateProductSP=JoinDateProductSP.drop(columns=['order_date'])
    # JoinDateProductSP['basket']=JoinDateProductSP['basket'].fillna(0)
    gender=0
    print(JoinDateProductSP)
    # sizeAtt
    # sizeAtt = pd.merge(datas['sizeAtt'], datesDF, how='cross')
    sizeAtt = datas['sizeAtt']
    sizeAtt['tmp'] = 1
    sizeAtt = pd.merge(sizeAtt, datesDF, on='tmp')
    sizeAtt = sizeAtt.drop(columns=['tmp'])
    print(JoinDateProductSP)
    JoinDateProductSP = pd.merge(JoinDateProductSP, sizeAtt, left_on=['product_id', 'date'],
                                 right_on=['product_id', 'date'], how='left')
    # JoinDateProductSP=JoinDateProductSP.drop(columns=['order_date'])
    # JoinDateProductSP['basket']=JoinDateProductSP['basket'].fillna(0)
    sizeAtt=0
    print(JoinDateProductSP)

    #write to csv
    file_name=abcType+'.csv'
    JoinDateProductSP.to_csv(file_name, sep=',',header=True)

# comment out all - to csv
#Process_ABC(productIDsA,dates,'A')
#Process_ABC(productIDsB,dates,'B')
Process_ABC(productIDsC,dates,'C')

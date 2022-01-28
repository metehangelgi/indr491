import pickle
import pandas as pd
import os

def readInitialData(datafile):
    datafileImport="data/"+datafile
    infile = open(datafileImport, 'rb')
    new_dict = pickle.load(infile)
    return new_dict

def readData(path,datafile):
    return pd.read_csv('./'+path+'/'+datafile+".csv", encoding='utf-8')

def initializing():
    salesData = "TY_Koc_Sales.pkl"
    basket = "TY_Koc_Basket.pkl"
    fav = "TY_Koc_Fav.pkl"
    gender = "TY_Koc_Gender.pkl"
    impression = "TY_Koc_Impression.pkl"
    price = "TY_Koc_Price.pkl"
    demand = "TY_Koc_Quantity_Demand.pkl"
    quantity = "TY_Koc_Quantity_new.pkl"
    rating = "TY_Koc_Rating_Review.pkl"
    removeFromFav = "TY_Koc_Removefromfav.pkl"
    sizeAtt = "TY_Koc_Size_Atts.pkl"
    visit = "TY_Koc_Visit.pkl"
    data = [salesData, basket, fav, gender, impression, price, demand, quantity, rating, removeFromFav, sizeAtt, visit]
    datas = {"salesData": [],
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
    keys = list(datas.keys())
    for i in range(len(data)):
        datas[keys[i]] = readInitialData(data[i])

    return datas

def createFolder(folder):
    # Directory
    if not os.path.isdir(folder):
        os.mkdir(folder)
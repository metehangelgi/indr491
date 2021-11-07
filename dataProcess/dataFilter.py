import pickle
import numpy as np
import random

def randomData(datafile):
    datafileImport="data/"+datafile
    infile = open(datafileImport, 'rb')
    new_dict = pickle.load(infile)
    #   print(type(new_dict))
    y = new_dict['product_id'].unique()
    random_ids = random.choices(y, k=20)
    infile.close()
    return random_ids

def getSample(datafile,random_ids):
    datafileImport="data/"+datafile
    datafileExport="data2/"+datafile
    infile = open(datafileImport, 'rb')
    new_dict = pickle.load(infile)
    found_dict=new_dict.loc[new_dict['product_id'].isin(random_ids)]
    file = open(datafileExport, 'wb')
    pickle.dump(found_dict, file)
    file.close()

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
random_ids=randomData(salesData)
for Onedata in data:
    getSample(Onedata,random_ids)








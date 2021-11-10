import pickle
import numpy as np
import random
import csv
import time

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



dates = sorted(datas["salesData"]["order_date"].unique())
#productIDs= datas["salesData"]["product_id"].unique()

#print(len(datas["price"]['product_id'].unique()))
sales= datas["salesData"].sort_values(by=['order_date'])
ids=sales["product_id"]

"""
rules1Problems=[]
rules2Problems=[]
rules3Problems=[]
for productID in ids:
    #rule1

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

   #rule2

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

#print(rules3Problems)
"""
def generalZeroPadding(datasID,date,dataname,rowname):
    if len(datasID[dataname]) != 0 and (date in datasID[dataname]['date'].tolist()):
        return datasID[dataname][datasID[dataname].date == date][rowname].iloc[0]
    else:
        return 0



Overall = []
iter=0
dates = sorted(datas["salesData"]["order_date"].unique())
productIDs= datas["salesData"]["product_id"].unique()
for productID in productIDs:
    priceAssigned=0
    ratingAssigned=0
    if iter==50: # number of productID
        break
    print(iter)
    iter=iter+1
    startDateT = time.time()

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
    getproductDataT = time.time()

    #print(datasID["fav"].sort_values('date').head(50))
    #print("getproductTime: ", getproductDataT - startDateT)

    if len(datasID["price"]) == 0:  # bu noktada bu tarihli productID discard edilebilir
        # gereksiz kontrol yerine en başta discard ettim.
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
        salesT = time.time()
        #print("SalesTime: ",  salesT- getproductDataT)

        # price
        if len(datasID["price"][datasID["price"].created_date == date]) != 0:
            rowArr.append(datasID["price"][datasID["price"].created_date == date]["price"].iloc[0])
            priceAssigned = 1
        elif priceAssigned == 0:
            currentPriceDates = datasID["price"][datasID["price"].created_date >= date]["created_date"]
            # normalde buna gerek olmaması lazım - hata verdiği için bunu yazdım şimdilik
            if len(currentPriceDates) == 0:
                rowArr.append(0)
            else:
                currentPriceDate = min(currentPriceDates)
                rowArr.append(datasID["price"][datasID["price"].created_date == currentPriceDate]["price"].iloc[0])
        else: #priceAssigned == 1
            currentPriceDate = max(datasID["price"][datasID["price"].created_date <= date]["created_date"])
            rowArr.append(datasID["price"][datasID["price"].created_date == currentPriceDate]["price"].iloc[0])

        priceT = time.time()
        #print("priceTime: ", priceT - salesT)

        #basket
        rowArr.append(generalZeroPadding(datasID,date,'basket','basket'))
        basketT = time.time()
        #print("basketTime: ", basketT- salesT)

        #fav
        #alongate etmeye gerek yok, fav sayısı 30-10-190-5-50 gibi veri gördüm.
        #                   yani bi anda düşme sebebi yok ise, bu fav günlük data olabilir kümülatif yerine
        rowArr.append(generalZeroPadding(datasID, date, 'fav', 'fav'))
        favT = time.time()
        #print("favTime: ", favT- basketT)

        #visit
        rowArr.append(generalZeroPadding(datasID, date, 'visit', 'visit'))
        visitT = time.time()
        #print("visitTime: ", visitT - favT)

        # impression
        rowArr.append(generalZeroPadding(datasID, date, 'impression', 'impression'))
        impressionT = time.time()
        #print("impressionTime: ", impressionT - visitT)

        # quantity
        #quantity zero padding değildi tam olarak o yüzden şimdilik ayrı tutuyorum.
        if len(datasID["quantity"])!=0 and date in datasID["quantity"]['date'].tolist():
            rowArr.append(datasID["quantity"][datasID["quantity"].date==date]["quantity"].iloc[0])
        else:
            rowArr.append(0) # bu 0 olmayabilir
        quantityT = time.time()
        #print("quantityTime: ", quantityT - impressionT)


        # quantity_demand
        rowArr.append(generalZeroPadding(datasID, date, 'demand', 'quantity_demand'))
        quantityDemandT = time.time()
        #print("quantityDemandTime: ", quantityDemandT - quantityT)

        # removefromfav
        rowArr.append(generalZeroPadding(datasID, date, 'removeFromFav', 'remove_from_fav')) # attribute ismi dikkat!
        removeFromFavT = time.time()
        #print("removeFromFavTime: ", removeFromFavT - quantityDemandT)


        #review count
        rowArr.append(generalZeroPadding(datasID, date, 'rating', 'reviewCount'))  # attribute ismi dikkat!

        # rating
        # altakkini kullanınca daha hızlı ama o zero padding
        """
        if len(datasID["rating"]) == 0:
            rowArr.append(None) # hiç rating yoksa ne vermeliyiz?
        elif date in datasID["rating"]['date'].tolist():
            rowArr.append(datasID["rating"][datasID["rating"].date == date]["rating"].iloc[0])
            ratingAssigned = 1
        elif ratingAssigned == 0:
            currentPriceDates = datasID["rating"][datasID["rating"].date >= date]["date"]
            # normalde buna gerek olmaması lazım - hata verdiği için bunu yazdım şimdilik
            if len(currentPriceDates) == 0:
                rowArr.append(None)
            else:
                currentPriceDate = min(currentPriceDates)
                rowArr.append(datasID["rating"][datasID["rating"].date == currentPriceDate]["rating"].iloc[0])
        else: #ratingAssigned == 1
            currentPriceDate = max(datasID["rating"][datasID["rating"].date <= date]["date"])
            rowArr.append(datasID["rating"][datasID["rating"].date == currentPriceDate]["rating"].iloc[0])
        """

        # zero padding olan rating
        if len(datasID["rating"]) != 0 and date in datasID["rating"]['date'].tolist():
            rowArr.append(datasID["rating"][datasID["rating"].date == date]["rating"].iloc[0])
            ratingAssigned = 1
        else:
            rowArr.append(0) #bu değişmeli! bir önceki rating kullanılabilir
        ratingT = time.time()
        #print("ratingTime: ", ratingT - removeFromFavT)

        #gender

        # gender None Padding
        if len(datasID["gender"])!=0:
            rowArr.append(datasID["gender"]["gender"].iloc[0])
        else:
            rowArr.append(None)
        genderT = time.time()
        #print("genderTime: ", genderT - ratingT)


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
        sizeAttT = time.time()
        #print("sizeAttTime: ", sizeAttT - genderT)

        Overall.append(rowArr) # row ekleniyor

    endDateT = time.time() # 1 product için geçen süre
    print(endDateT-startDateT)

with open("test.csv","w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows(Overall)









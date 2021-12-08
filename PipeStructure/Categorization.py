import math
import subprocess

import DatabaseManage


def dataCategorization(data,categorization,numberOfSample):
    folder = "dataCategorization"
    DatabaseManage.createFolder(folder)

    if categorization =="ABC" :
        return abc(data)
    if categorization == "SBC":
        return sbc(numberOfSample)
    if categorization=="Slow-Fast":
        return slow_fast_moving(data)

# Returns the product ids for matching category combination
# e.g. filter_for_categoty("ABC"="A", "SPC"="intermittent")
# returns the product ids that belong to A and intermittent
def filter_for_category(categories, size):
    ids = list() #list of product ids
    pass

def label_ABCValue(row):
    return row["price"]*row["sales"]

def abc(data):
    idSale=data[["product_id", "sales"]]
    idPrice=data[["product_id", "price"]]
    PriceMeans = idPrice.groupby(by="product_id", sort=True).mean()
    SalesSums = idSale.groupby(by="product_id", sort=True).sum()

    JoinABC = SalesSums.merge(PriceMeans, on='product_id', how='left')
    JoinABC['price'] = JoinABC['price'].fillna(0)


    JoinABC['ABCValue'] = JoinABC.apply(lambda row: label_ABCValue(row), axis=1)
    JoinABCSorted = JoinABC.sort_values(by=['ABCValue'],ascending=False)

    productIDs=JoinABCSorted.index
    lenA = math.ceil(len(productIDs) * 0.1) #%10
    lenB = math.ceil(len(productIDs) * 0.2) #%20
    productIDsA =productIDs[0:lenA]
    productIDsB=productIDs[lenA:lenB+lenA]
    productIDsC=productIDs[lenB+lenA:]
    ABCoutput={'A':productIDsA,'B':productIDsB,'C':productIDsC}
    return ABCoutput

def sbc(numberOfSample):
    # subprocess.call (["/usr/bin/Rscript", "--vanilla", "lasso.r"])
    subprocess.call(["/usr/local/bin/Rscript", "--vanilla", "sbc.r", str(numberOfSample)])
    return None #sonra bakacağım

def slow_fast_moving(data):
    pass
import math

def dataCategorization(data,categorization):
    if "ABC" in categorization:
        return abc(data)
    if "SBC" in categorization:
        return sbc(data)
    if "Slow-Fast" in categorization:
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

def sbc(data):
    pass

def slow_fast_moving(data):
    pass
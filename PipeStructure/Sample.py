import random

def getProdID(salesData,numberOfSamples):
    ids = salesData["product_id"]
    return random.sample(list(ids), numberOfSamples)
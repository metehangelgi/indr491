import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV # cross_val_score ekledim
from sklearn.linear_model import Lasso
import math
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error, r2_score
import DatabaseManage
from sklearn import linear_model
from sklearn import decomposition, datasets
from sklearn.linear_model import LassoCV
from matplotlib import pyplot as plt
from sklearn.cluster import KMeans


def featureCreationSelection(datas):
    data = datas[
        ["product_id", "sales", "price", "basket", "fav", "visit", "impression", "quantity", "demand", "removeFromFav",
         "reviewCount", "rating"]]
    dateData = pd.read_csv("specialDates.csv")
    for column in dateData.columns:
        data[column] = np.tile(dateData[column].to_numpy(), len(datas["product_id"].unique()))
    col_names = []
    prodIds = []
    Xs = []
    Ys = []
    metrics, X_train, y_train, X_test, y_test, Xs, Ys,col_names = manipulation_and_search(prodIds,data,col_names,len(data),Xs,Ys)
    coeff2=metricExtraction(metrics, prodIds)
    prod = ['product_id']
    names = np.concatenate((prod, col_names))
    coefDF = pd.DataFrame(data=coeff2, columns=names)
    writeToCSV(coefDF,Xs,Ys)

def writeToCSV(coefDF,Xs,Ys):
    folder = "featureSelection"
    DatabaseManage.createFolder(folder)
    filename = folder+"/Lasso.csv"
    coefDF.to_csv(filename, sep=',', header=True, index=False)
    fileX = folder+"/A-LassoX.csv"
    Xs.to_csv(fileX, sep=',', header=True, index=False)
    fileY = folder+"/A-LassoY.csv"
    Ys.to_csv(fileY, sep=',', header=True, index=False)

def metricExtraction(metrics,prodIds):
    # metric extraction
    coefficients = []
    index = 0
    # prods=data["product_id"].unique()
    # prodIds
    for metric in metrics:
        Precoeffs = metric[1]
        prodID = prodIds[index]
        Postcoeffs = [f"{prodID}"]
        for Precoeff in Precoeffs:
            if Precoeff != 0.0:
                Postcoeffs.append(1.0)
            else:
                Postcoeffs.append(0.0)
        coefficients.append(Postcoeffs)
        index += 1
    coeff2 = coefficients[:]
    return coeff2

# Returns logarithmic range between start power to end power
# Alternative to np.logspace(10e-5, 10, 100, endpoint=True)
def log_range(start,end):
    lst = []
    for i in range(start-end+1):
        power = start - i
        unit = 10**power
        for j in range(9):
            lst.append(unit*(j+1))
    return lst


def searching(X, y):
    alpha_search_range = log_range(2, -6)
    ## en iyi alpha değer için araştırma yapıyor
    pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('model', Lasso())
    ])

    # Searching for best alpha
    search = GridSearchCV(pipeline,
                          {'model__alpha': alpha_search_range},  # burayı nasıl değiştireceğimi anlamadım tam
                          scoring="neg_mean_squared_error", verbose=0
                          )
    # fit model
    search.fit(X, y)
    coefficients = search.best_estimator_.named_steps['model'].coef_
    # print(search.best_estimator_,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    return (search.best_estimator_.named_steps['model'].get_params()['alpha'], coefficients)

def regression(metrics, X_train, y_train, X_test, y_test):
    productLasso = []
    for metric in metrics:
        alph= metric[0] # burada en uygun alpha değerine göre lasso yapıyor ve sonuçları alıyor
        lasso= Lasso(alpha=alph)
        lasso.fit(X_train, y_train)
        y_train_pred= lasso.predict(X_train)
        y_test_pred= lasso.predict(X_test)
        productLasso.append((mean_absolute_percentage_error(y_train,y_train_pred), mean_absolute_percentage_error(y_test,y_test_pred)))
    return productLasso

def z_normal(col):
    if(np.std(col) == 0):
        return col
    return (col-np.mean(col))/np.std(col)

def manipulation_and_search(prodIds,data,col_names,num_prod,Xs,Ys, max_lag=7):
    boolTest = True
    metrics = []
    count = 0  ## kaç ürün için lasso yapılacak diye tutuldu
    prodsOfData = data["product_id"].unique()
    for j in range(len(prodsOfData)):

        prodData = data[data.product_id == int(prodsOfData[j])]
        """
        saleAvg = np.mean(prodData['sales'])
        if saleAvg < 20:
            continue
        """
        prodIds.append(prodsOfData[j])
        count += 1
        prodData = data[207 * j:207 * (j + 1)]
        # Productlasso = {}
        # productLasso=[]
        for column in prodData.columns:
            if column in ['product_id', 'sales', 'price', 'haftasonu', 'kampanya', 'resmitatil', 'ozelGun', 'Ozel3',
                          'Ozel5']: continue
            for lag in range(1, max_lag + 1):
                name = f"{column}({lag})"
                prodData[name] = prodData[column].shift(lag).copy()
        X = prodData.iloc[max_lag:, 2:]
        X.drop(["basket", "fav", "visit", "impression", "quantity", "demand", "removeFromFav", "reviewCount", "rating"],
               axis=1, inplace=True)
        if count == 1:
            col_names = X.columns  # get column names
        y = prodData["sales"][max_lag:]
        X_train, X_test = np.split(X, [int(.8 * len(X))])
        y_train, y_test = np.split(y, [int(.8 * len(y))])
        # data for dynamic regression
        X = pd.concat([X_train, X_test], ignore_index=True)
        Y = pd.concat([pd.DataFrame(y_train, columns=['sales']), pd.DataFrame(y_test, columns=['sales'])],
                      ignore_index=True)
        X.insert(0, 'product_id', f"{prodsOfData[j]}")
        Y.insert(0, 'product_id', f"{prodsOfData[j]}")
        if boolTest:
            Xs = X
            Ys = Y
            boolTest = False
        else:
            Xs = pd.concat([Xs, X], ignore_index=True)
            Ys = pd.concat([Ys, Y], ignore_index=True)
        # normalize
        for name in X_train.columns:
            X_train[name] = z_normal(X_train[name].copy())
            X_test[name] = z_normal(X_test[name].copy())
        y_train = z_normal(y_train.copy())
        y_test = z_normal(y_test.copy())

        results = searching(X_train, y_train)
        metrics.append(results)

        if count == num_prod:  # burada ürün sayısı belirtilecek
            break

    return metrics, X_train, y_train, X_test, y_test, Xs, Ys,col_names


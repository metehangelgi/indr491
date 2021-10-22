#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 22 09:46:54 2021

@author: metehangelgi
"""
import pmdarima as pm
from pmdarima.model_selection import train_test_split
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.arima_model import ARIMA
from statistics import mean
from math import sqrt
import pandas as pd

# import etmeyi başarana kadar serhattan ödünç aldım.  
def static_test(actual,forecast):
    """
    Static test function takes lists of actual and forecast values and returns
    MAE, MAPE, MSE, and RMSE. It requires non-zero list of values for actual. 
    Called static, since it is contrasted to deep learning testing schemes which
    are dynamic in nature.
    """
    AssertSameLen(actual, forecast)
    MAE = mean(abs(x - y) for x,y in zip(actual, forecast))
    MSE = mean((x - y)**2 for x,y in zip(actual, forecast))
    RMSE = sqrt(MSE)
    if (0 in actual):
        raise ZeroDivisionError("List of actual values has 0. It breakes MAPE.")
    else:
        MAPE = 100*mean(abs((x-y)/x) for x,y in zip(actual,forecast))

    print("Test results:")
    print(f"MAE = {MAE}\tMAPE = {MAPE}\tMSE = {MSE}\tRMSE = {RMSE}")
    print(f"Where len(actual)={len(actual)}  len(forecast)={len(forecast)}")
    
    print("...")

    return MAE, MAPE, MSE, RMSE

def AssertSameLen(ls1, ls2):
    assert (len(ls1) == len(ls2)), "Arguments must be in same in same length"

# if P Value > 0.05 we go ahead with finding the order of differencing.

def find_d(y):
    d=0
    result = adfuller(y)
    ADF=result[0]
    pValue=result[1]
    print('ADF Statistic: %f' % ADF)
    print('p-value: %f' % pValue)
    if pValue > 0.05:
        # Original Series
        fig, axes = plt.subplots(3, 2, sharex=True)
        axes[0, 0].plot(y); axes[0, 0].set_title('Original Series')
        plot_acf(y, ax=axes[0, 1])
    
        # 1st Differencing
        axes[1, 0].plot(np.diff(y)); axes[1, 0].set_title('1st Order Differencing')
        plot_acf(np.diff(y), ax=axes[1, 1])
        # 2nd Differencing
        axes[2, 0].plot(np.diff(np.diff(y))); axes[2, 0].set_title('2nd Order Differencing')
        plot_acf(np.diff(np.diff(y)), ax=axes[2, 1])
    
        plt.show()
        
        ## Adf Test
        adf_d= pm.arima.ndiffs(y, test='adf')
    
        # KPSS test
        kpss_d= pm.arima.ndiffs(y, test='kpss')
        d = kpss_d
    return d
        
def find_p(train):
    # PACF plot of 1st differenced series
    plt.rcParams.update({'figure.figsize':(9,3), 'figure.dpi':120})

    fig, axes = plt.subplots(1, 2, sharex=True)
    axes[0].plot(np.diff(train)); axes[0].set_title('1st Differencing')
    axes[1].set(ylim=(0,5))
    plot_pacf(np.diff(train), ax=axes[1])
    plt.title("p Value",None)
    plt.show()

def find_q(train):
    
    fig, axes = plt.subplots(1, 2, sharex=True)
    axes[0].plot(np.diff(train)); axes[0].set_title('1st Differencing')
    axes[1].set(ylim=(0,1.2))
    plot_acf(np.diff(train), ax=axes[1])
    plt.title("q Value",None)
    plt.show()

def find_arima(p,d,q,train):
    model = ARIMA(train, order=(p,d,q))
    model_fit = model.fit(disp=0)
    print(model_fit.summary())
    return model_fit


# evaluate an ARIMA model for a given order (p,d,q)
def evaluate_arima_model(X, arima_order):
	# prepare training dataset
	train_size = int(len(X) * 0.66)
	train, test = X[0:train_size], X[train_size:]
	history = [x for x in train]
	# make predictions
	predictions = list()
	for t in range(len(test)):
		model = ARIMA(history, order=arima_order)
		model_fit = model.fit()
		yhat = model_fit.forecast()[0]
		predictions.append(yhat)
		history.append(test[t])
	# calculate out of sample error
	MAE, MAPE, MSE, RMSE = static_test(test, predictions)
	return MAE    


# evaluate combinations of p, d and q values for an ARIMA model
def evaluate_models(dataset, p_values, d_values, q_values):
	dataset = dataset.astype('float32')
	best_score, best_cfg = float("inf"), None
	for p in p_values:
		for d in d_values:
			for q in q_values:
				order = (p,d,q)
				try:
					mse = evaluate_arima_model(dataset, order)
					if mse < best_score:
						best_score, best_cfg = mse, order
					print('ARIMA%s MSE=%.3f' % (order,mse))
				except:
					continue
	print('Best ARIMA%s MSE=%.3f' % (best_cfg, best_score))
    
    
def method1(y,train,tests):
    #d=find_d(train)
    #evaluate_models(y,[1,2,3,4,5],d,[1,2,3,4,5])
    #MAE=evaluate_arima_model(y,(1,1,2))
    #print(MAE)
    pass

def method2(y,train,tests):
    d= find_d(y)
    find_p(y)
    find_q(y)
    model_fit=find_arima(1,1,2,train)
    # Forecast
    fc, se, conf = model_fit.forecast(tests.shape[0], alpha=0.05)  # 95% conf
    
    # Make as pandas series
    fc_series = pd.Series(fc, index=None)
    lower_series = pd.Series(conf[:, 0], index=None)
    upper_series = pd.Series(conf[:, 1], index=None)
    
    # Plot
    plt.figure(figsize=(12,5), dpi=100)
    plt.plot(train, label='training')
    plt.plot(tests, label='actual')
    plt.plot(fc_series, label='forecast')
    plt.fill_between(lower_series.index, lower_series, upper_series, 
                     color='k', alpha=.15)
    plt.title('Forecast vs Actuals')
    plt.legend(loc='upper left', fontsize=8)
    plt.show()
    
def main():
    
    # read data into memory
    data_set = np.genfromtxt("PriceData.csv", delimiter = ",",skip_header=True)
    data=pd.read_csv("Price.csv")
    print(data.dtypes)
    y=data['Price'].values
    dates=data['Date'].values
    train_size = int(len(y) * 0.75)
    train = y[0:train_size]
    tests = y[train_size:]

    #print(values)
    #train=pd.DataFrame(train)
    #tests=pd.DataFrame(tests)
    #method1(y,train,tests)
    #method2(y,train,tests)
    # both methods failed 
    
    
if __name__ == "__main__":
    main()
    

    
    
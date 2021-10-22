#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 22 11:29:38 2021

@author: metehangelgi
"""

import numpy as np, pandas as pd
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import adfuller
from numpy import log
from pmdarima.arima.utils import ndiffs
import pmdarima as pm
from statsmodels.tsa.arima_model import ARIMA


def find_d(df):
    d=0
    result = adfuller(df.value.dropna())
    ADF=result[0]
    pValue=result[1]
    print('ADF Statistic: %f' % ADF)
    print('p-value: %f' % pValue)
    if pValue > 0.05:
        """
        # Original Series
        fig, axes = plt.subplots(3, 2, sharex=True)
        axes[0, 0].plot(df.value); axes[0, 0].set_title('Original Series')
        plot_acf(df.value, ax=axes[0, 1])
        
        # 1st Differencing
        axes[1, 0].plot(df.value.diff()); axes[1, 0].set_title('1st Order Differencing')
        plot_acf(df.value.diff().dropna(), ax=axes[1, 1])
        
        # 2nd Differencing
        axes[2, 0].plot(df.value.diff().diff()); axes[2, 0].set_title('2nd Order Differencing')
        plot_acf(df.value.diff().diff().dropna(), ax=axes[2, 1])
        
        plt.show()
        """
        ## Adf Test
        adf_d= pm.arima.ndiffs(df.value.dropna(), test='adf')
    
        # KPSS test
        kpss_d= pm.arima.ndiffs(df.value.dropna(), test='kpss')
        d = kpss_d
        print(d)
    return d

def find_p(df):
    # PACF plot of 1st differenced series
    plt.rcParams.update({'figure.figsize':(9,3), 'figure.dpi':120})
    
    fig, axes = plt.subplots(1, 2, sharex=True)
    axes[0].plot(df.value.diff()); axes[0].set_title('1st Differencing')
    axes[1].set(ylim=(0,5))
    plot_pacf(df.value.diff().dropna(), ax=axes[1])
    
    plt.show()

def find_q(df):
    
    fig, axes = plt.subplots(1, 2, sharex=True)
    axes[0].plot(df.value.diff()); axes[0].set_title('1st Differencing')
    axes[1].set(ylim=(0,1.2))
    plot_acf(df.value.diff().dropna(), ax=axes[1])
    
    plt.show()

def train_arima(train,p,d,q):
    model = ARIMA(train, order=(p,d,q))
    model_fit = model.fit(disp=-1)
    print(model_fit.summary())
    return model_fit

def build_arima(train,p,d,q):
    model_fit=train_arima(train,p,d,q)
    # Plot residual errors
    residuals = pd.DataFrame(model_fit.resid)
    fig, ax = plt.subplots(1,2)
    residuals.plot(title="Residuals", ax=ax[0])
    residuals.plot(kind='kde', title='Density', ax=ax[1])
    plt.show()
    
    # Actual vs Fitted
    model_fit.plot_predict(dynamic=False)
    plt.show()
    return model_fit
    

def method1(df):
    # Forecast
    train_size = int(len(df) * 0.85)
    train = df.value[:train_size]
    test = df.value[train_size:]
    
    fitted=build_arima(train,1,1,1) # it gets error when using find_d method
    fc, se, conf = fitted.forecast(len(test), alpha=0.05)  # 95% conf
    
    # Make as pandas series
    fc_series = pd.Series(fc, index=test.index)
    lower_series = pd.Series(conf[:, 0], index=test.index)
    upper_series = pd.Series(conf[:, 1], index=test.index)
    
    # Plot
    plt.figure(figsize=(12,5), dpi=100)
    plt.plot(train, label='training')
    plt.plot(test, label='actual')
    plt.plot(fc_series, label='forecast')
    plt.fill_between(lower_series.index, lower_series, upper_series, 
                     color='k', alpha=.15)
    plt.title('Forecast vs Actuals')
    plt.legend(loc='upper left', fontsize=8)
    plt.show()
    
def main():
    # Import data
    df = pd.read_csv('https://raw.githubusercontent.com/selva86/datasets/master/wwwusage.csv', names=['value'], header=0)
    # Create Training and Test
    method1(df)


if __name__ == "__main__":
    main()

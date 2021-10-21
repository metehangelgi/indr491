#!/usr/bin/env python3

# @author Serhat COŞKUN
# @mainpage ErrorMetrics
# @file ErrorMetrics.py
# I@brief Commonly used error metrics' definitions

from statistics import mean
from math import sqrt


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

if __name__ == "__main__":
    pass






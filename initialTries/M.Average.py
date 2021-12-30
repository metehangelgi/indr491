# -*- coding: utf-8 -*-
"""
Created on Sun Oct 24 21:20:22 2021

@author: hteki
"""
import pandas as pd

numbers = [1, 2, 3, 7, 9, 5]
window_size = 3
numbers_series = pd.Series(numbers)
windows = numbers_series.rolling(window_size)
moving_averages = windows.mean()

moving_averages_list = moving_averages.tolist()
without_nans = moving_averages_list[window_size - 1:]

print(without_nans)
    
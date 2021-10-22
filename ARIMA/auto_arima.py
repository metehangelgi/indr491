import pmdarima as pm
from pmdarima.model_selection import train_test_split
import numpy as np
import matplotlib.pyplot as plt

# Load/split your data
y = pm.datasets.load_wineind()
train, tests = train_test_split(y, train_size=150)
print(y)



def find_arima(train):

    # estimate number of seasonal differences using a Canova-Hansen test
    D = pm.arima.nsdiffs(train ,
                         m=12,  # commonly requires knowledge of dataset
                         max_D=12)
    
    # estimate number of seasonal differences using a Canova-Hansen test
    d = pm.arima.ndiffs(train ,
            max_d=12) 
    
    stepwise_fit = pm.auto_arima(train, start_p=1, start_q=1,
                                 max_p=3, max_q=3, m=12,
                                 start_P=0, seasonal=True,
                                 d=d, D=D, trace=True,
                                 error_action='ignore',  # don't want to know if an order does not work
                                 suppress_warnings=True,  # don't want convergence warnings
                                 stepwise=True)  # set to stepwise


    # make your forecasts
    forecasts = stepwise_fit.predict(tests.shape[0])  # predict N steps into the future
    return stepwise_fit,forecasts


stepwise_fit,forecasts = find_arima(train)
# Visualize the forecasts (blue=train, green=forecasts)
x = np.arange(y.shape[0])
plt.plot(x[:150], train, c='blue')
plt.plot(x[150:], np.stack(test+10 for test in tests), c='blue')
plt.plot(x[150:], forecasts, c='green')

plt.show()

print(stepwise_fit.summary())
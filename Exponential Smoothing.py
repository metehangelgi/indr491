def simple_exp_smooth(d,extra_periods=1,alpha=0.9):  
  d = np.array(d)  # Transform the input into a numpy array  
  cols = len(d)  # Historical period length  
  d = np.append(d,[np.nan]*extra_periods)  # Append np.nan into the demand array to cover future periods
  f = np.full(cols+extra_periods,np.nan)  # Forecast array  
  f[1] = d[0]  # initialization of first forecast  
  # Create all the t+1 forecasts until end of historical period  
  for t in range(2,cols+1):  
    f[t] = alpha*d[t-1]+(1-alpha)*f[t-1]  
  f[cols+1:] = f[t]  # Forecast for all extra periods 
  df = pd.DataFrame.from_dict({"Demand":d,"Forecast":f,"Error":d-f})
  return df
  
  
import numpy as np 
import pandas as pd
d=[28,19,18,13,19,16,19,18,13,16,16,11,18,15,13,15,13,11,13,10,12]
df = simple_exp_smooth(d,extra_periods=4)
print(df)


MAE = df["Error"].abs().mean()  
print("MAE:",round(MAE,2)) 
RMSE = np.sqrt((df["Error"]**2).mean())
print("RMSE:",round(RMSE,2))


df.index.name = "Periods"
df[["Demand","Forecast"]].plot(figsize=(8,3),title="Simple Smoothing",ylim=(0,30),style=["-","--"])

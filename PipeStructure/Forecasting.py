import subprocess
import DatabaseManage


def forecast(data,toCSVFile,forecastType,numberOfSample,rScript):
    folder = "forecast"
    DatabaseManage.createFolder(folder)
    forecastType = forecastType+".r"
    subprocess.call([rScript, "--vanilla", 'forecasting.r', str(numberOfSample)])
    return None  # R codes does not return, it saves to the csv
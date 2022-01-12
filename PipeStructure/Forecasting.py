import subprocess
import DatabaseManage


def forecast(data,toCSVFile,forecastType,numberOfSample,rScript):
    folder = "forecast"
    DatabaseManage.createFolder(folder)
    forecastType = forecastType+".r"
    subprocess.call([rScript, "--vanilla", forecastType, str(numberOfSample)])
    return None  # R codes does not return, it saves to the csv

def forecast2(data,toCSVFile,numberOfSample,rScript):
    folder = "forecast"
    DatabaseManage.createFolder(folder)
    subprocess.call([rScript, "--vanilla", "ensemble.r", str(numberOfSample)])
    return None  # R codes does not return, it saves to the csv
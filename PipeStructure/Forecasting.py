import subprocess
import DatabaseManage


def forecast(data,toCSVFile,forecastType,numberOfSample):
    folder = "forecast"
    DatabaseManage.createFolder(folder)
    forecastType = forecastType+".r"
    subprocess.call(["/usr/local/bin/Rscript", "--vanilla", forecastType, str(numberOfSample)])
    return None  # R codes does not return, it saves to the csv
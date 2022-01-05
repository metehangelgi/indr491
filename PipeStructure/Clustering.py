import math
import subprocess

import DatabaseManage
import pandas as pd

def callR(numberOfSample, toCSVFile,rScript):
    folder = "clustering"
    DatabaseManage.createFolder(folder)
    subprocess.call([rScript, "--vanilla", "clustering.r", str(numberOfSample)])
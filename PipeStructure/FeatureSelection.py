import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV # cross_val_score ekledim
from sklearn.linear_model import Lasso
import math
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error, r2_score
import DatabaseManage
from sklearn import linear_model
from sklearn import decomposition, datasets
from sklearn.linear_model import LassoCV
from matplotlib import pyplot as plt
from sklearn.cluster import KMeans
import subprocess


def FeatureSelection(numberOfSample,rScript):
    folder = "featureSelection"
    DatabaseManage.createFolder(folder)
    #subprocess.call (["/usr/bin/Rscript", "--vanilla", "lasso.r"])
    subprocess.call([rScript, "--vanilla", "lasso.r" , str(numberOfSample)])

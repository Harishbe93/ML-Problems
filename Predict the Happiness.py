# -*- coding: utf-8 -*-
"""
Created on Fri Sep 29 12:18:57 2017

@author: HA294046
"""
import os
path = "F:\Mach. Learning\Kaggle case studies\Predict the Happiness"
os.chdir()
os.getcwd()

import numpy as np
import pandas as pd
from nltk.corpus import stopwords
from nltk.stem import PorterStemmer

data = pd.read_csv(path+"/train.csv")
test = pd.read_csv(path+"/test.csv")

stops = set(stopwords.words("english"))

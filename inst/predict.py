# --make predictions

import pandas as pd
import numpy as np
import tensorflow as tf
import keras as keras
from sklearn import metrics

def predict_model(transformer_model, test_token_x, test_token_y):
  y_pred = np.argmax(transformer_model.predict(test_token_x), axis=1)

# --calculate metrics
  accuracy = metrics.accuracy_score(test_token_y, y_pred)
  precision, recall, fscore, _ = metrics.precision_recall_fscore_support(
    test_token_y, y_pred, average="weighted")

# --save results

  results_model = pd.DataFrame([{
        "accuracy":accuracy,
        "fscore": fscore, 
        "precision": precision,
        "recall": recall}])
  
  # results_model.to_csv("results_model.csv", index=False)

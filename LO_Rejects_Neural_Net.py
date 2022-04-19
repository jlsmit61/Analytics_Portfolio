from asyncore import write
from cProfile import label
from datetime import datetime
from operator import index
from pickle import TRUE
from tabnanny import verbose
from tkinter import Grid, Variable
from turtle import tiltangle
from typing import Sequence
from unittest import result
import matplotlib
from matplotlib import dates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from datetime import datetime, date, time as dt
from skimpy import skim

Rejects_imp = 'Rejects_Analysis.csv'

Rejects = pd.read_csv(Rejects_imp)
skim(Rejects)
Rejects['DocuSigned_Used'] = np.where(Rejects['DocuSign_Used'] == 'TRUE', 1, 0)
Rejects['Reject'] = np.where(Rejects['Reject'] == 'Reject', 1, 0)
Rejects['DocuSign_Used'] = np.where(Rejects['DocuSign_Used'] == 'TRUE', 1, 0)

Rejects['DS_Expiration_SameDay'] = np.logical_and(Rejects['DocuSign_Used'] == 1, Rejects['Same Day'] == 1)
Rejects['DS_Expiration_SameDay'] = np.where((Rejects['DS_Expiration_SameDay'] == True), 1, 0)
Rejects['DS_Expiration_Days'] = np.logical_and(Rejects['DocuSign_Used'] == 1, Rejects['2 to 5 Days'] == 1)
Rejects['DS_Expiration_Days'] = np.where((Rejects['DS_Expiration_Days'] == True), 1, 0)
Rejects['DS_Expiration_1wk'] = np.logical_and(Rejects['DocuSign_Used'] == 1, Rejects['Over 1 Week'] == 1)
Rejects['DS_Expiration_1wk'] = np.where((Rejects['DS_Expiration_1wk'] == True), 1, 0)
Rejects['DS_Expiration_2wks'] = np.logical_and(Rejects['DocuSign_Used'] == 1, Rejects['Over 2 Weeks'] == 1)
Rejects['DS_Expiration_2wks'] = np.where((Rejects['DS_Expiration_2wks'] == True), 1, 0)
Rejects['DS_Expiration_3wks'] = np.logical_and(Rejects['DocuSign_Used'] == 1, Rejects['Over 3 Weeks'] == 1)
Rejects['DS_Expiration_3wks'] = np.where((Rejects['DS_Expiration_3wks'] == True), 1, 0)
Rejects['DS_Expiration_Month'] = np.logical_and(Rejects['DocuSign_Used'] == 1, Rejects['Over 1 Month'] == 1)
Rejects['DS_Expiration_Month'] = np.where((Rejects['DS_Expiration_Month'] == True), 1, 0)

Rejects['NAME'] = np.where((Rejects['NAME'] == 'AACR Approval Subprocess'), 'AACR', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'AACR Manual Update Process'), 'AACR', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Account Documentation Submission Process'), 'ADS', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Account Retitling Process'), 'Acct Retitle', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'ACH Instructions'), 'AINS', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Advisory Goal Tool Process'), 'AGT', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Advisory Program Schedule Generation'), 'AWA', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Advisory Target Allocation Process'), 'ATAP', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Advisory Workflow Application (AWA)'), 'AWA', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Annual Advisory Client Review (AACR)'), 'AACR', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'AWA Subsession'), 'AWA', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Beneficiary Change Process'), 'Bene Change', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Blank Form Packet Creator'), 'AO', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Client Transfer Application'), 'CTA', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'CostBasis Process'), 'Cost Basis', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Create Bulk PDF of CRT Letters'), 'CRT', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'DRS_DRIP Process'), 'DRS_DRP', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'DTC (Free Delivery) Request Process'), 'DTC', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'DWAC Subprocess'), 'DWAC', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Fee Waiver Process'), 'Fee Waiver', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'FI Allocation Account Opening'), 'Fixed Income', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'FI Master Account Opening'), 'Fixed Income', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Find Existing Pre-sale Disclosure'), 'Retirement Plans', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Household Maintenance'), 'HM', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'IER Account Opening Process'), 'IER', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Insurance Submission Process'), 'Ins Submission', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'IRA Simplifiers Process'), 'AO', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'LiquidOffice ACAT Process'), 'ACAT', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Margin Option Process'), 'M/O', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Mutual Fund Networking Request Process'), 'MFNR', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Open an account'), 'AO', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Periodic Investment Purchase and Sell (PIPS)'), 'PIPS', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'PIM Composite Update Process'), 'PIM Comp Update', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Qualified Plan Account Opening'), 'Retirement Plans', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Qualified Plan Maintenance'), 'Retirement Plans', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Qualified Plan Pre-sale Disclosure Process'), 'Retirement Plans', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Review Previous Day Repurchase Trades'), 'Review Prev Day Trades', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Schedule C Update Subprocess'), 'Schedule C', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'SSI Approval Subprocess'), 'SSI', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Statement Household Maintenance'), 'HM', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Supervision Group Acceptance Process'), 'AO', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Systematic and Standing Instructions Process'), 'SSI', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Tax Gain or Loss Sub Process'), 'Tax GL', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Trade Correction Process'), 'Trade Corr', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Transfer to Client Resource Team Process'), 'TCRT', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'UTMA UGMA Exception State Registration Process'), 'UTMA Excep', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'UTMA UGMA Freeze Subprocess'), 'UTMA Retitle', Rejects['NAME'])
Rejects['NAME'] = np.where((Rejects['NAME'] == 'Verbal Account Opening Approval Subprocess'), 'AO', Rejects['NAME'])

df = Rejects['NAME'].str.get_dummies()
Rejects = pd.concat([Rejects, df], axis=1)


Rejects_pid = Rejects.drop(['PROCESS_ID', 'NAME', 'Start_Date', 'End_Date', 'Start_Day', 'Start_Month', 'End_Day', 'End_Month', 'DocuSigned_Used'], axis=1)
Rejects = Rejects_pid

skim(Rejects)



#Split into train, validation, and test sets
from sklearn.model_selection import train_test_split

features = Rejects.drop('Reject', axis=1)
labels = Rejects['Reject']

X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size=.2, random_state=42)
X_val, X_test, y_val, y_test = train_test_split(X_test, y_test, test_size=.5, random_state=42)

for dataset in [y_train, y_val, y_test]:
    print(round(len(dataset)/ len(labels), 2))

X_train.to_csv('rejects_train_features.csv', index = False)
X_val.to_csv('val_features.csv', index = False)
X_test.to_csv('test_features.csv', index = False)

y_train.to_csv('rejects_train_labels.csv', index = False)
y_val.to_csv('val_labels.csv', index = False)
y_test.to_csv('test_lables.csv', index = False)


import joblib
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
#Grid Seach assist in hyperparameter tuning
import warnings
warnings.filterwarnings('ignore', category=FutureWarning)
warnings.filterwarnings('ignore', category=DeprecationWarning)

tr_features = pd.read_csv('rejects_train_features.csv')
tr_labels = pd.read_csv('rejects_train_labels.csv')

#Create function to neatly present best parameteres when testing models
def print_results(results):
    print('Best Params: {}\n'.format(results.best_params_))

    means = results.cv_results_['mean_test_score']
    stds = results.cv_results_['std_test_score']
    for mean, std, params in zip(means, stds, results.cv_results_['params']):
        print('{} (+/-{}) for {}'.format(round(mean, 3), round(std*2, 3), params))

Rejects.head()

#Logistic Regression
#When being able to explain how variables affect model is important. 

lr = LogisticRegression()
parameters = {
    'C': [.001, .01, .1, 1, 10, 100, 1000]
}

cv = GridSearchCV(lr, parameters,  cv=5)
cv.fit(tr_features, tr_labels.values.ravel())

print_results(cv)

#Pickel and Save model to compare for later
joblib.dump(cv.best_estimator_,'LR_model.pkl')


#Support Vector Machine (SVC)
#Data is short and fat. Many variables but not a lot of rows/observations
from sklearn.svm import SVC
svc = SVC()
parameters = {
    'kernel': ['linear', 'rbf'],
    'C': [.1, 1, 10]
}

cv = GridSearchCV(svc, parameters, cv=5)
cv.fit(tr_features, tr_labels.values.ravel())

print_results(cv)
#Pickel and Save model to compare for later
joblib.dump(cv.best_estimator_,'SVM_model.pkl')

#Multilayer Perceptron (MLP)
#Complex....
from sklearn.neural_network import MLPClassifier
mlp = MLPClassifier()
parameters = {
    'hidden_layer_sizes': [(10,), (50,), (100,)],
    'activation': ['relu', 'tanh', 'logistic'],
    'learning_rate': ['constant', 'invscaling', 'adaptive']
}

cv = GridSearchCV(mlp, parameters, cv=5)
cv.fit(tr_features, tr_labels.values.ravel())

print_results(cv)

#Pickel and save model
joblib.dump(cv.best_estimator_,'MLP_model.pkl')

#Random Forest
#Few but Deep trees. Complex interactions and relationship
from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier()
parameters = {
    'n_estimators': [5, 50, 250],
    'max_depth': [2, 4, 8, 16, 32, None]
}

cv = GridSearchCV(rf, parameters, cv=5)
cv.fit(tr_features, tr_labels.values.ravel())

print_results(cv)
#Pickel model and save. 
joblib.dump(cv.best_estimator_, 'RF_model.pkl')

#Gradient Boosting
#Many, but shallow trees. Complex interactions and relationships
from sklearn.ensemble import GradientBoostingClassifier
gb = GradientBoostingClassifier()
parameters = {
    'n_estimators': [5, 50, 250, 500],
    'max_depth': [1, 3, 5, 7, 9],
    'learning_rate': [0.01, 0.1, 1, 10, 100]
}

cv = GridSearchCV(gb, parameters, cv=5)

cv.fit(tr_features, tr_labels.values.ravel())

print_results(cv)

#Pickel and save model
joblib.dump(cv.best_estimator_, 'GB_model.pkl')

#Neural Network Sequential

from numpy.random import seed
seed(99)
import tensorflow
tensorflow.random.set_seed(99)
from keras.models import Sequential
from keras.layers import Dense
Nnet = Sequential()
number_inputs = 48
number_hidden_nodes = 7
Nnet.add(Dense(units=number_inputs, activation='sigmoid', input_dim = number_inputs))
Nnet.add(Dense(units=3, activation='relu'))
Nnet.add(Dense(units=2, activation='tanh'))
Nnet.add(Dense(units=1, activation='sigmoid'))
Nnet.summary()
Rejects.shape
Nnet.compile(optimizer='adam',
            loss='binary_crossentropy',
            metrics=['accuracy'])

Nnet.fit(tr_features, tr_labels, epochs=1000, shuffle=True, verbose=2)

#Evaluate Models

from sklearn.metrics import accuracy_score, precision_score, recall_score 
from time import strptime, time

val_features = pd.read_csv('val_features.csv')
val_labels = pd.read_csv('val_labels.csv')

te_features = pd.read_csv('test_features.csv')
te_labels = pd.read_csv('test_lables.csv')

models = {}

for mdl in ['LR', 'SVM', 'MLP', 'RF', 'GB']:
    models[mdl] = joblib.load('{}_model.pkl'.format(mdl))

def evaluate_model(name, model, features, labels):
    start = time()
    pred = model.predict(features)
    end = time()
    accuracy = round(accuracy_score(labels, pred), 3)
    precision = round(precision_score(labels, pred), 3)
    recall = round(recall_score(labels, pred), 3)
    print('{} -- Accuracy: {} / Precision: {} / Recall: {} / Latency: {}ms'.format(name,
                                                                                    accuracy,
                                                                                    precision,
                                                                                    recall,
                                                                                    round((end - start) * 1000, 1)))

for name, mdl in models.items():
    evaluate_model(name, mdl, val_features, val_labels)

Nnet_test_loss, NNet_test_accuracy = Nnet.evaluate(te_features, te_labels, verbose=2)

#Evaluate model that performed best on training set, on the holdout test set
evaluate_model('Random Forest', models['RF'], te_features, te_labels)
evaluate_model('Gradient Boosting', models['GB'], te_features, te_labels)
evaluate_model('SVM', models['SVM'], te_features, te_labels)
evaluate_model('MLP', models['MLP'], te_features, te_labels)
evaluate_model('Logistic Regr', models['LR'], te_features, te_labels)
print('NNet Loss {}: NNet Accuracy {}'.format(round(Nnet_test_loss, 2), round(NNet_test_accuracy, 3)))

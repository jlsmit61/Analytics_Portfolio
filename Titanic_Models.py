from asyncore import write
from cProfile import label
import imp
from operator import index
from pickle import TRUE
from tkinter import Grid
from unittest import result
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from skimpy import skim

file = 'titanic.csv'
titanic = pd.read_csv(file)
skim(titanic)
#View Columns with missing values
titanic.isnull().sum()
 
#Correct age for missing to show as mean
titanic['Age'].fillna(titanic['Age'].mean(), inplace=True)
 
#Combine SibSp and Parch columns
for i, col in enumerate(['SibSp', 'Parch']):
    plt.figure(i)
    sns.catplot(x=col, y='Survived', data = titanic, kind = 'point', aspect = 2)
 
titanic['Family_cnt'] = titanic['SibSp'] + titanic['Parch']
 
#Drop unnecessary vairables
titanic.drop(['PassengerId', 'SibSp', 'Parch'], axis = 1, inplace = True)
 
#Fill in missing and create indicator for Cabin variable
titanic.isnull().sum()
titanic.groupby(titanic['Cabin'].isnull())['Survived'].mean()
titanic['Cabin_ind'] = np.where(titanic['Cabin'].isnull(), 0, 1)
 
#Convert Sex to numeric, notice use of dictionary where in R we would use ifelse
gender_num = {'male': 0, 'female': 1}
titanic['Sex'] = titanic['Sex'].map(gender_num)
 
#Drop unnecessary variables
titanic.drop(['Cabin', 'Embarked', 'Name', 'Ticket'], axis=1, inplace=True)
 
#Write cleaned data to csv
titanic.to_csv('titanic_cleaned_vscode.csv', index=False)
 
#Library to Split into train, validation, and test sets
from sklearn.model_selection import train_test_split
 
titanic = pd.read_csv('titanic_cleaned_vscode.csv')
features = titanic.drop('Survived', axis=1)
labels = titanic['Survived']
 
X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size=.4, random_state=42)
X_val, X_test, y_val, y_test = train_test_split(X_test, y_test, test_size=.5, random_state=42)
 
#Show partitioning of data for train, validate, test sets
for dataset in [y_train, y_val, y_test]:
    print(round(len(dataset)/ len(labels), 2))
 
X_train.to_csv('train_features.csv', index = False)
X_val.to_csv('val_features.csv', index = False)
X_test.to_csv('test_features.csv', index = False)
 
y_train.to_csv('train_labels.csv', index = False)
y_val.to_csv('val_labels.csv', index = False)
y_test.to_csv('test_lables.csv', index = False)
 
import joblib
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
#Grid Seach assist in hyperparameter tuning
import warnings
warnings.filterwarnings('ignore', category=FutureWarning)
warnings.filterwarnings('ignore', category=DeprecationWarning)
 
tr_features = pd.read_csv('train_features.csv')
tr_labels = pd.read_csv('train_labels.csv')
 
#Create function to neatly present best parameteres when testing models
def print_results(results):
    print('Best Params: {}\n'.format(results.best_params_))
 
    means = results.cv_results_['mean_test_score']
    stds = results.cv_results_['std_test_score']
    for mean, std, params in zip(means, stds, results.cv_results_['params']):
        print('{} (+/-{}) for {}'.format(round(mean, 3), round(std*2, 3), params))
 
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
 
#Evaluate Models
 
from sklearn.metrics import accuracy_score, precision_score, recall_score 
from time import time
 
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
 
#Evaluate model that performed best on training set, on the holdout test set
evaluate_model('Random Forest', models['RF'], te_features, te_labels)

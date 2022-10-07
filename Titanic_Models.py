from asyncore import write
from cProfile import label
from operator import index
from tkinter import Grid
from unittest import result
from keras.backend import flatten
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from numpy.lib import utils
import pandas as pd
import seaborn as sns
from skimpy import skim
from keras.models import Sequential
from keras.layers import Dense


file = 'titanic.csv'
titanic = pd.read_csv(file)
skim(titanic)
titanic.head()
#View Columns with missing values
titanic.isnull().sum()
 
#Correct age for missing to show as mean
titanic['Age'].fillna(titanic['Age'].mean(), inplace=True)
 
#Combine SibSp and Parch columns
titanic['Household_Total'] = titanic['SibSp'] + titanic['Parch']
 
#Drop unnecessary vairables
titanic.drop(['PassengerId', 'SibSp', 'Parch'], axis = 1, inplace = True)
 
#Fill in missing and create indicator for Cabin variable
titanic.isnull().sum()
titanic['Cabin_ind'] = np.where(titanic['Cabin'].isnull(), 0, 1)

#Wealthy Variable Creation
Fare_Avg = titanic['Fare'].mean()
titanic['Wealthy_Pclass'] = np.where((titanic['Pclass'] == 1), 1, 0)
titanic['Wealthy_Fare'] = np.where((titanic['Fare'] > Fare_Avg), 1, 0)

titanic['Wealthy'] = np.logical_and(titanic['Wealthy_Pclass'] == 1, titanic['Wealthy_Fare'] == 1)
titanic['Wealthy'] = np.where((titanic['Wealthy'] == False), 0, 1)

titanic.drop(['Wealthy_Pclass', 'Wealthy_Fare'], axis=1, inplace = True)

#Wealthy Location Variable
titanic['Wealthy_Loc_C'] = np.logical_and(titanic['Wealthy'] == 1, titanic['Embarked'] == 'C')
titanic['Wealthy_Loc_S'] = np.logical_and(titanic['Wealthy'] == 1, titanic['Embarked'] == 'S')
titanic['Wealthy_Loc_Q'] = np.logical_and(titanic['Wealthy'] == 1, titanic['Embarked'] == 'Q')

titanic['Wealthy_Loc_C'] = np.where((titanic['Wealthy_Loc_C'] == False), 0 , 1)
titanic['Wealthy_Loc_S'] = np.where((titanic['Wealthy_Loc_S'] == False), 0, 1)
titanic['Wealthy_Loc_Q'] = np.where((titanic['Wealthy_Loc_Q'] == False), 0, 1)

#Pclass and Cabin assignment
titanic['ThirdClass_Cabin'] = np.logical_and(titanic['Pclass'] == 3, titanic['Cabin_ind'] == 1)
titanic['ThirdClass_Cabin'] = np.where((titanic['ThirdClass_Cabin'] == False), 0, 1)

#Age Bins
titanic['Child'] = np.where((titanic['Age'] <=15), 1, 0)
titanic['Young_Adult'] = np.logical_and(titanic['Age'] >15, titanic['Age']<=29)
titanic['Young_Adult'] = np.where((titanic['Young_Adult'] == False), 0, 1)
titanic['Middle_Aged'] = np.logical_and(titanic['Age']>29, titanic['Age']<=54)
titanic['Middle_Aged'] = np.where((titanic['Middle_Aged'] == False), 0, 1)
titanic['Senior'] = np.where((titanic['Age']>54), 1, 0)

#Cheap Cabin: Fare less than avg and cabin indicator is present
titanic['Cheap_Cabin'] = np.logical_and(titanic['Fare'] < Fare_Avg, titanic['Cabin_ind'] == 1)
titanic['Cheap_Cabin'] = np.where((titanic['Cheap_Cabin'] == False), 0, 1)

#Cheap tickets based on Age
titanic['Age_Fare_Child'] = np.logical_and(titanic['Child'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_Child'] = np.where((titanic['Age_Fare_Child'] == False), 0, 1)
titanic['Age_Fare_YA'] = np.logical_and(titanic['Young_Adult'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_YA'] = np.where((titanic['Age_Fare_YA'] == False), 0, 1)
titanic['Age_Fare_MA'] = np.logical_and(titanic['Middle_Aged'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_MA'] = np.where((titanic['Age_Fare_MA'] == False), 0, 1)
titanic['Age_Fare_Senior'] = np.logical_and(titanic['Senior'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_Senior'] = np.where((titanic['Age_Fare_Senior'] == False), 0, 1)

#Wealthy by Sex
titanic['Wealthy_Female'] = np.logical_and(titanic['Wealthy'] == 1, titanic['Sex'] == "female")
titanic['Wealthy_Female'] = np.where((titanic['Wealthy_Female'] == False), 0, 1)
titanic['Wealthy_Male'] = np.logical_and(titanic['Wealthy'] == 1, titanic['Sex'] == "male")
titanic['Wealthy_Male'] = np.where((titanic['Wealthy_Male'] == False), 0, 1)


#Convert Sex to numeric
gender_num = {'male': 0, 'female': 1}
titanic['Sex'] = titanic['Sex'].map(gender_num)

#Drop unnecessary variables
titanic.drop(['Cabin', 'Embarked', 'Name', 'Ticket'], axis=1, inplace=True)
 
#Write cleaned data to csv
titanic.to_csv('titanic_clean.csv', index=False)

 
#Library to Split into train, validation, and test sets
from sklearn.model_selection import train_test_split

 
titanic = pd.read_csv('titanic_clean.csv')
features = titanic.drop('Survived', axis=1)
labels = titanic['Survived']
 
#80% Training/10%Validation/10% Testing
X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size=.2, random_state=99)
#X_val, X_test, y_val, y_test = train_test_split(X_test, y_test, test_size=.5, random_state=99)
 
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
#Grid Seach assist in hyperparameter tuning
import warnings
warnings.filterwarnings('ignore', category=FutureWarning)
warnings.filterwarnings('ignore', category=DeprecationWarning)
 

#Logistic Regression
lr = LogisticRegression()
parameters = {
    'C': [.001, .01, .1, 1, 10, 100, 1000]
}
 
lr_cv = GridSearchCV(lr, parameters,  cv=5)
lr_cv.fit(X_train, y_train.values.ravel())
print('Accuracy: {}; Best Parameter {}'.format(round(lr_cv.best_score_, 2), lr_cv.best_params_))
 
 
#Support Vector Machine (SVC)
from sklearn.svm import SVC
svc = SVC()
parameters = {
    'kernel': ['linear', 'rbf'],
    'C': [.1, 1, 10]
}
 
svc_cv = GridSearchCV(svc, parameters, cv=5)
svc_cv.fit(X_train, y_train.values.ravel())
print('Accuracy: {}; Best Parameter {}'.format(round(svc_cv.best_score_, 2), svc_cv.best_params_))

  
#Multilayer Perceptron (MLP)
from sklearn.neural_network import MLPClassifier
mlp = MLPClassifier()
parameters = {
    'hidden_layer_sizes': [(10,), (50,), (100,)],
    'activation': ['relu', 'tanh', 'logistic'],
    'learning_rate': ['constant', 'invscaling', 'adaptive']
}
 
mlp_cv = GridSearchCV(mlp, parameters, cv=5)
mlp_cv.fit(X_train, y_train.values.ravel())
print('Accuracy: {}; Best Parameter {}'.format(round(mlp_cv.best_score_, 2), mlp_cv.best_params_)) 

 
#Random Forest
from sklearn.ensemble import RandomForestClassifier
 
rf = RandomForestClassifier()
parameters = {
    'n_estimators': [5, 50, 250],
    'max_depth': [2, 4, 8, 16, 32, None]
}
 
rf_cv = GridSearchCV(rf, parameters, cv=5)
rf_cv.fit(X_train, y_train.values.ravel())
print('Accuracy: {}; Best Parameter {}'.format(round(rf_cv.best_score_, 2), rf_cv.best_params_)) 

  
#Gradient Boosting
from sklearn.ensemble import GradientBoostingClassifier
gb = GradientBoostingClassifier()
parameters = {
    'n_estimators': [5, 50, 250, 500],
    'max_depth': [1, 3, 5, 7, 9],
    'learning_rate': [0.01, 0.1, 1, 10, 100]
}
 
gb_cv = GridSearchCV(gb, parameters, cv=5)
 
gb_cv.fit(X_train, y_train.values.ravel())
 
print('Accuracy: {}; Best Parameter {}'.format(round(gb_cv.best_score_, 2), gb_cv.best_params_))

#Neural Network Sequential()
from numpy.random import seed
seed(99)
import tensorflow
tensorflow.random.set_seed(99)
NNet1 = Sequential()
number_inputs = 22
number_hidden_nodes = 7
NNet1.add(Dense(units=number_hidden_nodes,activation='sigmoid', input_dim=number_inputs))
NNet1.add(Dense(units=3, activation = 'relu'))
NNet1.add(Dense(units=2, activation='tanh'))
NNet1.add(Dense(units = 1, activation='sigmoid'))
NNet1.summary()

NNet1.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])

NNet1.fit(X_train, y_train, epochs=1800, verbose=2)

""" NNet1_loss, NNet1_accuracy = NNet1.evaluate(X_val, y_val, verbose=2)
print(f"Loss: {NNet1_loss}, Accuracy: {NNet1_accuracy}") """
  
#Evaluate Models

#Excluding Validation Set, not interested in measuring model parameteres within same model type. 
""" lr_pred = lr_cv.predict(X_val)
svc_pred = svc_cv.predict(X_val)
mlp_pred = mlp_cv.predict(X_val)
rf_pred = rf_cv.predict(X_val)
gb_pred = gb_cv.predict(X_val)
print('Logistic Accuracy: {}'.format(accuracy_score(y_val, lr_pred)))
print('SVM Accuracy: {}'.format(accuracy_score(y_val, svc_pred)))
print('MLP Accuracy: {}'.format(accuracy_score(y_val, mlp_pred)))
print('Random Forest Accuracy: {}'.format(accuracy_score(y_val, rf_pred)))
print('Gradient Boosting Accuracy: {}'.format(accuracy_score(y_val, gb_pred))) """

#Testing
from sklearn.metrics import accuracy_score, precision_score, recall_score 
rf_pred_test = rf_cv.predict(X_test)
print('Random Forest Accuracy: {}'.format(round(accuracy_score(y_test, rf_pred_test), 3)))
gb_pred_test = gb_cv.predict(X_test)
print('Gradient Boosting Accuracy: {}'.format(round(accuracy_score(y_test, gb_pred_test), 3)))                                                                        
mlp_pred_test = mlp_cv.predict(X_test)
print('MLP Accuracy: {}'.format(round(accuracy_score(y_test, mlp_pred_test), 3)))
svc_pred_test = svc_cv.predict(X_test)
print('SVM Accuracy: {}'.format(round(accuracy_score(y_test, svc_pred_test), 3)))
lr_pred_test = lr_cv.predict(X_test)
print('Logistic Accuracy: {}'.format(round(accuracy_score(y_test, lr_pred_test), 3)))
NNet1_test_loss, NNet1_test_accuracy = NNet1.evaluate(X_test, y_test, verbose=2)
print('Accuracy: {}'.format(round(NNet1_test_accuracy, 3), round(NNet1_test_loss, 3)))

RF_Acc = round(accuracy_score(y_test, rf_pred_test), 3)
GB_Acc = round(accuracy_score(y_test, gb_pred_test), 3)
MLP_Acc = round(accuracy_score(y_test, mlp_pred_test), 3)
SVM_Acc = round(accuracy_score(y_test, svc_pred_test), 3)
LR_Acc = round(accuracy_score(y_test, lr_pred_test), 3)
NNet_Acc = round(NNet1_test_accuracy, 3)
 
df = pd.DataFrame([RF_Acc, GB_Acc, MLP_Acc, SVM_Acc, LR_Acc, NNet_Acc], ['RandomForest', 'Gradient Boosting', 'Multilayer Perceptron', 'Support Vector Machine', 'Logistic Regression', 'Neural Network'])
Accuracy_Table_df = df.rename(columns={0: 'Test Accuracy'})
Accuracy_Table = Accuracy_Table_df.sort_values(by=['Test Accuracy'], ascending=False)
Accuracy_Table

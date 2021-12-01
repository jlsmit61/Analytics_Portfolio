#line chart evaluating two variables (Gallons used and Total Price)
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt

fuel_capture = pd.read_csv('All Sources Fuel Capture 2020')
ax = plt.gca()
dataset.plot(kind='line', x='Index', y='Gallons', xlabel='Month', ax=ax)
dataset.plot(kind='line', x='Index', y='Total Price', xlabel='Month', color='red', ax=ax)
plt.tick_params(labelsize=16)
ax.set_title('Gallons Used and Total Price')
plt.show()

#Regression analysis on same two variables (Gallons used and Total Price)
regression = sns.regplot('Gallons', 'Total Price', data=dataset)
regression.set_ylim(bottom=0)
regression.set_xlim(left=0)
plt.title('Relationship of Gallons Used vs. Total Price')
plt.set_ylim=([0,100000])
plt.show()

#Gallons per month used as a barchart
bar_chart = sns.barplot(x='Index', y='Gallons', data=dataset)
bar_chart.set_xlabel('Month')
bar_chart.set_title('Gallons per Month')
plt.show()

#Gallons used per month in high price scenario
sns.catplot(x='Index', y='Gallons', col='High_prices',kind='bar',data=dataset)
plt.show()

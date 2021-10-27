#Determine pay based on position and club performance

#load rankings
dfRank = pd.read_csv('mls_rankings17.csv')
dfRank_copy10 = dfRank.copy()
#Create top10 column id-ing true/false
dfRank_copy10['top10'] = dfRank_copy10['ranking'] <= 10
#dfRank_copy10.set_index(['top10'])
#Combine Rank file with Salary file
combined = pd.merge(dfRank_copy10, dfMLS_copy9, left_on=['id', 'club'], right_on=['id', 'club'])
#Create pivot table with index as top10, base salary as the fill, columns as position, and agg'd by mean
combined.pivot_table(['base_salary'], index=['top10'], columns=['position'], aggfunc='mean').style.format('{:.2f}')
#Higher performing clubs tend to pay more to their position A and M players, the other position are essentially the same. 


#Do states with largest number of parks see the same amt of visitors
#Load parks csv 
parks = pd.read_csv('parks.csv')
parks_df = pd.DataFrame(parks)
#Create filter using isin() for desired three states
parks_df['parks_state_filter'] = parks_df['location'].isin(['California', 'Alaska', 'Utah'])
#Set state filter to true on dataframe to see data on only the three states we want
CA_AK_UT = parks_df[parks_df['parks_state_filter'] == True]
#Create boxplot using new filtered dataframe of just the three desired states
sns.boxplot(x=CA_AK_UT['location'], y=CA_AK_UT['visitors_2019'], data=CA_AK_UT)
#These parks do not see the same number of visitors despite having the largest number of parks.


#Show chart of top 10 building heights in NYC grouped by street.
#Load data from csv
NYC_df = pd.read_csv('nyc_buildings.csv')
NYC_df_copy2 = NYC_df.copy()
#group streets by summed height, sorted and retained only the top 10
NYC_street_height = NYC_df_copy2['height'].groupby(NYC_df_copy2['street']).sum().reset_index().sort_values(by='height', ascending=False)[:10]
#create a matplotlib figure subplot
f, ax = plt.subplots(figsize=(14,4))
#create bar plot for top 10 heights by street
NYC_street_height.plot(ax=ax, kind='bar',xlim=[0,10])
#create top 10 variable to use in ticks label
top10 = NYC_street_height[:10]
#set ticks to pair with street names
ax.set_xticks(range(10))
#set labels to top 10 variable, rotaiton, fontsize, etc
ax.set_xticklabels(top10['street'], rotation=30, fontsize='small')
plt.show()
#The top 5 are significantly taller by street than the back half of the top 10. 

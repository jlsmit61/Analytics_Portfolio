#Status Breakdown Chart

import matplotlib.pyplot as plt
dataset.plot(kind='barh', x='Status', y='Number of Calls', color='blue', figsize=(30,12))
plt.rcParams.update({'font.size': 36})
plt.tick_params(labelsize=36)
plt.legend(['Calls per Status'])
plt.show()

#GOST-CI Overview Completed vs. Incomplete work
import matplotlib.pyplot as plt
import seaborn as sns
sns.catplot(x='Process', y='Open Tickets', col='Complete?', kind='bar', data=dataset)
plt.show()

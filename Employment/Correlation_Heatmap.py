import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
#generate paths to csv files

value_added_path = "agriculture-value-added-per-worker-wdi.csv"
working_hours_path = "annual-working-hours-per-worker.csv"
food_expenditure_path = "food-expenditure-vs-gdp.csv"
gdp_path = "national-gdp-wb.csv"
gdp_ppc_path = "gdp-per-capita-worldbank.csv"

# Load all datasets
value_added_df = pd.read_csv(value_added_path)
working_hours_df = pd.read_csv(working_hours_path)
food_expenditure_df = pd.read_csv(food_expenditure_path)
gdp_df = pd.read_csv(gdp_path)
gdp_ppc_df = pd.read_csv(gdp_ppc_path)

# Merge all datasets on 'Entity', 'Code', and 'Year'
from functools import reduce
data_frames = [value_added_df, working_hours_df, food_expenditure_df, gdp_df, gdp_ppc_df]
merged_df = reduce(lambda left, right: pd.merge(left, right, on=['Entity', 'Code', 'Year'], how='outer'), data_frames)

print(merged_df.head())

# Compute the correlation matrix
corr = merged_df.corr()

# Generate a mask for the upper triangle
mask = np.triu(np.ones_like(corr, dtype=bool))

# Set up the matplotlib figure
plt.figure(figsize=(11, 9))

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap='coolwarm', vmax=.3, center=0, square=True, linewidths=.5, cbar_kws={"shrink": .5}, annot=True, annot_kws={"size": 8}) 

plt.show()
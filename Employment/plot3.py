import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

#generate paths to csv files

value_added_path = "agriculture-value-added-per-worker-wdi.csv"
working_hours_path = "annual-working-hours-per-worker.csv"
food_expenditure_path = "food-expenditure-vs-gdp.csv"
gdp_path = "national-gdp-wb.csv"
gdp_ppc_path = "gdp-per-capita-worldbank.csv"


# Load the datasets
food_expenditure_df = pd.read_csv(food_expenditure_path)
agriculture_value_df = pd.read_csv(value_added_path)

# Merging the datasets on 'Entity', 'Code', and 'Year'
merged_df = pd.merge(food_expenditure_df, agriculture_value_df, on=['Entity', 'Code', 'Year'], how='inner')

#clustering
# Define clusters
def assign_cluster(name):
    clusters = {
        'A-E': ['A', 'B', 'C', 'D', 'E'],
        'F-J': ['F', 'G', 'H', 'I', 'J'],
        'K-O': ['K', 'L', 'M', 'N', 'O'],
        'P-T': ['P', 'Q', 'R', 'S', 'T'],
        'U-Z': ['U', 'V', 'W', 'X', 'Y', 'Z']
    }
    for key, letters in clusters.items():
        if name[0] in letters:
            return key
    return 'Other'

merged_df['Cluster'] = merged_df['Entity'].apply(assign_cluster)

merged_df['Cluster'] = merged_df['Entity'].apply(assign_cluster)

# Apply the clustering function
merged_df['Cluster'] = merged_df['Entity'].apply(assign_cluster)

# Set the aesthetic style of the plots
sns.set(style="whitegrid")

# Create the scatter plot
plt.figure(figsize=(12, 8))
sns.scatterplot(data=merged_df, x='Total food expenditure', y='Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)', hue='Cluster', palette='viridis', alpha=0.6)

# Add plot title and labels
plt.title('Comparison of Total Food Expenditure and Agriculture Value Added per Worker')
plt.xlabel('Total Food Expenditure (USD)')
plt.ylabel('Agriculture Value Added per Worker (constant 2015 USD)')

# Show the plot
plt.show()

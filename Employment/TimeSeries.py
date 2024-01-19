import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

#generate paths to csv files

value_added_path = "agriculture-value-added-per-worker-wdi.csv"
working_hours_path = "annual-working-hours-per-worker.csv"
food_expenditure_path = "food-expenditure-vs-gdp.csv"
gdp_path = "national-gdp-wb.csv"
gdp_ppc_path = "gdp-per-capita-worldbank.csv"



# Load datasets
gdp_df = pd.read_csv('national-gdp-wb.csv')
value_added_df = pd.read_csv('agriculture-value-added-per-worker-wdi.csv')

# Merge datasets on 'Entity', 'Code', and 'Year'
merged_df = pd.merge(gdp_df, value_added_df, on=['Entity', 'Code', 'Year'], how='inner')

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


# Set the aesthetic style of the plots
sns.set_style("whitegrid")

unique_clusters = merged_df['Cluster'].unique()

for cluster in unique_clusters:
    cluster_data = merged_df[merged_df['Cluster'] == cluster]
    
    # Initialize the matplotlib figure
    plt.figure(figsize=(14, 8))
    
    # Create the first subplot for GDP
    ax1 = sns.lineplot(data=cluster_data, x='Year', y='GDP, PPP (constant 2017 international $)', hue='Entity', legend='brief', palette='coolwarm')
    ax1.set_ylabel('GDP (constant 2017 international $)', color='b')
    ax1.tick_params('y', colors='b')
    plt.title(f'GDP and Agriculture Value Added per Worker Over Time ({cluster})')
    plt.xlabel('Year')
    
    # Create the second subplot for Agriculture Value Added per Worker with a secondary y-axis
    ax2 = ax1.twinx()
    sns.lineplot(data=cluster_data, x='Year', y='Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)', hue='Entity', legend=False, palette='viridis', ax=ax2)
    ax2.set_ylabel('Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)', color='g')
    ax2.tick_params('y', colors='g')
    
    # Positioning the legend
    ax1.legend(title='Country', bbox_to_anchor=(1.05, 1), loc='upper left', fontsize='small')
    
    plt.tight_layout()
    plt.show()
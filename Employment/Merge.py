import pandas as pd

# Load Employment Data
employment_data_path = '/mnt/data/Employment_Indicators_Agriculture_E_All_Data_NOFLAG.csv'
employment_data = pd.read_csv(employment_data_path, encoding='ISO-8859-1')

# Filter for total employment in agriculture and relevant columns
employment_data_total = employment_data[employment_data['Sex'] == 'Total']
employment_data_total = employment_data_total.filter(regex='Area|Indicator|Y20')

# Reshape to long format
employment_data_long = employment_data_total.melt(id_vars=['Area Code', 'Area', 'Indicator'], 
                                                  var_name='Year', value_name='Employment')
# Convert 'Year' to integer
employment_data_long['Year'] = employment_data_long['Year'].str.strip('Y').astype(int)

# Load GDP Data
gdp_data_path = '/mnt/data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6298258.csv'
gdp_data = pd.read_csv(gdp_data_path, skiprows=4)

# Reshape to long format
gdp_data_long = gdp_data.melt(id_vars=['Country Code', 'Country Name'], var_name='Year', value_name='GDP')
gdp_data_long['Year'] = gdp_data_long['Year'].astype(int)

# Merge the datasets on 'Area Code' from employment data and 'Country Code' from GDP data, and 'Year'
merged_data = pd.merge(employment_data_long, gdp_data_long,  left_on=['Area Code', 'Year'], 
                       right_on=['Country Code', 'Year'], how='inner')

# Display the first few rows of the merged dataset
print(merged_data.head())

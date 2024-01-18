import pandas as pd

# Load datasets
df_agri = pd.read_csv('path_to_agriculture_data.csv')
df_prices = pd.read_csv('path_to_price_data.csv')
df_gdp = pd.read_csv('path_to__gdp_data.csv')  # If you chose GDP data
df_cpi = pd.read_csv('path_to__cpi_data.csv')  # If you chose CPI data

# Basic Cleaning
def clean_data(df):
    df = df.drop_duplicates()
    df = df.dropna()  
    return df

df_agri = clean_data(df_agri)
df_prices = clean_data(df_prices)
df_gdp = clean_data(df_gdp)  #GDP data
df_cpi = clean_data(df_cpi)  #CPI data


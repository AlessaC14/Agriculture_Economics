# Effects of Credit for Agriculture

library(tidyverse)
library(ggplot2)

# read csv files
total_credit <- read.csv("total_credit.csv")
credit_to_agriculture <- read.csv("credit_to_agriculture.csv")
GDP_share <- read.csv("API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_6299253.csv")
GDP <- read.csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6298258.csv")

# focus on 2021
GDP <- data.frame("Area" = GDP$"Country.Name", "GDP" = GDP$"X2021")
GDP_share <- data.frame("Area" = GDP_share$"Country.Name", "Share" = GDP_share$"X2021")

# remove NaN
GDP <- na.omit(GDP)
GDP_share <- na.omit(GDP_share)
total_credit <- na.omit(total_credit)
credit_to_agriculture <- na.omit(credit_to_agriculture)

# fix units to USD
total_credit$"Value" <- total_credit$"Value" * 1e6
credit_to_agriculture$"Value" <- credit_to_agriculture$"Value" * 1e6

# sort alphabetically
GDP <- GDP[order(GDP$"Area"), ]
GDP_share <- GDP_share[order(GDP$"Area"), ]
total_credit <- total_credit[order(total_credit$"Area"), ]
credit_to_agriculture <- credit_to_agriculture[order(credit_to_agriculture$"Area"), ]

# keep only the countries that are shared between all three datasets
include <- credit_to_agriculture$"Area" %in% GDP_share$"Area"
credit_to_agriculture <- credit_to_agriculture[include, ]
credit_to_agriculture <- data.frame("Area" = credit_to_agriculture$"Area", "Agr_Credit" = credit_to_agriculture$"Value")

include <- GDP_share$"Area" %in% credit_to_agriculture$"Area"
GDP_share <- GDP_share[include, ]
GDP_share <- data.frame("Area" = GDP_share$"Area", "Share" = GDP_share$"Share")

include <- total_credit$"Area" %in% credit_to_agriculture$"Area"
total_credit <- total_credit[include, ]
total_credit <- data.frame("Area" = total_credit$"Area", "Credit" = total_credit$"Value")

include <- GDP$"Area" %in% credit_to_agriculture$"Area"
GDP <- GDP[include, ]
GDP <- data.frame("Area" = GDP$"Area", "GDP" = GDP$"GDP")

# create data frame for analysis
percent_agr_credit <- credit_to_agriculture$"Agr_Credit" / total_credit$"Credit" * 100
df <- data.frame(credit_to_agriculture, "Credit" = total_credit$"Credit", "Perc_Agr_Cred" = percent_agr_credit, "GDP" = GDP$"GDP", "Share" = GDP_share$"Share")

# plot Agricultural Share of GDP versus Percent Credit to Agriculture
regression <- lm(df$"Share" ~ df$"Perc_Agr_Cred")
coefficients <- coef(regression)
xsample <- c(0, 30)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("ASGDPvPCA.png", width = 400, height = 300, res = 72)
ggplot(data = df) + 
  geom_point(mapping = aes(x=Perc_Agr_Cred, y=Share)) + 
  ylim(0, 100) + 
  labs(title="Agricultural Share of GDP versus Percent Credit to Agriculture", x="Percent Credit to Agriculture", y="Agricultural Share of GDP") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# remove the three outliers (ie., Tajikistan, Zimbabwe, and Sudan)
no_outliers_df <- df[order(df$"Perc_Agr_Cred", decreasing=TRUE), ]
no_outliers_df <- no_outliers_df[no_outliers_df$Area != "Tajikistan" & no_outliers_df$Area != "Zimbabwe" & no_outliers_df$"Area" != "Sudan", ]

regression <- lm(no_outliers_df$"Share" ~ no_outliers_df$"Perc_Agr_Cred")
coefficients <- coef(regression)
xsample <- c(0, 20)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("ASGDPvPCA_no_outliers.png", width = 400, height = 300, res = 72)
ggplot(data = no_outliers_df) + 
  geom_point(mapping = aes(x=Perc_Agr_Cred, y=Share)) + 
  ylim(0, 100) + 
  labs(title="Agricultural Share of GDP versus Percent Credit to Agriculture", x="Percent Credit to Agriculture", y="Agricultural Share of GDP") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# plot Agricultural Share of GDP versus GDP
regression <- lm(df$"Share" ~ df$"GDP")
coefficients <- coef(regression)
xsample <- c(0, 2.5e13)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("ASGDPvGDP.png", width = 400, height = 300, res = 72)
ggplot(data = df) + 
  geom_point(mapping = aes(x=GDP, y=Share)) + 
  xlim(0, 2.5e13) + 
  ylim(0, 100) + 
  labs(title="Agricultural Share of GDP verus GDP", x="GDP (USD)", y="Agricultural Share of GDP") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# remove outliers (ie., United States of America)
no_outliers_df <- df[order(df$"GDP", decreasing=TRUE), ]
no_outliers_df <- no_outliers_df[no_outliers_df$Area != "United States of America", ]

regression <- lm(no_outliers_df$"Share" ~ no_outliers_df$"GDP")
coefficients <- coef(regression)
xsample <- c(0, 6e12)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("ASGDPvGDP_no_outliers.png", width = 400, height = 300, res = 72)
ggplot(data = no_outliers_df) + 
  geom_point(mapping = aes(x=GDP, y=Share)) + 
  labs(title="Agricultural Share of GDP versus GPD", x="GDP", y="Agricultural Share of GDP") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# remove GDP > 1e12 USD
no_outliers_df <- df[order(df$"GDP", decreasing=TRUE), ]
no_outliers_df <- no_outliers_df[no_outliers_df$GDP < 1e12, ]

regression <- lm(no_outliers_df$"Share" ~ no_outliers_df$"GDP")
coefficients <- coef(regression)
xsample <- c(0, 1e12)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("ASGDPvGDP_lt1e12.png", width = 400, height = 300, res = 72)
ggplot(data = no_outliers_df) + 
  geom_point(mapping = aes(x=GDP, y=Share)) + 
  xlim(0, 1e12) + 
  ylim(0, 100) + 
  labs(title="Agricultural Share of GDP versus GPD", x="GDP", y="Agricultural Share of GDP") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

#plot Percent Credit to Agriculture versus Total Credit
regression <- lm(df$"Perc_Agr_Cred" ~ df$"Credit")
coefficients <- coef(regression)
xsample <- c(0, 1.2e13)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("PCAvTC.png", width = 400, height = 300, res = 72)
ggplot(data = df) + 
  geom_point(mapping = aes(x=Credit, y=Perc_Agr_Cred)) + 
  labs(title="Percent Credit to Agriculture versus Total Credit", x="Total Credit (USD)", y="Percent Credit to Agriculture (USD)") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# remove outlier (ie., United States of America)
no_outliers_df <- df[order(df$"Credit", decreasing=TRUE), ]
no_outliers_df <- no_outliers_df[no_outliers_df$"Area" != "United States of America", ]

regression <- lm(no_outliers_df$"Perc_Agr_Cred" ~ no_outliers_df$"Credit")
coefficients <- coef(regression)
xsample <- c(0, 2e12)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("PCAvTC_no_outliers.png", width = 400, height = 300, res = 72)
ggplot(data = no_outliers_df) + 
  geom_point(mapping = aes(x=Credit, y=Perc_Agr_Cred)) + 
  labs(title="Percent Credit to Agriculture versus Total Credit", x="Total Credit (USD)", y="Percent Credit to Agriculture (USD)") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# remove "rich" countries
no_outliers_df <- df[order(df$"Credit", decreasing=TRUE), ]
no_outliers_df <- no_outliers_df[no_outliers_df$"Credit" < 2e10, ]

regression <- lm(no_outliers_df$"Perc_Agr_Cred" ~ no_outliers_df$"Credit")
coefficients <- coef(regression)
xsample <- c(0, 2e10)
ysample <- coefficients[1]+coefficients[2]*xsample
sample_df <- data.frame(xsample, ysample)

png("PCAvTC_lt2e10.png", width = 400, height = 300, res = 72)
ggplot(data = no_outliers_df) + 
  geom_point(mapping = aes(x=Credit, y=Perc_Agr_Cred)) + 
  labs(title="Percent Credit to Agriculture versus Total Credit", x="Total Credit (USD)", y="Percent Credit to Agriculture (USD)") + 
  geom_line(mapping = aes(x=xsample, y=ysample), data=sample_df, color="red") + 
  theme_classic()
dev.off()

# Analyze United States over Time

# read csv files
US_total_credit <- read.csv("US_total_credit.csv")
US_credit_to_agriculture <- read.csv("US_credit_to_agriculture.csv")
US_GDP_share <- read.csv("API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_6299253.csv")
US_GDP <- read.csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6298258.csv")

# clean US GDP data
US_GDP_share <- US_GDP_share[US_GDP_share$"Country.Name" == "United States of America", ]
US_GDP_share <- US_GDP_share[ , grepl("^X", names(US_GDP_share))]
US_GDP_share <- US_GDP_share[ , 38:62]
US_GDP_share <- data.frame("Year" = c(1997:2021), "Share" = t(US_GDP_share[1, ]))
US_GDP_share <- data.frame("Year" = c(1997:2021), "Share" = US_GDP_share$"X252")

US_GDP <- US_GDP[US_GDP$"Country.Name" == "United States of America", ]
US_GDP <- US_GDP[ , grepl("^X", names(US_GDP))]
US_GDP <- US_GDP[ , 38:62]
US_GDP <- data.frame("Year" = c(1997:2021), "GDP" = t(US_GDP[1, ]))
US_GDP <- data.frame("Year" = c(1997:2021), "GDP" = US_GDP$"X252")

# clean US credit data
US_total_credit <- US_total_credit[7:31, ]
US_credit_to_agriculture <- US_credit_to_agriculture[7:31, ]

US_total_credit$"Value" <- US_total_credit$"Value" * 1e6
US_credit_to_agriculture$"Value" <- US_credit_to_agriculture$"Value" * 1e6

US_total_credit <- data.frame("Year" = US_total_credit$"Year", "Credit" = US_total_credit$"Value")
US_credit_to_agriculture <- data.frame("Year" = US_credit_to_agriculture$"Year", "Agr_Credit" = US_credit_to_agriculture$"Value")
US_percent_agr_credit <- US_credit_to_agriculture$"Agr_Credit" / US_total_credit$"Credit" * 100

# create data frame for analysis
US_df <- data.frame(US_credit_to_agriculture, "Credit" = US_total_credit$"Credit", "Perc_Agr_Cred" = US_percent_agr_credit, "GDP" = US_GDP$"GDP", "Share" = US_GDP_share$"Share")

write.csv(US_df, "US_df.csv")

# plot Total Credit, Credit to Agriculture, and GDP versus Time in the United States
long_df <- US_df[ , c(1, 2, 3, 5)]
long_df <- long_df %>% pivot_longer(cols = c(Agr_Credit, Credit, GDP), names_to = "Category", values_to = "Value")

png("US_GvT.png", width = 400, height = 300, res = 72)
ggplot(long_df, aes(x = Year, y = Value, color = Category)) +
  geom_point() +
  theme_classic() +
  labs(title = "Total Credit, Credit to Agriculture, and GDP", x = "Year", y = "USD", color = "Legend") + 
  scale_color_manual(values = c("Agr_Credit" = "red", "Credit" = "blue", "GDP" = "black"),
                     labels = c("Credit to Agriculture", "Total Credit", "GDP"))
dev.off()

# plot Percent Credit to Agriculture and Agricultural Share of GDP versus Time
long_df <- US_df[ , c(1, 4, 6)]
long_df <- long_df %>% pivot_longer(cols = c(Perc_Agr_Cred, Share), names_to = "Category", values_to = "Value")

png("US_AvT.png", width = 400, height = 300, res = 72)
ggplot(long_df, aes(x = Year, y = Value, color = Category)) +
  geom_point() +
  theme_classic() +
  labs(title = "Percent Credit to Agriculture and Agricultural Share of GDP", x = "Year", y = "Percent", color = "Legend") + 
  scale_color_manual(values = c("Perc_Agr_Cred" = "red", "Share" = "black"),
                     labels = c("Percent Credit to Agriculture", "Agricultural Share of GDP"))
dev.off()

regression <- lm(US_df$Perc_Agr_Cred ~ US_df$Share)

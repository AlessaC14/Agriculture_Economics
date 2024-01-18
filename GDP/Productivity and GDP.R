setwd("C:/Users/a/Documents/R")
 #GDP <- read.csv('https://raw.githubusercontent.com/AlessaC14/Agriculture_Economics/main/GDP/cereal-yield-vs-gdp-per-capita.csv')
GDP <- read.csv('original.csv')
class(GDP)

install.packages('ggplot2')
install.packages('gridExtra')
install.packages("viridis")
install.packages("tidyverse")

library(ggplot2)
library(gridExtra)
library(viridis)
library(tidyverse)

GDP1 <- as_tibble(GDP)
class(GDP1)
class(GDP)

### 2020 DATA ###

first <- GDP1 %>% filter(Year=="2020") %>% 
  select(Entity, Cereal.yield..kg.per.hectare.:GDP.per.capita..PPP..constant.2017.international...)
categorized <- first %>% 
  filter(Entity=="High-income countries"| Entity=="Low-income countries"| Entity=="Lower-middle-income countries"|
         Entity=="Middle-income countries"| Entity=="Upper-middle-income countries")


plt <- ggplot(data=categorized) +
  geom_point(mapping=aes(x=Cereal.yield..kg.per.hectare.,
                         y=GDP.per.capita..PPP..constant.2017.international..., 
                         color=Entity)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title="2020 GDP and cereal yield", x="Cereal yield (kg/hetare)", y="GDP", color="category") +
  theme_classic()
plt

all_countries <- first %>%
  select(Cereal.yield..kg.per.hectare.:GDP.per.capita..PPP..constant.2017.international...)
plt_all <- plot(all_countries[,c(1,2)], pch = 16, cex = 0.75,
                ylab = "GDP", xlab = "Cereal yield (kg/hetare)", 
                main = "2020 GDP and cereal yield")


### 2019 DATA ###

second <- GDP1 %>% filter(Year=="2019") %>% 
  select(Entity, Cereal.yield..kg.per.hectare.:GDP.per.capita..PPP..constant.2017.international...)
categorized2 <- second %>% 
  filter(Entity=="High-income countries"| Entity=="Low-income countries"| Entity=="Lower-middle-income countries"|
           Entity=="Middle-income countries"| Entity=="Upper-middle-income countries")

plt2 <- ggplot(data=categorized2) +
  geom_point(mapping=aes(y=GDP.per.capita..PPP..constant.2017.international..., 
                         x=Cereal.yield..kg.per.hectare., 
                         color=Entity)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title="2019 GDP and cereal yield", y="GDP", x="Cereal yield (kg/hetare)", color="category") +
  theme_classic()
plt2

all_countries2 <- second %>%
  select(Cereal.yield..kg.per.hectare.:GDP.per.capita..PPP..constant.2017.international...)
plt_all2 <- plot(all_countries2[,c(1,2)], pch = 16, cex = 0.75,
                ylab = "GDP", xlab = "Cereal yield (kg/hetare)", 
                main = "2019 GDP and cereal yield")


### 2018 DATA ###

third <- GDP1 %>% filter(Year=="2018") %>% 
  select(Entity, Cereal.yield..kg.per.hectare.:GDP.per.capita..PPP..constant.2017.international...)
categorized3 <- third %>% 
  filter(Entity=="High-income countries"| Entity=="Low-income countries"| Entity=="Lower-middle-income countries"|
           Entity=="Middle-income countries"| Entity=="Upper-middle-income countries")

plt3 <- ggplot(data=categorized3) +
  geom_point(mapping=aes(y=GDP.per.capita..PPP..constant.2017.international..., 
                         x=Cereal.yield..kg.per.hectare., 
                         color=Entity)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title="2018 GDP and cereal yield", y="GDP", x="Cereal yield (kg/hetare)", color="category") +
  theme_classic()
plt3

all_countries3 <- third %>%
  select(Cereal.yield..kg.per.hectare.:GDP.per.capita..PPP..constant.2017.international...)
plt_all3 <- plot(all_countries3[,c(1,2)], pch = 16, cex = 0.75,
                 ylab = "GDP", xlab = "Cereal yield (kg/hetare)", 
                 main = "2018 GDP and cereal yield")

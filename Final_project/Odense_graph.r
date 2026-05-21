#Install and load the tidyverse package

install.packages("tidyverse")
library(tidyverse)

#Import the CSV files containing housing prices and income data

bolig <- read_csv2("data/Propertyprice.csv")
indkomst <- read_csv2("data/Odense_inkomst.csv")

#Clean and prepare housing price data for Odense

bolig_clean <- bolig %>%
  filter(Municipality == "Odense") %>%
  
  mutate(
    Year = as.numeric(str_sub(Time, 1, 4))
  ) %>%
  group_by(Year) %>%
  summarise(
    boligpris = mean(Parcel_og_raekkehus_dkkprkm2, na.rm = TRUE),
    ejerlejlighed = mean(Ejerlejlighed_dkkprkm2, na.rm = TRUE)
  )
#Ensure the Year column is numeric
indkomst_clean <- indkomst %>%
  mutate(Year = as.numeric(Year))

#Merge the two datasets and create index values
data <- left_join(bolig_clean, indkomst_clean, by = "Year") %>%
  filter(!is.na(Income)) %>%
  arrange(Year) %>%
  mutate(
    hus_index = boligpris / first(boligpris) * 100,
    
    lejlighed_index = ejerlejlighed / first(ejerlejlighed) * 100,
    
    income_index = Income / first(Income) * 100
  )

#Create the graph
ggplot(data, aes(x = Year)) +
  
  geom_line(
    aes(y = hus_index, color = "House prices"),
    linewidth = 1.2
  ) +
  
  geom_line(
    aes(y = lejlighed_index, color = "Apartment prices"),
    linewidth = 1.2
  ) +
  
  geom_line(
    aes(y = income_index, color = "Income"),
    linewidth = 1.2
  ) +
  
  labs(
    title = "Housing prices and income in Odense",
    x = "Year",
    y = "Index (1992 = 100)",
    color = ""
  ) +
  
  theme_minimal()


#Install and load the tidyverse package
install.packages("tidyverse")
library(tidyverse)

#Read the CSV file into R and display column names and preview the dataset
data <- read.csv2("Data/Propertyprice.csv")
names(data)
head(data)

#Clean and reshape the data and extract the year from the time variable
data_clean <- data %>%
  mutate(
    Year = as.numeric(substr(Time, 1, 4))
  ) %>%
#Convert housing price columns into a long format and rename the housing categories
  pivot_longer(
    cols = c(Parcel_og_raekkehus_dkkprkm2, Ejerlejlighed_dkkprkm2),
    names_to = "Boligtype",
    values_to = "Pris"
  ) %>%
  mutate(
    Boligtype = recode(
      Boligtype,
      "Parcel_og_raekkehus_dkkprkm2" = "Huse",
      "Ejerlejlighed_dkkprkm2" = "Lejligheder"
    )
  )
#Create and customize the plot
ggplot(data_clean, aes(x = Year, y = Pris, color = Municipality)) +
  geom_smooth(se = FALSE, linewidth = 0.6) +
  facet_wrap(~ Boligtype, ncol = 1) +
  scale_x_continuous(breaks = seq(1992, 2024, by = 4)) +
  scale_y_continuous(breaks = seq(0, 70000, by = 5000)) +
  labs(
    title = "Udvikling i boligpriser fordelt på kommuner",
    x = "År",
    y = "Pris pr. m²",
    color = "Kommune"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
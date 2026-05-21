#Read the data into R

install.packages("tidyverse")
library(tidyverse)
read.csv2("DAMFinalProjectIncomeDataset2026.csv")

# Save the data into a variable
income_data <- read.csv2("DAMFinalProjectIncomeDataset2026.csv")
income_data <- income_data[, 1:3]
income_data <- na.omit(income_data)
income_data <- income_data[nchar(trimws(income_data$Municipality)) > 0, ]


#Make it into a graph
install.packages("ggplot2")
library(ggplot2)

#Costumize the graph
ggplot(income_data, aes(x = Year, y = Average_income_DKK, color = Municipality)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(limits = c(1992, 2024), breaks = seq(1992, 2024, by = 4)) +
  coord_cartesian(ylim = c(80000, 500000)) +
  labs(title = "Average Income in Danish Municipalities",
       x = "Year",
       y = "Average Income (DKK)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 25,face = "bold"),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25, face = "bold"),
    legend.position = "bottom"
  )

#Save the graph as a PNG
ggsave("income_plot_graph.png", width = 20, height = 22)
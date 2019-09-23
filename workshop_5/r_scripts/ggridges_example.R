library(tidyverse)
library(ggridges)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = cty, y = fct_reorder(manufacturer, cty), group = manufacturer)) +
  geom_density_ridges(scale = 5, size = 0.5, rel_min_height = 0.01) +
  labs(title = "Ridge Plot of City Fuel Consumption for Different Car Manufacturers", 
       x = "City fuel consumption (mpg)",
       y = NULL) +
  theme_ridges() 
  

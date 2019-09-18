devtools::install_github('bbc/bbplot')

library(tidyverse)
library(bbplot)

my_plot <- mpg %>%
  count(class) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(x = class, y = n, fill = class)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) 

my_plot <- mpg %>%
  count(class) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(x = class, y = n, fill = class)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE)  +
  bbc_style() +
  labs(title="Number of each type of car",
       subtitle = "Data collected in the US, 1999-2008")

finalise_plot(plot_name = my_plot,
              source = "ONS",
              save_filepath = "my_plot.png",
              width_pixels = 640,
              height_pixels = 550)

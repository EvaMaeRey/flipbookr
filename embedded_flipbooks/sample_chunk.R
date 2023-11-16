library(tidyverse)
Titanic %>% 
  data.frame() %>% 
  uncount(Freq) %>% 
  ggplot() + 
  aes(x = Sex) + 
  geom_bar(position = "fill") + 
  aes(fill = Survived)

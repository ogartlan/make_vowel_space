library(tidyverse)
library(zoo)


# set the path where your formant table lives
setwd("C:\\4th_Year\\Final_Year_Project\\my_voice")

df <- read.csv("my_formants.Table", stringsAsFactors = FALSE) 

set.seed(10)

vowels = tibble(vowel = rep(c("a", "e", "i", "o", "u"), each = 50),
                
                F1 = c(rnorm(50, mean = 800, sd = 100), 
                       rnorm(50, mean = 600, sd = 100), 
                       rnorm(50, mean = 350, sd = 100), 
                       rnorm(50, mean = 600, sd = 100), 
                       rnorm(50, mean = 350, sd = 100)),
                
                F2 = c(rnorm(50, mean = 1500, sd = 150), 
                       rnorm(50, mean = 2000, sd = 150), 
                       rnorm(50, mean = 2500, sd = 150), 
                       rnorm(50, mean = 1000, sd = 150), 
                       rnorm(50, mean = 800, sd = 150)))


ggplot(data = vowels, aes(x = F2, y = F1, color = vowel, label = vowel)) + 
  geom_text() +
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") +
  geom_density_2d() +
  theme(legend.position = "none") +
  theme_classic()
  
  

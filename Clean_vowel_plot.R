library(dplyr)
library(ggplot2)
library(readr)
library(zoo)


# set the path where your formant table lives
setwd("C:\\4th_Year\\Final_Year_Project\\my_voice")

df <- read.csv("my_formants_cleaned.Table", stringsAsFactors = FALSE) 


#--------------------------------------------------------------------#
#set.seed(10)
#
#vowels = tibble(vowel = rep(c("a", "e", "i", "o", "u"), each = 50),
#                
#                F1 = c(rnorm(50, mean = 800, sd = 100), 
#                      rnorm(50, mean = 600, sd = 100), 
#                      rnorm(50, mean = 350, sd = 100), 
#                      rnorm(50, mean = 600, sd = 100), 
#                      rnorm(50, mean = 350, sd = 100)),
#           
#                F2 = c(rnorm(50, mean = 1500, sd = 150), 
#                       rnorm(50, mean = 2000, sd = 150), 
#                       rnorm(50, mean = 2500, sd = 150), 
#                       rnorm(50, mean = 1000, sd = 150), 
#                       rnorm(50, mean = 800, sd = 150)))
#--------------------------------------------------------------------#

# the data frame should have this format:
#     vowel time_index v_time time_abs   F1   F2   F3
# 1      ii          1  0.000    0.359  288 2527 3498
# 2      ii          2  0.026    0.385  300 2547 3439
# 3      ii          3  0.053    0.411  263 2383 3367
# 4      ii          4  0.079    0.437  269 2387 3375
# 5      ih          1  0.000    1.200  345 2343 3338
# 6      ih          2  0.019    1.219 1358 2220 3269
# 7      ih          3  0.037    1.237  633 2160 3122
# 8      ih          4  0.056    1.256  403 2124 3038
# 9      ei          1  0.000    1.975  497 2390 3439
# 10     ei          2  0.024    1.999  397 2350 3272
# 11     ei          3  0.049    2.024  387 2341 3274
# 12     ei          4  0.073    2.048  367 2352 3257
#
# ... with each vowel having ten timepoints instead of four. 

#ggplot(df, aes(x = column1, y = column2)) +
#  geom_point()



#================================================================#
# https://en.wikipedia.org/wiki/Phonetic_symbols_in_Unicode#Vowels
vowel_lookup = 
    c(`ae` = "\u00E6",         # cat
      `ah` = "\u0251",         # cot
      `aw` = "\u0254",         # caught
      `ai` = "\u0251\u026A",   # ride
      `ait` = "\u0251\u026At", # right
      `au` = "a\u028A",        # cloud
      `eh` = "\u025B",         # bet
      `ei` = "e\u026A",        # rate
      `ih` = "\u026A",         # bit
      `ii` = "i",              # beat
      `oh` = "o\u028A",        # boat
      `oo` = "\u028A",         # cook
      `uh` = "\u028C",         # cut
      `uu` = "u",              # tooth
      `xx` = "\u0259",         # a(head)
      `er` = "\u025D",         # bird
      `eir` = "e\u026Ar",      # mary
      `ehr` = "\u025Br",       # merry
      `aer` = "\u00E6r",       # marry
      `cr` = "\u0254r",        # more
      `ar` = "\u0251r",        # far
      `oi` = "\u0254\u026A"    # joy
    )

#================================================================#
# add a new column with the IPA symbols 
# by indexing its names using the vowel code
df$IPA <- vowel_lookup[df$vowel]

#================================================================#
# initiate list of vowels that you want to leave out
exclude_these_Vs <- as.character("")

# for most vowel plots, I want to leave these out. 
exclude_these_Vs <- 
    c("cr","er","ar","xx","ai","ait","oi","oh","au","ei")
#================================================================#
#df_sum <- df %>%
#df
      #dplyr::filter(!vowel %in% exclude_these_Vs) %>% 
      #group_by(vowel, IPA, time_index) %>%

px_v_space_smooth <-df%>%   # df_sum %>%
  ggplot(.)+
  #data = vowels, 
  aes(x = F2, y = F1, color = vowel, label = IPA) + 
  geom_text(size = 3) +
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") +
  geom_density_2d() +
  theme(legend.position = "none") +
  theme_classic()
px_v_space_smooth

# Save the plot
ggsave(px_v_space_smooth, file = "My_accent_r.png",
       height = 3.7, width = 4.8, dpi = 600)

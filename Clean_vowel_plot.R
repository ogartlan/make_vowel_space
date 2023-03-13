library(dplyr)
library(ggplot2)
library(readr)
library(zoo)


# set the path where your formant table lives
setwd("C:\\4th_Year\\Final_Year_Project\\accent_plots_r")

#df <- read.csv("my_formants_feb_cleaned.Table", stringsAsFactors = FALSE) 
#FUNCTION - reads in all files from a given directory that end in ".TABLE" and combines them into one data frame - "df"
combine_tables <- function(directory, pattern, header=TRUE) {
  # Get a list of all files matching the specified pattern in the directory
  file_list <- list.files(directory, pattern=".table", full.names=TRUE)
  
  # Read in each file as a data frame and combine them using rbind
  df <- do.call(rbind, lapply(file_list, read.table, header=header))
  
  return(df)
}

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
    c(`aa\\d?` = "\u0251",         # bot        ɑ-ɒ
      `ae\\d?` = "\u00E6",         # bat        æ
      `ah\\d?` = "\u028C",         # butt       ʌ
      `ao\\d?` = "\u0254",         # caught     ɔ
      `aw\\d?` = "\u0061\u028A",   # bout       aʊ
      `ax\\d?` = "\u0259",         # comma      ə
      `axr\\d?` = "\u0259\U02DE",  # letter     ɚ
      `ay\\d?` = "\u0061\u026A",   # bite       aɪ
      `eh\\d?` = "\u025B",         # bet        ɛ
      `er\\d?` = "\U025C\U02DE",   # bird       ɝ
      `ey\\d?` = "\u0065\u026A",   # bait       eɪ
      `ih\\d?` = "\u026A",         # bit        ɪ
      `ix\\d?` = "\u0268",         # rabbit     ɨ
      `iy\\d?` = "\u0069",         # beat       i
      `ow\\d?` = "\u006F\u028A",   # boat       oʊ
      `oy\\d?` = "\u0254\u026A",   # boy        ɔɪ
      `uh\\d?` = "\u028A",         # book       ʊ
      `uw\\d?` = "\u0075",         # boot       u
      `ux\\d?` = "\u0289r"         # dude       ʉ
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
    c("cr","ar","xx","ai","ait","oi","oh","au","ei")
#================================================================#
### ONLY TAKING THE FIRST N INSTANCES OF EACH VOWEL 
#df_sum <- df %>%
#df
      #dplyr::filter(!vowel %in% exclude_these_Vs) %>% 
      #group_by(vowel, IPA, time_index) %>%

# set the number of instances to subset for each vowel type
n <- 100

# subset the first n instances of every vowel type
df_subset <- df %>% 
  group_by(vowel) %>% 
  slice(seq_len(n))
#================================================================#

px_v_space_smooth <-df%>%   # df_sum %>%
  ggplot(.)+
  #data = vowels, 
  aes(x = F2, y = F1, color = vowel, label = IPA) + 
  geom_text(size = 3) +
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") +
  geom_density_2d() +
  theme(legend.position = "none") +
  theme_classic() #+
  #xlim(0, 1400) +
  #ylim(500, 4000)
px_v_space_smooth

# Save the plot
ggsave(px_v_space_smooth, file = "My_accent_r_cleaned.png",
       height = 3.7, width = 4.8, dpi = 600)

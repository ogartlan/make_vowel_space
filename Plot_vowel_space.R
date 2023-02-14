library(ggplot2)
library(dplyr)
library(zoo)
rm(list = ls())

# set the path where your formant table lives
setwd("C:\\4th_Year\\Final_Year_Project\\my_voice")

df <- read.csv("my_formants.Table", stringsAsFactors = FALSE) 

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
# if you recorded multiple versions of the same vowel,
# then average over them here
#   MY OWN ATTEMPT AT GETTING AN AVERAGE FOR EACH UTTERANCE
#df_avgs <- df %>%
# number of rows in the data frame
num_rows <- nrow(df) - 1

#making a vector (array) of length = number of rows / 10
F1_avgs <- numeric(num_rows/10)
F2_avgs <- numeric(num_rows/10)

F1_current <- 0
F2_current <- 0

q <- 0
while (q <= num_rows/10){
    for (i in 1:10){
        F1_current <- F1_current + df[i + q, "F1"]
        F2_current <- F2_current + df[i + q, "F2"]
        }
    F1_avgs[q+1] <- F1_current / 10
    F2_avgs[q+1] <- F2_current / 10
    F1_current <- 0
    F2_current <- 0
    q <- q + 1
}

#plot(F1_avgs, F2_avgs)

df1 <- data.frame(x = F1_avgs, y = F2_avgs)
ggplot(df1, aes(x, y)) + 
geom_point() +
scale_x_reverse(position = "top", name = "F2 (Hz)")+
scale_y_reverse(position = "right", name = "F1 (Hz)")+
theme_bw()+
theme(legend.position = "none")


#================================================================#
#df_sum <- df %>%
#    # Filters out vowels taht are in the "excluded" list
#    dplyr::filter(!vowel %in% exclude_these_Vs) %>% 
#   # group rows by column values in the DataFrame. 
#    # Because of the %>% above, the data frame is
#    # the filtered csv
#    group_by(vowel, IPA, time_index) %>%
#    summarise(F1 = mean(F1, na.rm = TRUE), # na.rm skips over any NA values
#              F2 = mean(F2, na.rm = TRUE),
#              F3 = mean(F3, na.rm = TRUE)) %>%
#    group_by(vowel, IPA) %>%
#    # create a 5-sample rolling average
#    # mutate is for adding new variables
#    mutate(F1s = zoo::rollmean(F1, 5, na.pad = TRUE), # na.pad adds the NAs back in to fit the original data frame.
#           F2s = zoo::rollmean(F2, 5, na.pad = TRUE),
#           F3s = zoo::rollmean(F3, 5, na.pad = TRUE))

#================================================================#
# Make another data frame of just the trajectory endpoints
# **In the original code, time_index == 9 in all cases below. 
# ** Since I am only using this for simple vowels, not diphthongs,
# ** I will grab it from the middle of the vowel, i.e. time 5.
#df_endpt <- df_sum %>%
#    dplyr::filter(!vowel %in% exclude_these_Vs) %>%
#    group_by(vowel, IPA) %>%
#    summarise(
#        # F1s is ....
#        F1s = F1s[time_index == 5],
#        F2s = F2s[time_index == 5],
#        F3s = F3s[time_index == 5],
#        #
#        # F1 is ....
#        F1 = F1[time_index == 5],
#        F2 = F2[time_index == 5],
#        F3 = F3[time_index == 5])
#
#================================================================#
# Plot it!
#px_v_space_smooth <- df_sum %>%
#    dplyr::filter(time_index > 1, time_index < 10) %>%
#    ggplot(.)+
#    aes(x = F2, y = F1, group = vowel, color = vowel)+
#    # trajectory ** commented out for now
#    #geom_path(linewidth = 1.1)+
#    # IPA symbol encapsulated in a label
#    geom_label(data = df_endpt,label.padding = unit(0.2, "line"), 
#               aes(label = IPA))+
#    # another layer of text for the IPA symbol
#    # to ensure that it is visible,
#    # but still shows a trace of the underlying color
#    geom_text(data = df_endpt,
#              aes(label = IPA),
#              color = "black", alpha = 0.5)+
#    scale_x_reverse(position = "top", name = "F2 (Hz)")+
#    scale_y_reverse(position = "right", name = "F1 (Hz)")+
#    theme_bw()+
#    theme(legend.position = "none")
#px_v_space_smooth

# Save the plot
#ggsave(px_v_space_smooth, file = "My_vowel_no_diph.png",
#       height = 3.7, width = 4.8, dpi = 600)
#End!

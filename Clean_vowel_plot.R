  library(dplyr)
  library(ggplot2)
  library(readr)
  library(zoo)

  # set the path where your formant table lives
  setwd("D:\\american\\tables_speaker1")

  df <- data.frame(vowel = character(),
                   time_index = numeric(),
                   v_time = numeric(),
                   time_abs = numeric(),
                   F1 = numeric(),
                   F2 = numeric(),
                   F3 = numeric(),
                   stringsAsFactors = FALSE)

  # get a list of all .Table files in the directory
  file_list <- list.files(pattern = "\\.table$")
  #print(file_list)

  # loop over each file and read it in using read.csv
  for (file in file_list) {
    # construct the file path
    file_path <- file.path(getwd(), file)

    # read in the file using read.csv
    df1 <- read.csv(file_path, stringsAsFactors = FALSE)

    df <- rbind(df, df1)
  }

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
      c(`AA0` = "\u0251",         # bot        ɑ-ɒ
        `AA1` = "\u0251",         # bot        ɑ-ɒ
        `AA2` = "\u0251",         # bot        ɑ-ɒ
        `AE0` = "\u00E6",         # bat        æ
        `AE1` = "\u00E6",         # bat        æ
        `AE2` = "\u00E6",         # bat        æ
        `AH0` = "\u0259",         # butt       ʌ schwa
        `AH1` = "\u0259",         # butt       ʌ schwa
        `AH2` = "\u0259",         # butt       ʌ schwa
        `AO0` = "\u0254",         # caught     ɔ
        `AO1` = "\u0254",         # caught     ɔ
        `AO2` = "\u0254",         # caught     ɔ
        `AW0` = "\u0061\u028A",   # bout       aʊ
        `AW1` = "\u0061\u028A",   # bout       aʊ
        `AW2` = "\u0061\u028A",   # bout       aʊ
        `AY0` = "\u0061\u026A",   # bite       aɪ
        `AY1` = "\u0061\u026A",   # bite       aɪ
        `AY2` = "\u0061\u026A",   # bite       aɪ
        `EH0` = "\u025B",         # bet        ɛ
        `EH1` = "\u025B",         # bet        ɛ
        `EH2` = "\u025B",         # bet        ɛ
        `ER0` = "\U025C\U02DE",   # bird       ɝ
        `ER1` = "\U025C\U02DE",   # bird       ɝ
        `ER2` = "\U025C\U02DE",   # bird       ɝ
        `EY0` = "\u0065",         # bait       e
        `EY1` = "\u0065",         # bait       e
        `EY2` = "\u0065",         # bait       e
        `IH0` = "\u026A",         # bit        ɪ
        `IH1` = "\u026A",         # bit        ɪ
        `IH2` = "\u026A",         # bit        ɪ
        `IY0` = "\u0069",         # beat       i
        `IY1` = "\u0069",         # beat       i
        `IY2` = "\u0069",         # beat       i
        `OW0` = "\u006F",         # boat       o
        `OW1` = "\u006F",         # boat       o
        `OW2` = "\u006F",         # boat       o
        `OY0` = "\u0254\u026A",   # boy        ɔɪ
        `OY1` = "\u0254\u026A",   # boy        ɔɪ
        `OY2` = "\u0254\u026A",   # boy        ɔɪ
        `UH0` = "\u028A",         # book       ʊ
        `UH1` = "\u028A",         # book       ʊ
        `UH2` = "\u028A",         # book       ʊ
        `UW0` = "\u0075",         # boot       u
        `UW1` = "\u0075",         # boot       u
        `UW2` = "\u0075"          # boot       u
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
      c("AW0","AW1","AW2","AY0","AY1","AY2","OY0","OY1","OY2")
	  
  # making a new dataframe which does not include diphthongs
  df_excl <- df[!df$vowel %in% exclude_these_Vs, ]
  
  #================================================================#
  ## ONLY TAKING STEADY-STATE OF FORMANTS (Within 10% of time_index=5 value)
  # adds a column total_time_index with values 1 to however long the file is
  df_excl$total_time_index <- seq(1, nrow(df_excl))
  print("tti created and added to df_excl")
  # Create steady_state_df to house only steady state formant values
  steady_state_df <- data.frame(vowel = character(),
                   time_index = numeric(),
                   v_time = numeric(),
                   time_abs = numeric(),
                   F1 = numeric(),
                   F2 = numeric(),
                   F3 = numeric(),
                   IPA = character(),
                   total_time_index = numeric(),
                   stringsAsFactors = FALSE)
  print("steady_state_df created")

  medians_only_df <- data.frame(IPA = character(),
                   F1 = numeric(),
                   F2 = numeric(),
                   stringsAsFactors = FALSE)

  master_medians_df = df[FALSE,]

  plotting_medians_df <- data.frame(IPA = character(),
                   F1 = numeric(),
                   F2 = numeric(),
                   stringsAsFactors = FALSE)

  i <- 0
  offset = 0
  print(offset)
  unwanted_values <- c(1:3, 8:10)
  print(unwanted_values)
  print((1 + offset) : (3 + offset))
  print((8 + offset): (10 + offset))
  # i.e. FOR EVERY COLLECTION OF 10 VALUES, STARTING AT THE FIRST ONE:
  while (i <= (nrow(df_excl) / 10)) {
    #NOT USING THE FIRST AND LAST 2 TIMESTAMPS AS THESE ARE UNLIKELY TO BE STEADY STATE
    #median_df <- df_excl[(3 + (10*i)):(8 + (10*i)), ]
	#steady_state_df <- df_excl[(3 + (10*i)):(8 + (10*i)), ]
	#df_excl <- df_excl[!df$total_time_index %in% unwanted_values, ]
    #print("3rd plus 10i of df now: ")
    #print(df[3 + (10*i)])
    ##median_df <- arrange(median_df, F1)
    ##median_F1 <- (median_df[3, "F1"] + median_df[4, "F1"]) / 2
    ##median_df <- arrange(median_df, F2)
    ##median_F2 <- (median_df[3, "F2"] + median_df[4, "F2"]) / 2
    #median_F2 <- median(subset(median_
	offset = 10 * i
    df_excl <- df_excl[!df_excl$total_time_index %in% c((1 + offset) : (3 + offset)), ]
	df_excl <- df_excl[!df_excl$total_time_index %in% c((8 + offset): (10 + offset)), ]
    ##temp_df <- data.frame()
    ##temp_df <- rbind(temp_df, df[3 + (10*i), ])
    #print("first")
    #print(temp_df)
    ##temp_df[1, 5] <- median_F1
    ##temp_df[1, 6] <- median_F2
    #print("then")
    #print(temp_df)
    ##master_medians_df <- rbind(master_medians_df, temp_df)
    #print(master_medians_df)
    #print("F1")
    #print(median_F1)
    #print("F2")
    #print(median_F2)
    #print(master_medians_df)
    i <- i + 1
  }

  print(df_excl)

  

  IPA_list <- c("\u0251", "\u00E6", "\u0259", "\u0254", "\u0061\u028A", "\u0061\u026A", "\u025B", "\U025C\U02DE", "\u0065", "\u026A", "\u0069", "\u006F", "\u0254\u026A", "\u028A", "\u0075")
  
  m <- 1
  
  
 # MEDIAN PLOTTING -----------------------
 while (m <= length(IPA_list)) {
    plotting_medians_df[m,1] <- IPA_list[m]
    plotting_medians_df[m,2] <- median(subset(master_medians_df, IPA == IPA_list[m])$F1)
    plotting_medians_df[m,3] <- median(subset(master_medians_df, IPA == IPA_list[m])$F2)
    
    #print(IPA_list[m])
    #print(plotting_medians_df[m, 2])
    m <- m + 1
    
 }

  #================================================================#
  ### ONLY TAKING THE FIRST N INSTANCES OF EACH VOWEL 

  # set the number of instances to subset for each vowel type
  n <- 3

  # subset the first n instances of every vowel type
  df_subset <- df_excl %>% ##steady_state_df %>% 
    group_by(IPA) %>% 
    slice(seq_len(n))
  #================================================================#
  print(df_subset)

  px_v_space_smooth <- df_subset%>% #plotting_medians_df%>%   # df_sum %>%
    ggplot(.)+
    #data = vowels, 
    aes(x = F2, y = F1, color = IPA, label = IPA) + 
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
  ggsave(px_v_space_smooth, file = test00.png",
         height = 3.7, width = 4.8, dpi = 600)

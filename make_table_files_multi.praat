################################################
#
#  Extract formants from annotated textgrids
#
################################################
#
# Preparation: 
# work with a recording that contains the vowels 
#     that you want to analyze
#
# Create a formant object for that sound,
# and do it carefully
#  (ensure the formant settings are appropriate 
#    for the individual talker's voice)
#
# Mark the intervals that you want to analyze
#   in a Textgrid, like this:
#
#    ~~~~~~~~------~~~~~~~---~~~~~~~---
#   __________________________________
#    |   ah  |     |  ih  |  |  eh  |
#
################################################
#
# Name of the Textgrid file that you annotated
# (should also be the name of the Formant object)#
name$ = "G00007S1001"

# List all the formant files from a given folder
folder$ = "D:\FYP\Automation\test_output\"
Create Strings as file list: "list", "'folder$'*.TextGrid"
numberOfFiles = Get number of strings

# How many timepoints?
num_timepoints = 10

# Which tier are your annotations?
v_tier = 2

# Loop through each file
for ifile to numberOfFiles
    # Load the textgrid correspoding to the current formant file
    select Strings list
    filename$ = Get string: ifile
    gridBase$ = replace$(filename$, ".TextGrid", "", 0)
    Read from file: folder$ + gridBase$ + ".formant"
    Read from file: folder$ + gridBase$ + ".TextGrid"
 
    # Make the table
	Create Table with column names: "formants_'ifile'", 0, "vowel time_index v_time time_abs F1 F2 F3"
	#select Table formants
	#Rename: "table_'gridBase$'"
	row_index = 0

    # Count the intervals
	select TextGrid 'gridBase$'
	num_intervals = Get number of intervals: v_tier

	# Loop through the intervals
	for interval_index from 1 to num_intervals
		select TextGrid 'gridBase$'
		label$ = Get label of interval: v_tier, interval_index

		# proceed if the label contains a vowel (here denoted by starting with a vowel and having at least one thing after it)
		if index_regex(label$, "^[AEIOU]+")
			t_start = Get start time of interval: v_tier, interval_index
			t_end = Get end time of interval: v_tier, interval_index
			time_interval = (t_end - t_start)/(num_timepoints-1)

			selectObject: "Formant 'gridBase$'"

			# Loop through the timepoints
			for time_index from 1 to num_timepoints
				time_re_onset = (time_index-1)*time_interval
				current_time =  t_start + time_re_onset
				select Formant 'name$'
				f1 = Get value at time: 1, current_time, "hertz", "Linear"
				f2 = Get value at time: 2, current_time, "hertz", "Linear"
				f3 = Get value at time: 3, current_time, "hertz", "Linear"

			     # Add info to the table
				select Table formants_'ifile'
				Insert row: row_index + 1
				row_index = row_index + 1
				Set string value: row_index, "vowel", label$
				Set numeric value: row_index, "time_index", time_index
				Set numeric value: row_index, "v_time", 'time_re_onset:3'
				Set numeric value: row_index, "time_abs", 'current_time:3'
				
				if f1 != undefined
					Set numeric value: row_index, "F1", 'f1:0'
				else
					Set string value: row_index, "F1", "NA"
				endif
				
				if f2 != undefined
					Set numeric value: row_index, "F2", 'f2:0'
				else
					Set string value: row_index, "F2", "NA"
				endif
				
				if f3 != undefined
					Set numeric value: row_index, "F3", 'f3:0'
				else
					Set string value: row_index, "F3", "NA"
				endif

			endfor

		# end conditional if label isn't a vowel
		endif
	endfor

	# end loop through the intervals
    # Print a message indicating that the script has finished processing the current file
      
    # printf("Processed file %d of %d: %s\n", fileIndex, numberOfFiles, currentFile$)
	
endfor

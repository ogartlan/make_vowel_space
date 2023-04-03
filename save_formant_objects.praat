# Start with an input form
form Enter directory to save your formants
     sentence directory C:\Type Directory Here
endform

# Ask the user to select the formants they wane to save
pause select all formants you want to save
numberOfSelectedFormants = numberOfSelected ("Formant")

# Assign an object number to each formant object
for thisSelectedFormant to numberOfSelectedFormants
	formant'thisSelectedFormant' = selected("Formant",thisSelectedFormant)
endfor

# Loop through the formants
for thisFormant from 1 to numberOfSelectedFormants
    select formant'thisFormant'
	name$ = selected$("Formant")

	# Old style of Praat coding, but it still works
	do ("Save as text file...", directory$ + "\" + name$ + ".formant")

endfor

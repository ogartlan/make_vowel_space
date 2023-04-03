# Start with an input form
form Enter directory to save your tables
     sentence directory C:\Type Directory Here
endform

# Ask the user to select the tables they wane to save
pause select all tables you want to save
numberOfSelectedTables = numberOfSelected ("Table")

# Assign an object number to each table object
for thisSelectedTable to numberOfSelectedTables
	table'thisSelectedTable' = selected("Table",thisSelectedTable)
endfor

# Loop through the tables
for thisTable from 1 to numberOfSelectedTables
    select table'thisTable'
	name$ = selected$("Table")

	# Old style of Praat coding, but it still works
	do ("Save as text file...", directory$ + "\" + name$ + ".table")

endfor

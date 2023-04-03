These are files provided for COMP1204 CW1.

File info:
- al*.kml 
	These are the kml files containing storm data that need to be processed.
	There are three files to run the script against.
	A storm map should be generated for each one (using create_map_plot.sh).
	These map plots are located in the report.
- create_csv.sh
	The create_csv script should make csv files based on data extracted from the kml files.
- create_map_plot.sh
	This script generates plots from csv files. 
	Create_map_plot.sh is used by the following command
		./create_map_plot.sh storm_data.csv plot.png 
	to create a map of the storm data.
	NOTE: For this script to work correctly, it must be in the same location as plot-locations-on-map.gpi and world-50m.txt.
- plot-locations-on-map.gpi
	This is the gnuplot code for creating the storm plots. 
	It is run by the create_map_plot.sh script, so it shouldn't need to run or be modified in any way.
- world-50m.txt
	This contains information used to draw the world map background in the storm plots. 
	It is used by the plot-locations-on-map.gpi script.
- report.tex
	This is the Latex file of the report.
- report.pdf
	This is the pdf file of the report.

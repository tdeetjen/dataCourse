"""
Applied Optimization Course
Creating a Likert chart of the EIA generator data
Thomas Deetjen
"""

#define the function "make_EIA_data_likert_chart_single"
def make_EIA_data_likert_chart_single():
	#this function takes no arguments. It reads in generator data from the csv file specified below and produces a likert chart showing the total amount of generation from different power plant technologies in the United States in 2016.	
	
	
	###tell python that it needs to use the "pandas", "scipy", and "matplotlib" modules to implement this function.
	#pandas helps manage and manipulate data frames. scipy performs scientific and mathematical operations. matplotlib helps with creating charts.
	import pandas
	import scipy
	import matplotlib
	
	
	###start by reading in the csv file and manipulating that data into a smaller, aggregated dataframe for our chart
	#read in the csv file and store it in a pandas dataframe called "data"
	data = pandas.read_csv('merged_2016_f860_f923_data.csv')
	#create a "d2" dataframe that holds a portion of the "data" dataframe. ".groupby('Technology').agg('sum')" groups the rows by the Technology column and aggregates them using a sum. These functions are inherent for any pandas dataframe and can be called as an extension of the "data[['Technology','Net.Generation..Megawatthours.', 'Nameplate.Capacity']]" dataframe 
	d2 = data[['Technology','Net.Generation..Megawatthours.', 'Nameplate.Capacity']].groupby('Technology').agg('sum')
	#rename the columns. Note that the different technology names (the "tech" column in the R script) are held in the dataframe index in this example. run "d2.index" to see that. This means that the call "d2[0]", which would normally return the first row of the dataframe, now throws an error because "0" is not one of the indices.
	d2.columns = ['MWH_gen', 'MW_cap']
	#add the TWh column to convert the energy to the Terra-watt-hour unit that has better significant figures for our chart.
	d2['TWH_gen'] = d2.MWH_gen / 1000000
	#sort the dataframe, descending, by the energy quantities
	d2 = d2.sort('TWH_gen', ascending = False)
	#define custom colors green1 and blue1. Note that rgb colors in python need to be in the range of 0 to 1, so dividing by 255. will translate from a 0:255 format to a 0:1 format. Also note that we are converting the rgb to a hex format that's easier to use in python. The [0:7] call takes the first 6 digies of the hex format. There are other python packages geared towards handling color objects more elegantly.
	green1 = matplotlib.colors.rgb2hex(scipy.divide((38, 150, 18, 255), 255.))[0:7]
	blue1 = matplotlib.colors.rgb2hex(scipy.divide((12, 86, 126, 255), 255.))[0:7]
	#assign green1 to the rows with positive TWh values and blue1 to the rows with negative TWh values.	 
	d2['bar_col'] = green1
	d2.ix[d2.TWH_gen<0,'bar_col'] = blue1
	
	
	###now that we have our dataframe how we want it, we can start building our chart
	#for simplicity, we can define a "names" array and a "y_position" array that tells where on the y axis each bar will go. In this case, we are just evenly spacing them between y = 0 to 25
	names = d2.index
	y_position = scipy.arange(len(names))
	#define "fig" as a figure of size 10,6 with a white background
	fig = matplotlib.pyplot.figure(figsize=(10,6), facecolor='white')
	#define "ax" as an axes object that will hold the x y coordinates for our plot
	ax = matplotlib.pyplot.axes()
	#plot bar charts using the 1 to 26 "y_position" array for the y values, the TWH_gen information for the x values, and the bar_col information for the colors. We use arcsinh here to scale the bars better so 0.77 and 1145.6 can show up visually on the same chart
	ax.barh(y_position, scipy.arcsinh(d2.TWH_gen), color=d2.bar_col)
	
	
	###now we have the basic chart elements and can start annotating it
	#create tick labels for the x axis located at the appropriate arcsinh locations
	matplotlib.pyplot.xticks(scipy.arcsinh((-10, -1, 0, 1, 10, 100, 1000)), (-10, -1, 0, 1, 10, 100, 1000))
	#get rid of the top x axis ticks, and remove the y axis ticks
	ax.xaxis.set_ticks_position('bottom')
	ax.yaxis.set_ticks([])
	#a "spine" is the locational information for where the left, right, top, and bottom axes labels are. Here, we are removing all spines except for the bottom one. We set the position of the bottom spine 20 lower than normal to make room for the copyright text. smart_bounds keeps the x axis line from spanning the whole page.	
	for loc, spine in ax.spines.items():
		if loc in ['left', 'right', 'top']:
			spine.set_color('none') #dont' draw the spine
		elif loc in ['bottom']:
			spine.set_position(('outward', 20))
			spine.set_smart_bounds(True)
	#this script iterates through the 0:25 y_position array and adds text with the technology and the TWh for each technology. 'ha' stands for horizontal alignment. 
	for i in y_position:
		if d2.TWH_gen[i] < 0:
			ax.text(0.1, i, names[i], ha='left', color=blue1)
			ax.text(scipy.arcsinh(d2.TWH_gen[i])-0.1, i, str(round(d2.TWH_gen[i],2)) + ' TWh', ha='right', color=blue1)
		else:
			ax.text(-0.1, i, names[i], ha='right', color=green1)
			ax.text(scipy.arcsinh(d2.TWH_gen[i])+0.1, i, str(round(d2.TWH_gen[i],2)) + ' TWh', ha='left', color=green1)
	#add a title. the '\n' creates a new line
	matplotlib.pyplot.title('Total generation from natural gas surpassed coal and energy storage \n technologies consumed about 6.7 TWh of electricity in 2016', fontsize=18)
	#set the locatoin of the title
	ax.title.set_position([0.5, 0.9])
	#set the x axis label
	matplotlib.pylab.xlabel('Net electricity generated in the US (TWh)', fontsize=18)
	#add the asterisk information below the x axis
	ax.text(1.6, -6, '*EIA F860 & F923 data. Net = (output - input). Flywheel data unclear as they are reported as parts of cogen plants', ha='center', fontsize=10)
	#add the copyright above the x axis
	ax.text(1.6, -1.1, 'Copyright Joshua D. Rhodes, PhD | Energy Institute, The University of Texas at Austin | @joshdr83', ha='center', fontsize=8)
	#add the y axis text. This could also be done using matplotlib.pylab.ylabel() and adjusting the spine further outward to accomodate the added technology text. Without adjusting the spine, the ylabel will overlap the technology names.
	ax.text(-5.5, 16, 'Technology', ha='left', fontsize=18, rotation=90)
	
	
	###now the figure is all compiled. We can save it as a pdf and png at whatever dpi we need.
	fig.savefig('EIA_likert_chart_python.pdf', dpi=250)
	fig.savefig('EIA_likert_chart_python.png', dpi=250)


#run the function. It doesn't take any argument yet, but we can add some later if we want.	
make_EIA_data_likert_chart_single()

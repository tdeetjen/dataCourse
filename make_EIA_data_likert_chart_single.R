#define the function "make_EIA_data_likert_chart_single"
make_EIA_data_likert_chart_single <- function(){
  #this function takes no arguments. It reads in generator data from the csv file specified below and produces a likert chart showing the total amount of generation from different power plant technologies in the United States in 2016.

  #tell R that it needs to use the "plotrix" and "animation" packages to implement this function     
  library(plotrix)
  library(animation)
  
  #read in the csv file and store it in a data.frame called "data". Be sure that the "merged_2016_f860_f923_data.csv" file is in your working directory.
  data <- read.csv('merged_2016_f860_f923_data.csv')
  #the aggregate function will compile "data" into something more manageable, stored in a data.frame called "d2". As written, this command takes the sum of the "Net.Generation..Megawatthours" and "Nameplate.Capacity" columns aggregated by the power plant type in the "Technology" column.
  d2 <- aggregate(data[c('Net.Generation..Megawatthours.', 'Nameplate.Capacity')], by = list(data$Technology), FUN = sum)
  #rename the column headers for d2 so they are easier to call in our function
  names(d2) <- c('tech', 'MWH_gen', 'MW_cap')
  #convert MWh to TWh so we have something with more manageable significant figures. Note: 1 TWh (terra-watt-hour) equals 1-Million MWh (mega-watt-hours) and expresses an amount of energy.
  d2$TWH_gen <- d2$MWH_gen/1000000
  #by default, the rows in d2 are sorted by alphabetical order according to the column we aggregated over (data$Technology), but we want to sort it by energy (TWh). "with(d2, order(-TWH_gen))" produces an array of indices that shows where each row ranks in terms of TWh. For example, "with(d2, order(-TWH_gen))[1]" returns a "10", meaning that the 10th row of d2 has the 1st highest ranking of TWh. "d2[##, ]" returns the data from all columns of d2 for the "##" row. So, "d2[with(d2, order(-TWH_gen)), ]" uses the array of TWh rankings to return d2 data, row by row, in order of the TWh rankings. "d2 <- " assigns that re-ordered data.frame back the object "d2", effectively deleting the old version of d2 and replacing it with one whose rows are sorted by TWh.
  d2 <- d2[with(d2, order(-TWH_gen)), ]
  
  #define green and blue colors to use later in the chart
  green1 <- rgb(38,150,18, max=255)
  blue1 <- rgb(12,86,126, max=255)
  
  #d2$xxxx creates a new column for d2 called "xxxx". In this case, we're assigning the color information stored in "green1" to all of the generators and then replacing that with the color information stored in "blue1" for any generators whose annual TWh is less than 0. This will color the bars of the chart green unless there is a generator with negative energy (or positive energy consumption), which will be colored blue.
  d2$bar_col <- green1
  d2$bar_col[d2$TWH_gen < 0] <- blue1
  
  #create a pdf canvas to put our chart on. This will create a pdf file and leave it opened within R while we add visualizations to it
  pdf(file = 'EIA_likert_chart.pdf', width = 10, height = 6)
  
  #set background color to white)
  par(bg = 'white')
  
  #create a barchart of the energy data using the barplot function. "height = asinh(d2$TWH_gen)" sets the bar height equal to the hyperbolic arc-sine of each row's TWh data. This lets us use an exponential x-axis so that 0.77 and 1145.6 can show up visually on the same chart.
  mid <- barplot(height = asinh(d2$TWH_gen), horiz = T, xlim = c(-7, 10), xaxt = 'n', col = d2$bar_col)
  
  #add some text in the chart for each bar saying what the generation technology (d2$tech) is. The "y =" and "x =" arguments show where the text should be placed in the x-y axes. For mid[24] through mid[26] (the negative TWh parts of the bar plot) we use blue1 colored text to match the blue1 colored bars. Use green1 colored text for the rest.
  text(y = mid[24:26], x = c(0), labels = paste(' ', d2$tech[24:26], sep = ''), adj = c(0,.5), col = blue1, cex = 0.9)
  text(y = mid[1:23], x = c(0), labels = paste(d2$tech[1:23], ' ', sep = ''), adj = c(1,0.5), col = green1, cex = 0.9)
  
  #add some text in the chart for each bar saying what the total TWh value is. The "paste" command combines multiple pieces of information into a single string. 
  text(y = mid[24:26], x = asinh(d2$TWH_gen[24:26]), labels = paste(round(d2$TWH_gen[24:26], 2), ' TWh ', sep = ''), adj = c(1,0.5), col = blue1, cex = 0.9)
  text(y = mid[1:23], x = asinh(d2$TWH_gen[1:23]), labels = paste(' ', round(d2$TWH_gen[1:23], 2), ' TWh', sep = ''), adj = c(0,0.5), col = green1, cex = 0.9)
  
  #create the x axis. "at =" shows where the labels should be located, and "labels =" shows the names that those labes should be given. Note that we use "at = c(...)" where "at =" must be given a single object and "c(...)" is a numeric array that contains multiple pieces of information but can be supplied to "at =" as a single object.
  axis(side = 1, at = c(asinh(-10), asinh(-1), 0, asinh(1), asinh(10), asinh(100), asinh(1000)), labels = c(-10, -1, 0, 1, 10, 100, 1000))
  
  #add some more text to the chart, like the title and axes titles.
  mtext(side = 1, text = "Net electricity generated in the US (TWh)", line = 2.5, cex = 1.5)
  mtext(side = 1, text = '*EIA F860 & F923 data. Net = (output - input). Flywheel data unclear as they are reported as parts of cogen plants', line = 3.5, cex = 0.75)
  mtext(side = 1, text = 'Copyright Joshua D. Rhodes, PhD | Energy Institute, The University of Texas at Austin | @joshdr83', line = -1, cex = 0.6)
  mtext(side = 2, text = 'Technology', cex = 1.5, line = -1)
  
  #title
  mtext(side = 3, text = 'Total generation from natural gas surpassed coal and energy storage \n technologies consumed about 6.7 TWh of electricity in 2016', cex = 1.5)
  
  #in general, "dev.off()" shuts off the current device. In this case, that device is the chart and pdf we are creating. If we don't call "dev.off()" here, then the pdf and chart will stay opened inside R without being published and saved.
  dev.off()
  
  #this line uses the ImageMagick package to copy the pdf we just created and save it as a png file. This line of code won't work unless you have ImageMagick properly installed in R.
  #im.convert('EIA_likert_chart.pdf', output = 'EIA_likert_chart.png', extra.opts="-density 150")
  
}

#now that we have built the function, we need to call it. We could add some functionality to this later, perhaps giving it an argument that takes "energy" to plot TWh or "power" to plot the installed MW capacity of the different power plant types. For now, the function takes no arguments, so we just open and close that arguments portion with some empty paretheses.
make_EIA_data_likert_chart_single()

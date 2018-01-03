make_EIA_data_likert_chart_single <- function(){
  
  library(plotrix)
  library(animation)
  
  data <- read.csv('merged_2016_f860_f923_data.csv')
  d2 <- aggregate(data[c('Net.Generation..Megawatthours.', 'Nameplate.Capacity')], by = list(data$Technology), FUN = sum)
  names(d2) <- c('tech', 'MWH_gen', 'MW_cap')
  d2$TWH_gen <- d2$MWH_gen/1000000
  d2 <- d2[with(d2, order(-TWH_gen)), ]
  
  green1 <- rgb(38,150,18, max=255)
  blue1 <- rgb(12,86,126, max=255)
  
  d2$bar_col <- green1
  d2$bar_col[d2$TWH_gen < 0] <- blue1
  

  pdf(file = 'EIA_likert_chart.pdf', width = 10, height = 6)
  
  par(bg = 'white')
  
  mid <- barplot(height = asinh(d2$TWH_gen), horiz = T, xlim = c(-7, 10), xaxt = 'n', col = d2$bar_col)
  
  text(y = mid[24:26], x = c(0), labels = paste(' ', d2$tech[24:26], sep = ''), adj = c(0,.5), col = blue1, cex = 0.9)
  text(y = mid[1:23], x = c(0), labels = paste(d2$tech[1:23], ' ', sep = ''), adj = c(1,0.5), col = green1, cex = 0.9)
  
  text(y = mid[24:26], x = asinh(d2$TWH_gen[24:26]), labels = paste(round(d2$TWH_gen[24:26], 2), ' TWh ', sep = ''), adj = c(1,0.5), col = blue1, cex = 0.9)
  text(y = mid[1:23], x = asinh(d2$TWH_gen[1:23]), labels = paste(' ', round(d2$TWH_gen[1:23], 2), ' TWh', sep = ''), adj = c(0,0.5), col = green1, cex = 0.9)
  
  axis(side = 1, at = c(asinh(-10), asinh(-1), 0, asinh(1), asinh(10), asinh(100), asinh(1000)), labels = c(-10, -1, 0, 1, 10, 100, 1000))
  
  mtext(side = 1, text = "Net electricity generated in the US (TWh)", line = 2.5, cex = 1.5)
  mtext(side = 1, text = '*EIA F860 & F923 data. Net = (output - input). Flywheel data unclear as they are reported as parts of cogen plants', line = 3.5, cex = 0.75)
  mtext(side = 1, text = 'Copyright Joshua D. Rhodes, PhD | Energy Institute, The University of Texas at Austin | @joshdr83', line = -1, cex = 0.6)
  mtext(side = 2, text = 'Technology', cex = 1.5, line = -1)
  
  mtext(side = 3, text = 'Total generation from natural gas surpassed coal and energy storage \n technologies consumed about 6.7 TWh of electricity in 2016', cex = 1.5)
  
  dev.off()
  
  im.convert('EIA_likert_chart.pdf', output = 'EIA_likert_chart.png', extra.opts="-density 150")
  

  
}
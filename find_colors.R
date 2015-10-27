###############################################################################
###############################################################################
#Script for displaying base colors available in R
###############################################################################
###############################################################################

#this R code Earl F. Glynn. You can find it on this webpage: 
#http://research.stowers-institute.org/efg/R/Color/Chart/
#A function to display the different colors you can select in R

#first step, a function to have a good contrast between the background and the 
#text
SetTextContrastColor <- function(color)
{
  ifelse( mean(col2rgb(color)) > 127, "black", "white")
}
#Define this array of text contrast colors that correponds to each
#member of the colors() array.
TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )


#Plot matrix of R colors, in index order, 25 per row.
#This example plots each row of rectangles one at a time.
colCount <- 25 # number per row
rowCount <- 27
plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0))
title("R colors")

for (j in 0:(rowCount-1))
{
  base <- j*colCount
  remaining <- length(colors()) - base
  RowSize <- ifelse(remaining < colCount, remaining, colCount)
  rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
       border="black",
       col=colors()[base + (1:RowSize)])
  text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
       col=TextContrastColor[base + (1:RowSize)])
}


#same plot but colors sorted by hue-saturation-value
#Plot matrix of R colors, in "hue" order, 25 per row.
#This example plots each rectangle one at a time.
RGBColors <- col2rgb(colors()[1:length(colors())])
HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,],
                      maxColorValue=255)
HueOrder <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )
plot(0, type="n", ylab="", xlab="",
     axes=FALSE, ylim=c(rowCount,0), xlim=c(1,colCount))

title("R colors -- Sorted by Hue, Saturation, Value")

for (j in 0:(rowCount-1))
{
  for (i in 1:colCount)
  {
    k <- j*colCount + i
    if (k <= length(colors()))
    {
      rect(i-0.5,j-0.5, i+0.5,j+0.5, border="black", col=colors()[ HueOrder[k] ])
      text(i,j, paste(HueOrder[k]), cex=0.7, col=TextContrastColor[ HueOrder[k] ])
    }
  }
}


#same plot as the first one, but instead of the number, the name of the color
#is displayed in the rectangle. Plot matrix of R colors, in index order, 
#25 per row
colCount <- 25 # number per row
rowCount <- 27
plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0))
title("R colors")

for (j in 0:(rowCount-1))
{
  base <- j*colCount
  remaining <- length(colors()) - base
  RowSize <- ifelse(remaining < colCount, remaining, colCount)
  rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
       border="black",
       col=colors()[base + (1:RowSize)])
  text((1:RowSize), j, colors()[(base + (1:RowSize))], cex=0.4,
       col=TextContrastColor[base + (1:RowSize)])
}


###############################################################################
#END
###############################################################################
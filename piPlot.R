library(plotrix)
library(RColorBrewer)

# Grab raw digits from file (first 10K digits of fractional pi)
rawPiDigits <- readLines("PI10K_DP.txt")

# convert to a data.frame
piDigits <- data.frame(digit = as.numeric(unlist(strsplit(rawPiDigits,""))))

# create row index for the list of digits (I'd like a better solution to this)
piDigits$idx <- 1:nrow(piDigits) -1

# compute remainder, mod 360 for the degree measure
piDigits$degs<- sapply(piDigits$idx,FUN=function(x){
    as.numeric(x %% 360)
})

# quotient mod 360
piDigits$q <- sapply(piDigits$idx,FUN=function(x){
    as.numeric((x %/% 360) + 1) 
})

# normalize by the maximum rotation of degrees
norm <- max(piDigits$q)

# create color mapping function and map colors
piColorFunc <- colorRampPalette(brewer.pal(10,"Spectral"))
piColors <- piColorFunc(10)

# create plot, add text
polar.plot((piDigits$digit * piDigits$q)/norm,piDigits$degs,show.grid=FALSE,line.col=piColors[piDigits$digit + 1],lwd=.5,do.first=plot_bg(col="black"))
text(0,0, expression(pi),cex=8,adj=.5,col='black')
text(0,0, expression(pi),cex=8,adj=c(.52,.52),col=piColors[1])
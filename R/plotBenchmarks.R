# build function to plot timings
# devtools::install_github("WinVector/WVPlots")
library(ggplot2)
library('WVPlots')

plotTimings <- function(timings) {
  timings$expr <- reorder(timings$expr, -timings$time, FUN=max)
  ggplot(data = timings, aes(x=nRow,y=time, color=expr)) +
    geom_point(alpha=0.8) + geom_smooth(alpha=0.8) 
  
  nmax <- max(timings$nRow)
  tsub <- timings[timings$nRow==nmax,]
  tsub$expr <- reorder(tsub$expr, tsub$time, FUN = median)
  
  list(
    ggplot(data=timings,aes(x=nRow,y=time,color=expr)) +
      geom_point(alpha=0.8) + geom_smooth(alpha=0.8),
    ggplot(data=timings,aes(x=nRow,y=time,color=expr)) +
      geom_point(alpha=0.8) + geom_smooth(alpha=0.8) +
      scale_y_log10(),
    WVPlots::ScatterBoxPlot(tsub,'expr','time',
                            title=paste('max(nRow) = ', nmax)) +
      coord_flip()
  )
}
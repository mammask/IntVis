OverlayedHist <- function(mData       ,
                          featureVar  ,
                          grouper     ,
                          mbinwidth   ,
                          mTitle      ,
                          mxlab       ,
                          mylab       ,
                          mlegendTitle,
                          dynPlot,
                          savePlot,
                          savePath
){
  
  # function name: OverlayedHist
  #       purpose: To produce overlayed histograms grouped by a classification variable
  #       version: 1a
  #          date: 10/02/2017
  #       contact: mammaskon@gmail.com
  #          name: Konstantinos Mammas
  #        inputs:
  #              : mData        = dataset
  #              : featureVar   = vector of feature variables
  #              : grouper      = response variable (classifier)
  #              : mbinwidth    = histogram binwidth
  #              : mTitle       = main title
  #              : mxlab        = xlab names
  #              : mylab        = ylab names
  #              : mlegendTitle = legend title
  #              : dynPlot      = if interactive plot then TRUE
  #              : savePlot     = if save plot then TRUE
  #              : savePath     = path to save plot
  
  library(data.table)
  library(ggplot2)
  library(plotly)
  
  classDat <- copy(mData[,c(featureVar, grouper), with = F])
  setnames(classDat, c(featureVar, grouper), c("Feature", "Response Variable"))
  classDat[, `Response Variable`:= as.factor(`Response Variable`)]
  
  p <- ggplot(classDat, aes(Feature, fill = `Response Variable`)) +
    geom_histogram(alpha = 0.7, position = 'identity', binwidth = mbinwidth) + 
    scale_fill_manual(values=c("#377EB8","#E41A1C")) + 
    ggtitle(mTitle) +
    xlab(mxlab) + ylab(mylab) + 
    guides(fill=guide_legend(title=mlegendTitle)) + theme(plot.title = element_text(size=10))
  
  
  if (savePlot == T) { ggsave(paste0(savePath,"/Histogram_of_",featureVar,".PNG"), dpi = 300) }
  
  if(dynPlot == T) { p <- ggplotly(p) }
  
  return(p)
  
}
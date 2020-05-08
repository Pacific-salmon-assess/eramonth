#=============================================
#Set of functions to produce plots for 


#' plotcyerraster
#'
#' Plot output from calcMonthly function. See details there.
#' @param monthly.df.stock data frame output from calcmoncyer, filtered by stock
#' 
#' @export
#' @examples
#'
plotcyerraster <- function(monthly.df.stock){

  monthly.df.stock$Year <- factor(monthly.df.stock$Recovery_Year)
  monthly.df.stock$Month <- factor(monthly.df.stock$Recovery_Month)

  p <- ggplot(monthly.df.stock) +
       geom_raster(aes(x=Month,y=Year,fill= moncyer),stat = "identity") +
       facet_wrap(~Fishery_Group )+
       theme_bw(10) + ylab("CYER") + 
       scale_y_discrete(limits = rev(levels(monthly.df.stock$Year)), 
        breaks = rev(levels(monthly.df.stock$Year))[seq(1,length(levels(monthly.df.stock$Year)),by=2)]) +
       scale_fill_viridis_c(option="A",direction = -1, end=.9) +
       ggtitle(paste(unique(monthly.df.stock$Stock_Name),"-", unique(monthly.df.stock$Stock)))
    
    return(p)
}
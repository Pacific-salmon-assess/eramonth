#' plotMonthly
#'
#' Plot output from calcMonthly function. See details there.
#' @param monthly.df data frame output from calcMonthly
#' @param stk stock to plot
#' @param fgroup fishery grouping to plot (depends on calcMonthly inputs!)
#' @param type plot type. For now, one of "basic" or "box"
#' @export
#' @examples
#'



plotMonthly <- function(monthly.df,stk="ATN",fgroup = "ISBM_CBC_Term_N",type="basic"){
  
  df.sub <- monthly.df %>% dplyr::filter(Stock == stk, Fishery_Group == fgroup)
  
  if(type == "basic"){
    
    plot(1:5,1:5,type="n", bty="n",xlab="Month",ylab="Prop. of CWT Recoveries (Not ER!)",xlim=c(0,13),
         ylim=c(0,1))
    
    yrs.vec <- sort(unique(df.sub$Recovery_Year))   
    for(i in 1:length(yrs.vec)){
      #print(yr)
      df.yr <-  df.sub %>% dplyr::filter(Recovery_Year == yrs.vec[i])   
      #print(df.yr)
      lines(df.yr$Recovery_Month + i * 0.05,df.yr$monprop,type="h",col="lightblue")  
      
    }} # end basic plot  
  
  
  if(type == "box"){
    par(bty="n")
    boxplot(monprop ~ Recovery_Month,data= df.sub, ylim=c(0,1), border="darkblue",col="lightblue",
            xlab="Month", ylab = "Prop. of CWT Recoveries (Not ER!)")
    
    
  }  # end box plot
  
  
  title(main= paste(stk,fgroup,sep= " / "),cex.main=0.85)
  
  
  
  
  sub.txt <- paste(
    paste(length(unique(df.sub$Recovery_Year)),"Years"),
    paste0(min(df.sub$Recovery_Year), "-", max(df.sub$Recovery_Year)  ),
    paste0("n=",round(min(df.sub$yeartotal)), "-", round(max(df.sub$yeartotal))  ),
    sep=" / ")
  
  title(main=sub.txt,cex.main=0.7, line = 1)
  
  
  # add in ggplot and plotly versions
  
  
  
}

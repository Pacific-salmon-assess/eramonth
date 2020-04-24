#' calcMonthly
#'
#' Calculate monthly split of annual mortality rate
#' @param stk = text string with stock name (e.g. "ATN")
#' @param fmap = data frame with fisheries mapping
#' @param pathdb = text string with path to the CAS database (need either this or the cas input below)
#' @param cas = data frame with CAS data, generated from databases (need either this or the pathdb input above)
#' @param pathcmz = text string with path to CMZ file
#' @param tracing = if TRUE, print various summaries  
#' @export
#' @examples
#' 


calcMonthly <- function(stk = "ATN", fmap = NULL,pathdb = NULL,cas = NULL, pathcmz = NULL,
                        tracing = FALSE){


  
# include various quality checks for the inputs here 
if(is.null(fmap)){warning("Need a fisheries mapping input!"); stop()}
if(is.null(pathdb) & is.null(cas)){warning("Need either a path to the CAS DB, or a dataframe input"); stop()} 
if(!is.null(pathdb) & !is.null(cas)){warning("Cannot have both a path to the CAS DB and a dataframe input. Pick one!"); stop()}
if(is.null(pathcmz)){warning("Need a CMZ input!"); stop()}   
   
if(!is.null(pathdb)){casdf <- readcasdbsplit(pathdb)}
if(!is.null(cas)){casdf <- cas}
  
cmz <- readcmzsplit(pathcmz)
casstk <- unique(casdf$Stock)  
casdf$nameCMZ <- fmap$nameCMZ[match(casdf$CFileFishery_Id, fmap$CFILENO)]

dbstk <- casdf[casdf$Stock==stk,]

monthlycwt <- aggregate(dbstk$Total_Expanded, 
                        list(Stock=dbstk$Stock, Stock_Name=dbstk$Stock_Name, 
                                        Recovery_Year=dbstk$Recovery_Year,  
                                        Recovery_Month=dbstk$Recovery_Month), sum) %>%
                        rename(monthtotal  = x)
yearlycwt <- aggregate(dbstk$Total_Expanded, list(Stock=dbstk$Stock,Stock_Name= dbstk$Stock_Name,  Recovery_Year=dbstk$Recovery_Year), sum)

monthlycwt$yeartotal <- yearlycwt$x[match(monthlycwt$Recovery_Year,yearlycwt$Recovery_Year )]

#these proportions need to be multiplied by the annual mortality. 
print(monthlycwt$monthtotal/monthlycwt$yeartotal)
monthlycwt$monprop <- monthlycwt$monthtotal/monthlycwt$yeartotal 

return(monthlycwt)
  
if(tracing){
# need labels etc here
print("casdf ------------------------")
summary(casdf)
summary(cmz)
summary(fmap)
print("Fisheries --------------------")
unique(casdf$CFileFishery_Name)
print("--------------------")
match(fmap$nameCMZ,names(cmz[,4:ncol(cmz)]))
summary(dbstk)
}

}

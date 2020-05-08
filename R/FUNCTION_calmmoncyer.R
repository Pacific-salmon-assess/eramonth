#' calmmoncyer
#'
#' Read in  and clean up the CMZ file, a product of the mortality distribution tables program
#' @param monthly.idi  data frame otput from the calcMonthly  function
#' @param cmzpath full path to the cmz file , including file name
#' @export
#' @examples
#' 
calcmoncyer <- function(monthly.idi, cmzpath){

	#casdf<-df
	
    cmz <- read.csv(cmzpath,stringsAsFactors =FALSE)

    a <- reshape(cmz, direction = "long", varying=list(4:ncol(cmz)),timevar = "fisheries", v.names = "value")
    a$fisheries_name <- colnames(cmz)[4:ncol(cmz)][a$fisheries]

    a <- a[a$CatchYear>2008,]
    a <- a[a$MortType == "TM",]

    aa<- dplyr::left_join(monthly.idi, a, by=c("Stock" = "stock",
                                           "Recovery_Year" = "CatchYear", 
	                                      "Fishery_Group" = "fisheries_name"))

    aa$moncyer <- aa$monprop * aa$value 

    return(aa)
 
}











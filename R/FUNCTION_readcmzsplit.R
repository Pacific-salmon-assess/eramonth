#' readcmzsplit
#'
#' Read in  and clean up the CMZ file, a product of the mortality distribution tables program
#' @param pathcmz path to the cmzfile .   
#' @export
#' @examples
#' 
readcmzsplit <- function(pathcmz){

	
    cmz <-  read.csv(pathcmz)
    cmz <- cmz[,colSums(is.na(cmz))<nrow(cmz)]

    #rowSums(cmz[,!(names(cmz) %in% c("stock","MortType","CatchYear"))],na.rm =TRUE) 


     return(cmz)

 
}




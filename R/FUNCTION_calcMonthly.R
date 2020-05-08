#' calcMonthly
#'
#' Calculate monthly distribution of tag recoveries for custom fishery groupings (loops through all stocks in the CAS input file).
#' @param fmap = data frame that matches 195 CAS fishery ids (CFILENO) in col 1 to a lookup grouping in col 2. Lookup groupings are used as output labels.
#' @param pathdb = text string with path to the CAS database (need either this or the cas input below)
#' @param cas = data frame with CAS data, generated from database (need either this or the pathdb input above)
#' @param tracing = if TRUE, print various summaries  
#' @param infill = if TRUE, include months with 0 recoveries
#' @export
#' @examples
#'


calcMonthly <- function(fmap = NULL, 
                        pathdb = NULL,cas = NULL,
                        tracing = FALSE,
                        infill = TRUE){


  
# include various quality checks for the inputs here 
if(is.null(fmap)){warning("Need a fisheries mapping input!"); stop()}
if(is.null(pathdb) & is.null(cas)){warning("Need either a path to the CAS DB, or a dataframe input"); stop()} 
if(!is.null(pathdb) & !is.null(cas)){warning("Cannot have both a path to the CAS DB and a dataframe input. Pick one!"); stop()}
#if(is.null(pathcmz)){warning("Need a CMZ input!"); stop()}   
  
if(!is.null(pathdb)){casdf <- readcasdbsplit(pathdb)}
if(!is.null(cas)){casdf <- cas}
 
#if(!(stk %in% unique(casdf$Stock))){warning("Selected 'stk' is not in the CAS data."); stop()}     
   
#cmz <- readcmzsplit(pathcmz)
#casdf$nameCMZ <- fmap$nameCMZ[match(casdf$CFileFishery_Id, fmap$CFILENO)]

monthlycwt.out <- data.frame(Stock = NA,
                               Stock_Name = NA,
                               Fishery_Group = NA,
                               Recovery_Year = NA,
                               Recovery_Month = NA,
                               monthtotal = NA,
                               yeartotal = NA,
                               monprop = NA)
  
  
  
stk.list <- sort(unique(casdf$Stock))
print(stk.list)

fgroups.list <- sort(unique(fmap[,2]))  
print(fgroups.list )
  
for(stk in stk.list){
  
  print("#######################################")
  print(stk)
  print("#######################################")

  for(fgroup in fgroups.list){

    print("---")
    print(fgroup)

    fgroup.ids <- fmap$CFILENO[fmap[,2] == fgroup]
    #print(fgroup.ids)
    
    db.sub  <- casdf %>% dplyr::filter(Stock==stk, CFileFishery_Id %in% fgroup.ids)
    
    stk.name <- unique(db.sub$Stock_Name)
    #print(dim(db.sub))

    if(dim(db.sub)[1]>0){
      
      monthlycwt <- aggregate(db.sub$Total_Expanded, 
                              list( Stock=db.sub$Stock, Stock_Name=db.sub$Stock_Name, 
                                    Fishery_Group = rep(fgroup,dim(db.sub)[1]), 
                                              Recovery_Year=db.sub$Recovery_Year,  
                                              Recovery_Month=db.sub$Recovery_Month), sum) %>%
                              rename(monthtotal  = x)
      yearlycwt <- aggregate(db.sub$Total_Expanded, list(Stock=db.sub$Stock,Stock_Name= db.sub$Stock_Name,  Recovery_Year=db.sub$Recovery_Year), sum)
      
      monthlycwt$yeartotal <- yearlycwt$x[match(monthlycwt$Recovery_Year,yearlycwt$Recovery_Year )]
      
      # monthly proportions
      monthlycwt$monprop <- monthlycwt$monthtotal/monthlycwt$yeartotal 
      
      # fill in missing month
      # could probably do this whole loop more elegantly with spread/gather or something.
      if(infill == TRUE){
        for(yr in sort(unique(monthlycwt$Recovery_Year))){
        
          missing.months  <-  setdiff(1:12,monthlycwt$Recovery_Month[monthlycwt$Recovery_Year == yr])
          year.total <- unique(monthlycwt$yeartotal[monthlycwt$Recovery_Year == yr])
          
          if(length(missing.months) > 0 ){
            add.rows <- data.frame(Stock = stk, Stock_Name = stk.name, Fishery_Group = fgroup,
                                   Recovery_Year = yr,Recovery_Month = missing.months ,
                                   monthtotal = 0 , yeartotal = year.total,
            					            monprop = 0)
            				   
            					   
            monthlycwt <- rbind(monthlycwt,add.rows)
          } # end if missing months
  
        } # end adding missing month
      } # end if infill = TRUE

      monthlycwt.out <-  rbind(monthlycwt.out,monthlycwt)

      #print("a------------")
      #print(add.rows)
      #print("m------------")
      #print(monthlycwt)
      #print("o------------")
      #print(monthlycwt.out)

    } # end if dim[1] > 0

  } # end looping through fisheries
} # end looping through stocks

# drop blank row
monthlycwt.out <- monthlycwt.out[-1,]
  
# sort the output by Fishery/Year/Month
monthlycwt.out <- monthlycwt.out %>% arrange(Stock,Fishery_Group, Recovery_Year,Recovery_Month)

return(monthlycwt.out)


  
if(tracing){
# need labels etc here
print("casdf ------------------------")
summary(casdf)
#summary(cmz)
summary(fmap)
print("Fisheries --------------------")
unique(casdf$CFileFishery_Name)
print("--------------------")
#match(fmap$nameCMZ,names(cmz[,4:ncol(cmz)]))
summary(dbstk)
}

}

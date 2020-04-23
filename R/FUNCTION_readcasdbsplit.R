#' readcasdbsplit
#'
#' Read in number of CWT by fishery an year based on query provided by Gayle
#' @param pathdb path to the cas database from which CWT recoveries are to be extracted.   
#' @importFrom RODBC odbcConnectAccess2007  sqlQuery
#' @export
#' @examples
#' 
readcasdbsplit <- function(pathdb){

	

    
     chnl <- odbcConnectAccess2007(pathdb)

     mySQL <- paste0("SELECT SpeciesStock.Stock, SpeciesStock.Description AS Stock_Name, Year(CWDBRecovery.RecoveryDate) AS Recovery_Year,",
     	" Month(CWDBRecovery.RecoveryDate) AS Recovery_Month, CFileFishery.Id AS CFileFishery_Id, CFileFishery.Description AS CFileFishery_Name,",
     	" CWDBRecovery.Age, Switch(Left(CWTMark1,1)='5','Adclipped',Left(CWTMark1,1)='0','Unclipped') AS Release_Status, Count(CWDBRecovery.RecoveryId)",
     	" AS ObservedTotal, Sum(CWDBRecovery.AdjustedEstimatedNumber) AS SumOfAdjustedEstimatedNumber,",
     	" Sum(CWDBRecovery.AdjustedEstimatedNumber*((IIf(IsNull(CWTMark1Count),0,CWTMark1Count)+IIf(IsNull(CWTMark2Count),0,CWTMark2Count)+IIf(IsNull(NonCWTMark1Count),0,NonCWTMark1Count)+",
     	" IIf(IsNull(NonCWTMark2Count),0,NonCWTMark2Count))/(IIf(IsNull(CWTMark1Count),0,CWTMark1Count)+IIf(IsNull(CWTMark2Count),0,CWTMark2Count)))) AS Total_Expanded FROM (Fishery INNER JOIN",
     	" ((CWDBRecovery INNER JOIN WireTagCode ON CWDBRecovery.TagCode = WireTagCode.TagCode) INNER JOIN SpeciesStock ON WireTagCode.Stock = SpeciesStock.Stock)",
     	" ON Fishery.Id = CWDBRecovery.Fishery) INNER JOIN (FisheryCFileFishery INNER JOIN CFileFishery ON FisheryCFileFishery.CFileFishery = CFileFishery.Id)",
     	" ON Fishery.Id = FisheryCFileFishery.Fishery WHERE (((Year([CWDBRecovery].[RecoveryDate]))>=2009)) GROUP BY SpeciesStock.Stock,",
     	" SpeciesStock.Description, Year(CWDBRecovery.RecoveryDate), Month(CWDBRecovery.RecoveryDate), CFileFishery.Id, CFileFishery.Description,",
     	" CWDBRecovery.Age, Switch(Left(CWTMark1,1)='5','Adclipped',Left(CWTMark1,1)='0','Unclipped') ORDER BY SpeciesStock.Stock,",
     	" SpeciesStock.Description, Year(CWDBRecovery.RecoveryDate), Month(CWDBRecovery.RecoveryDate), CFileFishery.Id, CFileFishery.Description,",
     	" CWDBRecovery.Age, Switch(Left(CWTMark1,1)='5','Adclipped',Left(CWTMark1,1)='0','Unclipped');")



     df <- RODBC::sqlQuery( chnl , query = mySQL ) 

     return(df)

   

}







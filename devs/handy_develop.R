#=========================================================================
#handy scripts to be ran during development 
#do not expect any organization or documentation in this file
#=========================================================================




#put this file in Rbuild ignore


#RODBC::odbcCloseAll()
#devtools::load_all()


devtools::document()

#equivalent to ctrl + b in Rstudio 
#devtools::build(binary= TRUE)
devtools::build()

#this loads all the files in R

detach("package:deratools", unload=TRUE)
system("Rcmd.exe INSTALL --preclean --no-multiarch --with-keep.source C:/Users/worc/Documents/CTC/deratools_0.0.0.9000.tar.gz")
#install.packages( "C:/Users/worc/Documents/CTC/ream_0.0.0.9000.tar.gz")

library(deratools)


fmap<-read.csv("fisheriesmap.csv")
#devtools::use_data(x, mtcars)


pathdb <- "C:/Users/worc/Documents/CTC/ream/devs/testtxt/CASClient_2019_BE.mdb"
casdf <- readcasdbsplit(pathdb)

pathcmz <- "C:/Users/worc/Documents/CTC/ERA/2019/mortality_distribution_tables/2019ERA Mortality Distrib Tables V1_5age/catchDistribution_CMZ.csv"
cmz <- readcmzsplit(pathcmz)

summary(casdf)
summary(cmz)
summary(fmap)

casstk <- unique(casdf$Stock)  

	unique(casdf$CFileFishery_Name)

match(fmap$nameCMZ,names(cmz[,4:ncol(cmz)]))


casdf$nameCMZ <- fmap$nameCMZ[match(casdf$CFileFishery_Id, fmap$CFILENO)]

dbstk <- casdf[casdf$Stock=="ATN",]
summary(dbstk)

monthlycwt <- aggregate(dbstk$Total_Expanded, list(Stock=dbstk$Stock, Stock_Name=dbstk$Stock_Name, Recovery_Year=dbstk$Recovery_Year,  Recovery_Month=dbstk$Recovery_Month), sum)
yearlycwt <- aggregate(dbstk$Total_Expanded, list(Stock=dbstk$Stock,Stock_Name= dbstk$Stock_Name,  Recovery_Year=dbstk$Recovery_Year), sum)

monthlycwt$yeartotal <- yearlycwt$x[match(monthlycwt$Recovery_Year,yearlycwt$Recovery_Year )]

monthlycwt$monprop <- monthlycwt$x/monthlycwt$yeartotal 


#=============================================
#Set of functions to write Rmd snippets
#Author: Catarina Wor
#Date: May 2020
#=============================================









#' Write headers for Rmd dashboard
#' 
#' @param title Title of flexdashboard
#' 
#' @return The Rmd header 
#'
#' @export
#' 
write_headr <- function(title) {
	

	headr<-c(paste0("--- \n",
			"title: ", title, "\n",
			"output:\n",
			"  flexdashboard::flex_dashboard:\n",
            "    orientation: columns\n",
            "    vertical_layout: scroll\n",
			"  theme: flatly\n" ,
			"  always_allow_html: yes\n",
			"--- \n "))


	return(headr)

}





#' Write R setup for Rmd dashboard
#' 
#' @param packs vector containing the list of packages to be written in dashboard
#' 
#' @return The Rmd header 
#'
#' @export
#' 
write_setup <- function(packs) {
	
	headr<- c(
          "```{r setup, echo=FALSE}\n",
          paste0(sapply(packs,function(x)paste0("library(",x,")\n"))),
          "```\n")

	return(headr)

}




#' Write R setup for Rmd dashboard
#' 
#' @param code Additional code one may want to include in the dashboard
#' 
#' @return The Rmd header 
#'
#' @export
#' 
write_rmdcode <- function(code) {
	
	headr <- paste0(
          "```{r prep, echo=FALSE}\n ",
          code,
          "```\n ")

	return(headr)

}



#' Write R setup for Rmd dashboard
#' 
#' @param monthly.df.stock.name name of data frame output from calcmoncyer, filtered by stock
#' @param obj_name stock acronym
#' 
#' @return The Rmd header 
#'
#' @export
#' 
write_raster_chunks <- function(monthly.df.name,obj_name) {

	
    if(!is.na(obj_name)){
	   rmd<-c(
	      paste0("### ", obj_name, "\n"),
	      paste0(
      	  "```{r }\n",
          "if(obj_name == 'RBT'){\n",
          	monthly.df.name," <- ",monthly.df.name,"[",monthly.df.name,"$Fishery_Group != 'TERM_CA_WCVI_S' &",
                               monthly.df.name,"$Fishery_Group != 'TERM_CA_WCVI_S' &",
                               monthly.df.name,"$Fishery_Group != 'TERM_CA_WCVI_FS',]\n",
          "}else if(obj_name == 'QUI'){\n",
          	   monthly.df.name," <- ",monthly.df.name,"[",monthly.df.name,"$Fishery_Group != 'ISBM_SBC_JNST_TERM_S' &",
               monthly.df.name,"$Fishery_Group != 'TERM_CA_GEO_ST_FS',]
          }\n",
      	  "monthly.df.stock.name <- ",monthly.df.name,"[",monthly.df.name,"$Stock=='",obj_name,"',]\n",
      	  "ggplotly(plotcyerraster(monthly.df.stock.name)",
		")%>%layout(
      margin = list(b = 50, l = 150,r = 150))\n```\n")      	
	)

	 return(rmd)
  }else{
    return(NULL)
  }


}




#' write_dash_raster
#' 
#' @param monthly.df.stock.name name of data frame output from calcmoncyer, filtered by stock
#' @param obj_name stock acronym
#' 
#' @return The Rmd header 
#'
#' @export
#' 
write_dash_raster <- function(fn, title,packs, code) {


    headr <- write_headr(title)
    headr2 <- write_setup(packs)

    #code <- paste0("da <- read.csv(",moncyerdir,"stringsAsFactors=FALSE)\n")
    headr3 <- write_rmdcode(code)


    chk1<-NULL

    stk <- unique(da$Stock)
    
    for(i in stk){

    	if(sum(da$Stock==i)>0){
        
            chk <- write_raster_chunks("da",i)

            chk1<- c(chk1, "\n ",chk)
        }
    }
    
    dash<-c(headr, "\n",
      headr2, "\n",
      headr3, "\n","\n",
      "Column {.tabset}",
      "-------------------------------------\n",
      chk1)
    

    conn <- file(fn)
    write(dash, conn)
    close(conn)

}
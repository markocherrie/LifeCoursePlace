#' Generate lifecourse configuration file
#'
#' This function generates greenspace 'exposure' for participants.
#'
#' @param exposuredatafolder
#' @param outcomedatafolder
#' @return A dataframe of exposures and times
#' @export
#'
#'
#'


gen_lifecourseconfig<-function(datafolder, outcomedatafolder){
  listoffiles<-list.files(exposuredatafolder)
  exposure<-gsub("_.*$", "", listoffiles)
  year<-readr::parse_number(listoffiles)
  config<-data.frame(exposure, year)

  listoffiles<-list.files(outcomedatafolder)
  birth<-readr::parse_number(listoffiles)

  config$age<-config$year-birth
  config<-config[(config$age>0),]

}

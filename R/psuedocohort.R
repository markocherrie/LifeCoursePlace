#' Generate Psuedo Longitudinal Cohort
#'
#' This function generates random Open Street Map addresses for a given bounding box
#'
#'
#' @param longmin minimum longitude of bounding box
#' @param longmax maximum longitude of bounding box
#' @param latmin minimum latitude of bounding box
#' @param latmax maximum longitude of bounding box
#' @param numberofaddresses How many addresses to try and sample (<1000)
#' @return A dataframe of addresses in bounding box
#' @export
#'

pseudocohort<-function(longmin, longmax, latmin, latmax, numberofaddresses){
sample<-data.frame(cbind(runif(1000,longmin,longmax),runif(1000,latmin,latmax)))
colnames(sample)<-c("long", "lat")
sample_addresses <- photon::reverse(x = sample$long[1:numberofaddresses], y = sample$lat[1:numberofaddresses])
sample_addresses<-sample_addresses[sample_addresses$osm_value=="house",]
sample_addresses<-sample_addresses[!is.na(sample_addresses$x),]
sample_addresses
}



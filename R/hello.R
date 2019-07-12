# Generate Psuedo Longitudinal Cohort
#
# This is an function takes a bounding box and randomises address allocation
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

pseudocohort<-function(longmin, longmax, latmin, latmax, numberinsample){
sample<-data.frame(cbind(runif(1000,-3.316587,-3.071410),runif(1000,55.891648,55.991446)))
colnames(sample)<-c("long", "lat")
sample_addresses <- photon::reverse(x = sample$long[1:100], y = sample$lat[1:100])
sample_addresses<-sample_addresses[sample_addresses$osm_value=="house",]
sample_addresses<-sample_addresses[!is.na(sample_addresses$x),]
}


#' Generate greenspace buffers
#'
#' This function generates greenspace 'exposure' for participants.
#'
#' @param lat
#' @param lon
#' @param id
#' @param year
#' @param buffersize
#' @return A dataframe of addresses with buffered exposure
#' @export
#'
#'
#'

greenspace_life= function(lat, lon, id, year, buffersize){

  # make the points a spatial object
  coords = cbind(lon, lat)
  sp = SpatialPoints(coords)
  proj4string(sp) = CRS('+proj=longlat +datum=WGS84')
  residential_loc = spTransform(sp, CRS('+proj=tmerc +lat_0=49
                                      +lon_0=-2 +k=0.9996012717 +x_0=400000
                                      +y_0=-100000 +ellps=airy +datum=OSGB36
                                      +units=m +no_defs'))

  # buffers (set at 500m, 1000m and 1500m)
  homebuffer<-gBuffer(residential_loc, width = buffersize)

  # read the parks from within the function

  # buffer 1- Subtract the home buffer away from the park area
  #    Then, if this is intersection is more than 0, code as 1
  #    Next, find the area of the buffer and generate a percentage of how much of the buffer is in green space
  # Split the dates of the residence into the three time periods we are interested in




  if (DATE>=1936 & DATE<=1958) {
    Homeintersection<-gIntersection(HomeSPDF_1, parks_1949, byid=T)
  } else if (DATE>=1959 & DATE<=1978) {
    Homeintersection<-gIntersection(HomeSPDF_1, parks_1969, byid=T)
  } else
    Homeintersection<-gIntersection(HomeSPDF_1, parks_2009, byid=T)
  Homearea<-ifelse(is.null(Homeintersection), 0, gArea(Homeintersection))
  Homearea2<-gArea(HomeSPDF_1)
  home_green_perc<-ifelse(Homearea==0, 0, (Homearea/Homearea2)*100)

  # Size of intersection
  intersection<-Homearea

  # buffer 2- This is a repeat of the above at the larger buffer size
  if (DATE>=1936 & DATE<=1958) {
    Homeintersection<-gIntersection(HomeSPDF_2, parks_1949, byid=T)
  } else if (DATE>=1959 & DATE<=1978) {
    Homeintersection<-gIntersection(HomeSPDF_2, parks_1969, byid=T)
  } else
    Homeintersection<-gIntersection(HomeSPDF_2, parks_2009, byid=T)
  Homearea<-ifelse(is.null(Homeintersection), 0, gArea(Homeintersection))
  Homearea2<-gArea(HomeSPDF_2)
  home_green_perc2<-ifelse(Homearea==0, 0, (Homearea/Homearea2)*100)

  # Size of intersection
  intersection_2<-Homearea

  # buffer 3
  if (DATE>=1936 & DATE<=1958) {
    Homeintersection<-gIntersection(HomeSPDF_3, parks_1949, byid=T)
  } else if (DATE>=1959 & DATE<=1978) {
    Homeintersection<-gIntersection(HomeSPDF_3, parks_1969, byid=T)
  } else
    Homeintersection<-gIntersection(HomeSPDF_3, parks_2009, byid=T)
  Homearea<-ifelse(is.null(Homeintersection), 0, gArea(Homeintersection))
  Homearea2<-gArea(HomeSPDF_3)
  home_green_perc3<-ifelse(Homearea==0, 0, (Homearea/Homearea2)*100)

  # Size of intersection
  intersection_3<-Homearea

  # size of parks combined (mean)
  if (DATE>=1936 & DATE<=1958) {
    green_space_size<-parks_1949$Shape_Area[which.min(gDistance(residential_loc, parks_1949, byid=TRUE))]
  } else if (DATE>=1959 & DATE<=1978) {
    green_space_size<-parks_1969$Shape_Area[which.min(gDistance(residential_loc, parks_1969, byid=TRUE))]
  } else
    green_space_size<-parks_2009$Shape_Area[which.min(gDistance(residential_loc, parks_2009, byid=TRUE))]

  # distance to nearest park
  if (DATE>=1936 & DATE<=1958) {
    distance<-gDistance(residential_loc, parks_1949)
  } else if (DATE>=1959 & DATE<=1978) {
    distance<-gDistance(residential_loc, parks_1969)
  } else
    distance<-gDistance(residential_loc, parks_2009)

  # Data output and index creation
  greenspaceexposure <-data.frame(LBC_CODE, DATE, home_green_perc, intersection, home_green_perc2, intersection_2, home_green_perc3, intersection_3, green_space_size, distance)
  return(greenspaceexposure)
}

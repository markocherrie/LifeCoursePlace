#' Generate longitudinal pseudo cohort
#'
#' This function generates a longitudinal cohort of participants
#' with random Open Street Map addresses movement
#'
#'
#'
#' @param birthyear year of birth for cohort
#' @param longmin minimum longitude of bounding box
#' @param longmax maximum longitude of bounding box
#' @param latmin minimum latitude of bounding box
#' @param latmax maximum longitude of bounding box
#' @return A cohort of participants with longitudinal address history
#' @export
#'

gen_psuedo_cohort<-function(birthyear, longmin, longmax, latmin, latmax){
PC01<-pseudocohort(longmin,longmax, latmin,latmax, 200)[1:200,]
PC01$Year<-birthyear
PC01$id<-paste0("PC_", seq(1:200))
PC110<-pseudocohort(longmin,longmax, latmin,latmax, 100)[1:100,]
PC110$Year<-sample(seq(birthyear+1,birthyear+10), replace=T, 100)
PC110$id<-paste0("PC_", seq(1:100))
PC1120<-pseudocohort(longmin,longmax, latmin,latmax, 200)[1:200,]
PC1120$Year<-sample(seq(birthyear+11,birthyear+20), replace=T, 200)
PC1120$id<-paste0("PC_", seq(1:200))
PC2130<-pseudocohort(longmin,longmax, latmin,latmax, 200)[1:200,]
PC2130$Year<-sample(seq(birthyear+21,birthyear+30), replace=T, 200)
PC2130$id<-paste0("PC_", seq(1:200))
PC3140<-pseudocohort(longmin,longmax, latmin,latmax, 100)[1:100,]
PC3140$Year<-sample(seq(birthyear+31,birthyear+40), replace=T, 100)
PC3140$id<-paste0("PC_", seq(1:100))
PC4150<-pseudocohort(longmin,longmax, latmin,latmax, 100)[1:100,]
PC4150$Year<-sample(seq(birthyear+41,birthyear+50), replace=T, 100)
PC4150$id<-paste0("PC_", seq(1:100))
PC5160<-pseudocohort(longmin,longmax, latmin,latmax, 50)[1:50,]
PC5160$Year<-sample(seq(birthyear+51,birthyear+60), replace=T, 50)
PC5160$id<-paste0("PC_", seq(1:50))
PC6170<-pseudocohort(longmin,longmax, latmin,latmax, 50)[1:50,]
PC6170$Year<-sample(seq(birthyear+61,birthyear+70), replace=T, 50)
PC6170$id<-paste0("PC_", seq(1:50))
PC7180<-pseudocohort(longmin,longmax, latmin,latmax, 25)[1:25,]
PC7180$Year<-sample(seq(birthyear+71,birthyear+80), replace=T, 25)
PC7180$id<-paste0("PC_", seq(1:25))
PC8190<-pseudocohort(longmin,longmax, latmin,latmax, 25)[1:25,]
PC8190$Year<-sample(seq(birthyear+81,birthyear+90), replace=T, 25)
PC8190$id<-paste0("PC_", seq(1:25))

PC<-rbind(PC01, PC110, PC2130, PC3140, PC4150, PC5160, PC6170, PC7180, PC8190)

PC
}


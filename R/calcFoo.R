calcFoo <- function(){

  bioenergy <- calcOutput("1stBioenergyPast", aggregate = FALSE)
  att <- calcOutput("Attributes", aggregate = FALSE)[,,"ge"]
  x <- bioenergy[,seq(1965,2020,5) ,   c("ethanol", "oils")][, , "INDPROD", drop = TRUE]

}

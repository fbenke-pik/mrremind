#' fullREMIND
#'
#' Function that produces the complete regional data set required for the REMIND model.
#'
#' @author Lavinia Baumstark
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}}, \code{\link[madrat]{calcOutput}}
#' @export
#' @examples
#' \dontrun{
#' fullREMIND()
#' }
#'
fullREMIND <- function() {
  gdpPopScen <- c("SSPs", "SSP2IndiaDEAs")
  feDemScen <- c(gdpPopScen, "SSP2_lowEn", "SSP2_highDemDEU", "SSP2_NAV_all")


  calcOutput("FeDemandIndustry", scenarios = feDemScen, signif = 4, file = "f_fedemandInd.cs4r")
  calcOutput("FeDemandBuildings", subtype = "FE", scenario = feDemScen, round = 8, file = "f_fedemandBuild.cs4r")

  calcOutput("IoRemind", subtype = "output", round = 8, file = "f04_IO_output.cs4r")
  calcOutput("IoRemind", subtype = "input", round = 8, file = "f04_IO_input.cs4r")
  calcOutput("IoRemind", subtype = "output", additionalCorrections = FALSE, round = 8, file = "f04_IO_output_raw.cs4r")
  calcOutput("IoRemind", subtype = "input", additionalCorrections = FALSE, round = 8, file = "f04_IO_input_raw.cs4r")
}

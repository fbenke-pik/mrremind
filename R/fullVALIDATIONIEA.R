fullVALIDATIONIEA <- function() {

  # general ----

  calcOutput("IeaEnergyBalances", ieaVesion = "default", file = "IeaEnergyBalances.cs4r")

  # Industry ----

  gdpPopScen <- c("SSPs", "SSP2IndiaDEAs")
  feDemScen <- c(gdpPopScen, "SSP2_lowEn", "SSP2_highDemDEU", "SSP2_NAV_all")

  calcOutput("EnergyBalancesOutputToIndustry", file = "EnergyBalancesOutputToIndustry.cs4r")

  calcOutput("FeDemandIndustry", scenarios = feDemScen, signif = 4, last_empirical_year = 2022,
             file = "f_fedemandInd_2022.cs4r")

  calcOutput("FeDemandIndustry",
             scenarios = "SSP2",
             signif = 4,
             aggregate = FALSE,
             file = "f_fedemandInd_unaggregated.cs4r")


  # Historical ----

  calcOutput(type = "PE", ieaVersion = "latest", file = "PE.cs4r")
  calcOutput(type = "FE", ieaVersion = "latest", file = "FE.cs4r")

  # Buildings ----

  calcOutput(type = "IOEdgeBuildings", ieaVersion = "default",
             subtype = "output_EDGE", file = "IOEdgeBuildings_output_EDGE.cs4r")
  calcOutput(type = "IOEdgeBuildings", ieaVersion = "default",
             subtype = "output_EDGE_buildings", file = "IOEdgeBuildings_output_EDGE_buildings.cs4r")

  # Transport ----
  calcOutput(type = "IEAOutputTransport", file = "IEAOutputTransport.cs4r")

  }

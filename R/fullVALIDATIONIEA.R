fullVALIDATIONIEA <- function() {

  # general ----

  calcOutput("IeaEnergyBalances", ieaVersion = "default", file = "IeaEnergyBalances.cs4r")

  # Industry ----

  calcOutput("EnergyBalancesOutputToIndustry", file = "EnergyBalancesOutputToIndustry.cs4r")

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

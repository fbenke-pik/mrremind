#' Calculate air pollutant emissions for a reference year, for use
#' in combination with GAINS data at different sectoral aggregations
#'
#' @param subtype just "total" is supported
#' @param baseyear year to take as a reference from CEDS, ignored for the EDGAR2005 LUC CO2 emissions
#' @param outunits "Mt/yr" or "kt/yr"
#' @param namesformat "GAINS2025" or "REMIND" or "REMINDexo", the standard to use for pollutant names
#' @return magclass object
#' @author Gabriel Abrahao
#' @importFrom magclass getNames<- getYears<-

calcAirPollEmiRef <- function(
    subtype = "total", baseyear = 2020, outunits = "Mt/yr", namesformat = "GAINS2025") {

  # Mapping from GAINS to CEDS2025 pollutant names
  polnamesmap <- c(
    "CO" = "co",
    "NOX" = "no2_n",
    "PM_BC" = "bc_c",
    "PM_OC" = "oc_c",
    "SO2" = "so2",
    "NH3" = "nh3_n",
    "VOC" = "nmvoc"
  )

  # Reading CEDS2025 emissions. TODO: Check units
  # Subsetting with polnamesmap already ensures the right order
  fullceds <- readSource("CEDS2025")[, baseyear, polnamesmap]
  getItems(fullceds, 3.2) <- names(polnamesmap)
  getSets(fullceds)[getSets(fullceds) == "pollutant"] <- "species"

  # Converting units. GAINS seems to be in kt of each pollutant,
  # CEDS is in Mt, but NOx and NH3 are ostensibly in MtN
  if (outunits == "kt/yr") {
    fullceds[, , ] <- fullceds[, , ] * 1e3
  }
  # Assuming NO2, ktN to ktNO2
  fullceds[, , "NOX"] <- fullceds[, , "NOX"] * (14 + 16 + 16) / 14
  # ktN to ktNH3 (actually makes for a worse fit with GAINS)
  fullceds[, , "NH3"] <- fullceds[, , "NH3"] * (14 + 3) / 14

  fixPolNames <- function(mag, fmt = "REMIND") {
    # Mapping from GAINS2025 to REMIND (oldGAINS) pollutant names
    if (fmt == "REMIND") {
      polnamesmap <- c(
        "CO" = "CO",
        "NOx" = "NOX",
        "BC" = "PM_BC",
        "OC" = "PM_OC",
        "SO2" = "SO2",
        "NH3" = "NH3",
        "VOC" = "VOC"
      )
    } else if (fmt == "REMINDexo") {
      polnamesmap <- c(
        "CO" = "CO",
        "NOx" = "NOX",
        "BC" = "PM_BC",
        "OC" = "PM_OC",
        "SOx" = "SO2",
        "NH3" = "NH3",
        "NMVOC" = "VOC"
      )
    }
    mag <- mag[, , polnamesmap]
    getItems(mag, "species") <- names(polnamesmap)
    return(mag)
  }

  if (namesformat %in% c("REMIND", "REMINDexo")) {
    fullceds <- fixPolNames(fullceds, namesformat)
  }

  totceds <- dimSums(fullceds, dim = 3.1, na.rm = TRUE)

  out <- setYears(totceds)
  unit <- outunits
  desc <- paste0("Emissions in year ", baseyear)

  return(list(
    x = out,
    weight = NULL,
    unit = unit,
    description = desc
  ))
}

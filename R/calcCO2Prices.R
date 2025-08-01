calcCO2Prices <- function() {
  x <- readSource("ExpertGuess", subtype = "co2prices")

  # Convert from $2005 to $2017
  x <- GDPuc::toolConvertGDP(
    gdp = x,
    unit_in = "constant 2005 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  getNames(x) <- NULL

  # Read data used for weight
  ceds <- calcOutput("Emissions", datasource = "CEDS2025", aggregate = FALSE)
  ceds <- ceds[, , "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)"]
  # For years in the future, use last year available from CEDS
  ceds <- ceds[, base::pmin(getYears(x), max(getYears(ceds))), ]
  getYears(ceds) <- getYears(x)

  list(x = x,
       weight = ceds,
       unit = "US$2017/t CO2",
       description = "CO2 prices in 2010, 2015 and 2020")
}

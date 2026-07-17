fullVALIDATIONIEA <- function() {


  calcOutput("EnergyBalancesOutputToIndustry",
             file = "EnergyBalancesOutputToIndustry.cs4r")

  calcOutput(
    type = "Industry_Value_Added",
    scenarios = "SSP2",
    match.steel.historic.values = TRUE,
    match.steel.estimates = "IEA_ETP",
    warnNA = FALSE,
    file = "Industry_Value_Added.cs4r"
  )

  calcOutput(
    type = "Steel_Projections",
    subtype = "production",
    scenarios = "SSP2",
    match.steel.historic.values = TRUE,
    match.steel.estimates = "IEA_ETP",
    supplementary = FALSE,
    file = "Steel_Projections.cs4r"
  )

  region_mapping_21 <- toolGetMapping('regionmapping_21_EU11.csv', 'regional',
                                      where = 'mappingfolder') %>%
    as_tibble() %>%
    select(iso3c = 'CountryCode', region = 'RegionCode')

  calcOutput(
    type = "industry_subsectors_specific", subtype = "FE",
    scenarios = "SSP2",
    regions = unique(region_mapping_21$region),
    file = "industry_subsectors_specific.cs4r",
    aggregate = FALSE
  )

  calcOutput(
    type = "FE", ieaVersion = "latest", file = "FE.cs4r",
  )

  calcOutput("FeDemandIndustry",
             scenarios = "SSP2",
             signif = 4,
             aggregate = FALSE,
             file = "f_fedemandInd_unaggregated.cs4r")

}

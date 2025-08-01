#' Calc Emission Factors
#'
#'

calcEmissionFactors <- function(subtype = "emission_factors", sectoral_resolution = "aggregated") {
  if (!(subtype %in% c("emission_factors", "activities", "emissions"))) {
    stop('subtype must be in c("emission_factors", "activities", "emissions")')
  }

  #-- INITIALISATION ----------------

  vcat(2, ">> Initialization...\n")

  # local functions
  allocate_c2r_ef <- function(id_ef, ip_region, ip_country, ip_year, ip_scenario) {
    dummy <- id_ef[ip_region, ip_year, ip_scenario]
    dummy[, , ] <- setCells(id_ef[ip_country, ip_year, ip_scenario], "GLO")
    return(dummy)
  }

  allocate_min2r_ef <- function(id_ef, ip_region, ip_countryGroup, ip_year, ip_scenario) {
    dummy <- id_ef[ip_region, ip_year, ip_scenario]

    # Get minimum values across country group
    tmp <- quitte::as.quitte(id_ef[ip_countryGroup, ip_year, ip_scenario]) %>%
      group_by(!!!syms(c("data1", "data2"))) %>%
      summarise(value = ifelse(all(.data$value == 0), 0,
        min(.data$value[.data$value > 0], na.rm = TRUE)
      )) %>% # a value 0 is often a sign for a NA that has been replaced with 0 for small countries
      dplyr::ungroup() %>%
      as.data.frame() %>%
      quitte::as.quitte() %>%
      as.magpie()

    # Allocate minimum values to region
    dummy[ip_region, ip_year, ip_scenario] <- setYears(tmp)

    return(dummy)
  }

  # conversion factors
  conv_ktSO2_to_ktS <- 1 / 2 # 32/(32+2*16)
  conv_kt_per_PJ_to_Tg_per_TWa <- 1e-3 / (1e15 / (365 * 24 * 60 * 60) * 1e-12)

  # user-defined parameters
  time <- c(seq(2005, 2055, 5), seq(2060, 2110, 10), 2130, 2150)

  # These are additional scenarios to the CLE and MFR
  scenario <- c(
    "SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "FLE", "MFR", "CLE",
    "MFR_Transports", "GlobalEURO6", "FLE_building_transport",
    "SLCF_building_transport"
  )

  p_countryCategories <- "useGAINSregions"

  # list of OECD countries
  # TODO: may want to place this in a mapping file or in a R library
  r_oecd <- c(
    "AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU",
    "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD",
    "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
  )

  # set of sectors for which no emission factor will be computed
  # (because there is no activity reported, or not in terms of energy)
  dimSector_skipEF <- c(
    "AACID", "CEMENT", "CHEM", "CHEMBULK", "CUSM", "NACID", "PAPER", "STEEL",
    "Losses_Coal", "Losses_Distribution_Use", "Losses_Vent_Flare",
    "Transformations_Coal", "Transformations_HLF", "Transformations_HLF_Refinery",
    "Transformations_LLF", "Transformations_NatGas"
  )

  dimSector_skipEF_edge <- c(
    "End_Use_Industry_Bio_Trad", "End_Use_Industry_Coal", "End_Use_Industry_HLF",
    "End_Use_Industry_LLF", "End_Use_Industry_NatGas", "End_Use_Residential_Bio_Mod",
    "End_Use_Residential_Bio_Trad", "End_Use_Residential_Coal", "End_Use_Residential_HLF",
    "End_Use_Residential_LLF", "End_Use_Residential_NatGas", "End_Use_Services_Bio_Trad",
    "End_Use_Services_Coal"
  )
  dimSector_skipEF_edge <- c("")

  #-- READ IN DATA ------------------
  vcat(2, ">> Read-in data... \n")
  # read in ECLIPSE data
  #  > activity data
  activities <- readSource("ECLIPSE", subtype = paste0("activities.", sectoral_resolution))
  activities <- activities[, c(2005, 2010, 2020, 2030, 2050), ]
  #  > emission data
  emissions <- readSource("ECLIPSE", subtype = paste0("emissions.", sectoral_resolution))
  emissions <- emissions[, c(2005, 2010, 2020, 2030, 2050), ]

  # read in sectoral mapping (ECLIPSE (IMAGE) <> REMIND)
  map_sectors_ECLIPSE2REMIND <- toolGetMapping(
    type = "sectoral", name = "mappingECLIPSEtoREMINDsectors.csv",
    where = "mrremind"
  )  %>%
    # remove no longer used technologies pcc and pco
    filter(!grepl("pcc|pco", .data$remind))

  # read in regional map (select ISO and GAINS codes only). This is required for the construction of the SSPs

  map_regions <-utils::read.csv2(
    toolGetMapping(type = "regional", name = "regionmappingGAINS.csv", returnPathOnly = TRUE, where = "mrremind"),
    stringsAsFactors = TRUE
  )[, c(2, 3)]

  map_regions <- map_regions %>%
    filter(.data$CountryCode != "ANT") %>% # Remove Netherland Antilles (not in REMIND regional mapping)
    filter(.data$RegionCode != "") %>%
    mutate(RegionCode = gsub(
      "\\ \\+", "\\+",
      gsub(
        "^\\s+|\\s+$", "",
        gsub("[0-9]", "", .data$RegionCode)
      )
    )) %>%
    mutate(CountryCode = factor(.data$CountryCode))

  gdp_cap <- calcOutput("GDPpc", scenario = "SSP2", aggregate = FALSE)[, 2005, ]

  #-- PROCESS DATA ------------------
  vcat(2, ">> Process data... \n")
  # set of sectors for which emission factors are computed
  dimSector_EF <- getNames(activities)[!getNames(activities) %in% c(dimSector_skipEF, dimSector_skipEF_edge)]

  # Regional selections
  # select one country pertaining to WEU (all WEU countries should have the same EF). Used for SSP scenario rules
  select_weu <- paste(map_regions[which(map_regions$RegionCode == "Western Europe")[1], 1])

  # Retrieve Transport names
  transportNames <- getNames(activities)[grepl("End_Use_Transport", getNames(activities))]
  buildingNames <- getNames(activities)[grepl("End_Use_Industry|End_Use_Residential|End_Use_Services", getNames(activities))]

  # convert SO2 emission from TgSO2 to TgS
  emissions[, , "SO2"] <- emissions[, , "SO2"] * conv_ktSO2_to_ktS

  # define missing SLE scenario (assumed to be 3/4 of the distance between CLE and MFR,
  # according to discussion with Zig Klimont on 18th Feb 2016)
  cle <- emissions[, , "CLE"]
  getNames(cle) <- gsub("CLE", "MFR", getNames(cle))
  sle <- cle - (cle - emissions[, , "MFR"]) * 0.75
  getNames(sle) <- gsub("MFR", "SLE", getNames(sle))
  emissions <- mbind(emissions, sle)
  rm(cle, sle)

  # calculate emission factors (only for power and end-use sectors, and not empty activities)
  # and convert from kt/PJ to Tg/Twa
  ef_eclipse <- emissions[, , dimSector_EF] /
    activities[, , dimSector_EF] *
    conv_kt_per_PJ_to_Tg_per_TWa
  getSets(ef_eclipse) <- c("region", "year", "data1", "data2", "data3")

  # some regions/countries have NA values everywhere. Allocate EF of the region to which they belong (except for Antarctica)
  NAregions <- c(
    "AIA", "ATF", "BVT", "CCK", "COK", "CXR", "ESH", "FLK", "GIB", "GLP", "GUF",
    "HMD", "IOT", "MSR", "MTQ", "MYT", "NFK", "NIU", "NRU", "PCN",
    "REU", "SGS", "SHN", "SJM", "SPM", "TKL", "TWN", "UMI", "VAT", "VGB", "WLF"
  )
  MissingRegions <- c("ALA", "BES", "BLM", "CUW", "GGY", "IMN", "JEY", "MAF", "PSE", "SSD", "SXM")
  AssociatedGAINSregions <- c(
    "Western Europe", "Rest Central America", "Rest Central America", "Rest Central America",
    "Western Europe", "Western Europe", "Western Europe",
    "Rest Central America", "Middle East", "Northern Africa", "Rest Central America"
  )
  ef_eclipse["ATA", , ] <- 0 # Antarctica -> 0
  for (kregi in NAregions) {
    subsitute_region <- map_regions$CountryCode[map_regions$RegionCode == map_regions$RegionCode[map_regions$CountryCode == kregi] & !map_regions$CountryCode %in% c(NAregions, MissingRegions)][1]
    tmp <- ef_eclipse[subsitute_region, , ]
    getItems(tmp, dim = 1) <- kregi
    ef_eclipse[kregi, , ] <- tmp
  }
  # some regions have no population data when disaggregating.
  for (kregi in MissingRegions) {
    substitute_region <- map_regions$CountryCode[map_regions$RegionCode == AssociatedGAINSregions[which(MissingRegions == kregi)] &
      !map_regions$CountryCode %in% MissingRegions][1]
    tmp <- ef_eclipse[substitute_region, , ]
    getItems(tmp, dim = 1) <- kregi
    ef_eclipse[kregi, , ] <- tmp
  }

  # for the remaining NAs just set EF to 0 (activity levels are 0)
  ef_eclipse[is.na(ef_eclipse)] <- 0
  rm(NAregions, MissingRegions, AssociatedGAINSregions)

  # define exogenous emission data
  emissions_exogenous <- emissions[, , dimSector_skipEF]

  # make output dummy "ef" and "emi" which then has to be filled by the data
  ef <- do.call(
    "mbind",
    lapply(
      scenario,
      function(s) {
        new.magpie(
          getItems(ef_eclipse, dim = 1),
          c(2005, 2010, 2030, 2050, 2100),
          gsub("CLE", s, getNames(ef_eclipse[, , "CLE"]))
        )
      }
    )
  )

  getSets(ef) <- c("region", "year", "data1", "data2", "data3") # forcing set names to avoid errors while filtering

  emi <- do.call(
    "mbind",
    lapply(
      scenario,
      function(s) {
        new.magpie(
          getItems(emissions_exogenous, dim = 1),
          c(2005, 2010, 2030, 2050, 2100),
          gsub("CLE", s, getNames(emissions_exogenous[, , "CLE"]))
        )
      }
    )
  )

  # define country categories
  if (p_countryCategories == "perCountry") {
    # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
    r_L <- dimnames(gdp_cap[getItems(ef, dim = 1), , ])$ISO3[which(gdp_cap[getItems(ef, dim = 1), , ] <= 2750)]
    # high and medium income countries
    r_HM <- setdiff(getItems(ef, dim = 1), r_L)
    # High-Medium income countries with strong pollution policies in place
    r_HMStrong <- c("AUS", "CAN", "USA", "JPN") # FIXME which definition???
    # High-Medium income countries with lower emissions goals
    r_HMRest <- setdiff(r_HM, r_HMStrong)
  } else {
    # Compute mean GDP/Cap per GAINS region
    regionMean_gdppcap <- sapply(unique(map_regions$RegionCode), function(x) {
      mean(gdp_cap[map_regions$CountryCode[map_regions$RegionCode == x], , ])
    })

    # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
    r_L <- levels(map_regions$CountryCode[map_regions$RegionCode %in% names(regionMean_gdppcap[regionMean_gdppcap <= 2750])])
    # high and medium income countries
    r_HM <- setdiff(getItems(ef, dim = 1), r_L)
    # High-Medium income countries with strong pollution policies in place
    r_HMStrong <- map_regions$CountryCode[map_regions$RegionCode %in% c("Western Europe", "Japan")] # FIXME definition taken from JeS matlab script
    # High-Medium income countries with lower emissions goals
    r_HMRest <- setdiff(r_HM, r_HMStrong)
  }

  # generate FLE and SSP scenarios
  # -------- Fix all scenarios to CLE in 2005 and 2010 ----------
  ef[, c(2005, 2010), ] <- ef_eclipse[, c(2005, 2010), "CLE"]
  emi[, c(2005, 2010), ] <- emissions_exogenous[, c(2005, 2010), "CLE"]

  # ---------------- FLE ----------------------------------------
  # FLE: CLE 2010 emission factors and emissions are held constant
  ef[, , "FLE"] <- setYears(ef[, 2010, "FLE"], NULL) # NULL is actually the default value, skipping afterwards
  emi[, , "FLE"] <- setYears(emi[, 2010, "FLE"], NULL)

  # ---------------- SSP1 ---------------------------------------
  # Emission factors
  # low income countries
  ef[r_L, 2030, "SSP1"] <- ef_eclipse[r_L, 2030, "CLE"] # 2030: CLE30
  ef[r_L, 2050, "SSP1"] <- base::pmin(
    setYears(ef[r_L, 2030, "SSP1"]),
    setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE"))
  ) # 2050: CLE30 WEU, if not higher than 2030 value

  # 2100: SLE30, if not higher than 2050 value
  ef[r_L, 2100, "SSP1"] <- base::pmin(setYears(ef[r_L, 2050, "SSP1"]), setYears(ef_eclipse[r_L, 2030, "SLE"]))
  # high income countries
  ef[r_HM, 2030, "SSP1"] <- 0.75 * ef_eclipse[r_HM, 2030, "CLE"] # 2030: 75% of CLE30
  # 2050: SLE30, if not higher than 2030 value
  ef[r_HM, 2050, "SSP1"] <- base::pmin(setYears(ef[r_HM, 2030, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "SLE"]))
  # 2100: MFR, if not higher than 2050 value
  ef[r_HM, 2100, "SSP1"] <- base::pmin(setYears(ef[r_HM, 2050, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "MFR"]))

  # Emissions
  # low income countries
  emi[r_L, 2030, "SSP1"] <- emissions_exogenous[r_L, 2030, "CLE"] # 2030: CLE30
  emi[r_L, 2050, "SSP1"] <- base::pmin(setYears(emi[r_L, 2030, "SSP1"]), setYears(0.5 * emissions_exogenous[r_L, 2030, "CLE"]
    + 0.5 * emissions_exogenous[r_L, 2030, "SLE"])) # 2050: CLE30 WEU, if not higher than 2030 value
  emi[r_L, 2100, "SSP1"] <- base::pmin(setYears(emi[r_L, 2050, "SSP1"]), setYears(emissions_exogenous[r_L, 2030, "SLE"])) # 2100: SLE30, if not higher than 2050 value
  # high income countries
  emi[r_HM, 2030, "SSP1"] <- 0.75 * emissions_exogenous[r_HM, 2030, "CLE"] # 2030: 75% of CLE30
  emi[r_HM, 2050, "SSP1"] <- base::pmin(setYears(emi[r_HM, 2030, "SSP1"]), setYears(emissions_exogenous[r_HM, 2030, "SLE"])) # 2050: SLE30, if not higher than 2030 value
  emi[r_HM, 2100, "SSP1"] <- base::pmin(setYears(emi[r_HM, 2050, "SSP1"]), setYears(emissions_exogenous[r_HM, 2030, "MFR"])) # 2100: MFR, if not higher than 2050 value

  # ----------------- SSP2 --------------------------------------
  # Emission factors
  # High-Medium income countries with strong pollution policies in place
  ef[r_HMStrong, 2030, "SSP2"] <- ef_eclipse[r_HMStrong, 2030, "CLE"] # 2030: CLE30
  ef[r_HMStrong, 2050, "SSP2"] <- base::pmin(
    setYears(ef[r_HMStrong, 2030, "SSP2"]),
    setYears(ef_eclipse[r_HMStrong, 2030, "SLE"])
  ) # 2050: SLE30
  ef[r_HMStrong, 2100, "SSP2"] <- base::pmin(
    setYears(ef[r_HMStrong, 2050, "SSP2"]),
    setYears(allocate_min2r_ef(ef_eclipse, r_HMStrong, r_oecd, 2030, "SLE"))
  ) # 2100: Lowest SLE30 or lower
  # High-Medium income countries with lower emissions goals
  ef[r_HMRest, 2030, "SSP2"] <- ef_eclipse[r_HMRest, 2030, "CLE"] # 2030: CLE30
  ef[r_HMRest, 2050, "SSP2"] <- base::pmin(
    setYears(ef[r_HMRest, 2030, "SSP2"]),
    setYears(allocate_min2r_ef(ef_eclipse, r_HMRest, r_HMRest, 2030, "CLE"))
  ) # 2050: Min CLE30
  ef[r_HMRest, 2100, "SSP2"] <- base::pmin(
    setYears(ef[r_HMRest, 2050, "SSP2"]),
    setYears(allocate_c2r_ef(ef_eclipse, r_HMRest, select_weu, 2030, "SLE"))
  ) # 2100: SLE30 WEU
  # low income countries
  ef[r_L, 2030, "SSP2"] <- setYears(ef_eclipse[r_L, 2020, "CLE"]) # 2030: CLE20
  ef[r_L, 2050, "SSP2"] <- base::pmin(
    setYears(ef[r_L, 2030, "SSP2"]),
    setYears(allocate_min2r_ef(ef_eclipse, r_L, r_L, 2030, "CLE"))
  ) # 2050: Min CLE30
  ef[r_L, 2100, "SSP2"] <- base::pmin(
    setYears(ef[r_L, 2050, "SSP2"]),
    setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE"))
  ) # 2100: CLE30 WEU

  # Emissions
  # High-Medium income countries with strong pollution policies in place
  emi[r_HMStrong, 2030, "SSP2"] <- emissions_exogenous[r_HMStrong, 2030, "CLE"] # 2030: CLE30
  emi[r_HMStrong, 2050, "SSP2"] <- base::pmin(
    setYears(emi[r_HMStrong, 2030, "SSP2"]),
    setYears(emissions_exogenous[r_HMStrong, 2030, "SLE"])
  ) # 2050: SLE30
  emi[r_HMStrong, 2100, "SSP2"] <- base::pmin(
    setYears(emi[r_HMStrong, 2050, "SSP2"]),
    setYears(emissions_exogenous[r_HMStrong, 2030, "SLE"] * 0.8)
  ) # 2100: Lowest SLE30 or lower -> 0.8*SLE30
  # High-Medium income countries with lower emissions goals
  emi[r_HMRest, 2030, "SSP2"] <- emissions_exogenous[r_HMRest, 2030, "CLE"] # 2030: CLE30
  emi[r_HMRest, 2050, "SSP2"] <- base::pmin(
    setYears(emi[r_HMRest, 2030, "SSP2"]),
    setYears(emissions_exogenous[r_HMRest, 2030, "SLE"])
  ) # 2050: Min CLE30 -> SLE30
  emi[r_HMRest, 2100, "SSP2"] <- base::pmin(
    setYears(emi[r_HMRest, 2050, "SSP2"]),
    setYears(emissions_exogenous[r_HMRest, 2030, "SLE"] * 0.8)
  ) # 2100: SLE30 WEU -> 0.8*SLE30
  # low income countries
  emi[r_L, 2030, "SSP2"] <- setYears(emissions_exogenous[r_L, 2020, "CLE"]) # 2030: CLE20
  emi[r_L, 2050, "SSP2"] <- base::pmin(
    setYears(emi[r_L, 2030, "SSP2"]),
    setYears(emissions_exogenous[r_L, 2030, "CLE"])
  ) # 2050: Min CLE30 -> CLE30
  emi[r_L, 2100, "SSP2"] <- base::pmin(
    setYears(emi[r_L, 2050, "SSP2"]),
    setYears(emissions_exogenous[r_L, 2030, "SLE"] * 0.95)
  ) # 2100: CLE30 WEU -> 0.95*SLE30

  # H-M-Strong:   2030 CLE30; 2050 SLE30;     2100 Lowest SLE30 or lower [EUR, JPN]                     = [3 5]
  # H-M-Rest:     2030 CLE30; 2050 Min CLE30; 2100 EUR SLE30             [CHN, LAM, MEA, ROW, RUS, USA] = [2 6 7 9 10 11]
  # Low:          2030 CLE20; 2050 Min CLE30; 2100 EUR CLE30             [AFR, IND, OAS]                = [1 4 8]

  # ----------------- SSP3 --------------------------------------
  # Emission factors
  # High-Medium income countries with strong pollution policies in place
  ef[r_HMStrong, 2030, "SSP3"] <- ef_eclipse[r_HMStrong, 2030, "CLE"]                                                # 2030: CLE30
  ef[r_HMStrong, 2050, "SSP3"] <- base::pmin(setYears(ef[r_HMStrong,        2030, "SSP3"]),
                                       setYears(ef_eclipse[r_HMStrong, 2030, "CLE"]))                                # 2050: CLE30
  ef[r_HMStrong, 2100, "SSP3"] <- base::pmin(setYears(ef[r_HMStrong,        2050, "SSP3"]),
                                       setYears(allocate_min2r_ef(ef_eclipse, r_HMStrong, r_oecd, 2030, "CLE")))   # 2100: Lowest CLE30 or lower
  # High-Medium income countries with lower emissions goals
  ef[r_HMRest, 2030, "SSP3"]  <- ef_eclipse[r_HMRest, 2030, "CLE"]                                                   # 2030: CLE30
  ef[r_HMRest, 2050, "SSP3"]  <- base::pmin(setYears(ef[r_HMRest,       2030, "SSP3"]),
                                      setYears(allocate_min2r_ef(ef_eclipse, r_HMRest, r_HMRest, 2030, "CLE")))    # 2050: Min CLE30
  ef[r_HMRest, 2100, "SSP3"]  <- base::pmin(setYears(ef[r_HMRest, 2050, "SSP3"]),
                                      setYears(allocate_c2r_ef(ef_eclipse, r_HMRest, select_weu, 2030, "CLE")))    # 2100: CLE30 WEU
  # low income countries
  ef[r_L, 2030, "SSP3"]       <- setYears(ef_eclipse[r_L, 2020, "CLE"])                                            # 2030: CLE20
  ef[r_L, 2050, "SSP3"]       <- base::pmin(setYears(ef[r_L,       2030, "SSP3"]),
                                      setYears(allocate_min2r_ef(ef_eclipse, r_L, r_L, 2030, "CLE")))              # 2050: Min CLE30
  ef[r_L, 2100, "SSP3"]       <- base::pmin(setYears(ef[r_L, 2050, "SSP3"]),
                                      setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))         # 2100: CLE30 WEU

  # Emissions
  # High-Medium income countries with strong pollution policies in place
  emi[r_HMStrong, 2030, "SSP3"] <- emissions_exogenous[r_HMStrong, 2030, "CLE"]                                               # 2030: CLE30
  emi[r_HMStrong, 2050, "SSP3"] <- base::pmin(setYears(emi[r_HMStrong,        2030, "SSP3"]),
                                        setYears(emissions_exogenous[r_HMStrong, 2030, "CLE"]))                                # 2050: CLE30
  emi[r_HMStrong, 2100, "SSP3"] <- base::pmin(setYears(emi[r_HMStrong,        2050, "SSP3"]),
                                        setYears(emissions_exogenous[r_HMStrong, 2030, "CLE"] ))                           # 2100: Lowest CLE30 or lower -> 0.8*CLE30
  # High-Medium income countries with lower emissions goals
  emi[r_HMRest, 2030, "SSP3"]  <- emissions_exogenous[r_HMRest, 2030, "CLE"]                                                  # 2030: CLE30
  emi[r_HMRest, 2050, "SSP3"]  <- base::pmin(setYears(emi[r_HMRest,       2030, "SSP3"]),
                                       setYears(emissions_exogenous[r_HMRest, 2030, "CLE"]))                                  # 2050: Min CLE30 -> CLE30
  emi[r_HMRest, 2100, "SSP3"]  <- base::pmin(setYears(emi[r_HMRest, 2050, "SSP3"]),
                                       setYears(emissions_exogenous[r_HMRest, 2030, "CLE"] ))                              # 2100: CLE30 WEU -> 0.8*CLE30
  # low income countries
  emi[r_L, 2030, "SSP3"]       <- setYears(emissions_exogenous[r_L, 2020, "CLE"])                                           # 2030: CLE20
  emi[r_L, 2050, "SSP3"]       <- base::pmin(setYears(emi[r_L, 2030, "SSP3"]),
                                       setYears(emissions_exogenous[r_L, 2030, "CLE"]))                                     # 2050: Min CLE30 -> CLE30
  emi[r_L, 2100, "SSP3"]       <- base::pmin(setYears(emi[r_L, 2050, "SSP3"]),
                                       setYears(emissions_exogenous[r_L, 2030, "CLE"] ))                                # 2100: CLE30 WEU -> 0.95*CLE30

  # ----------------- SSP4 --------------------------------------
  # TODO

  # ----------------- SSP5 --------------------------------------
  # set SSP5 to the values of SSP1
  ef[, , "SSP5"] <- ef[, , "SSP1"]
  emi[, , "SSP5"] <- emi[, , "SSP1"] # does not really make sense...

  # filter all regions and sectors that are constant between 2030 and 2050 and continue to decline afterwards. Replace by linear interpolation
  # between 2030 and 2100

  # ----------------- CLE and MFR -------------------------------
  ef[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")] <- ef_eclipse[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")]
  ef[, 2100, c("CLE", "MFR")] <- setYears(ef_eclipse[, 2050, c("CLE", "MFR")]) # for 2100, take the same values as in 2050

  emi[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")] <- emissions_exogenous[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")]
  emi[, 2100, c("CLE", "MFR")] <- setYears(emissions_exogenous[, 2050, c("CLE", "MFR")]) # for 2100, take the same values as in 2050

  # ---------------- Global EURO6 for Transports -----------------------------
  ef[, c(2030, 2050, 2100), "GlobalEURO6"] <- ef[, c(2030, 2050, 2100), "SSP2"]
  ef[, c(2030, 2050, 2100), "GlobalEURO6"][, , transportNames] <- setCells(ef["FRA", c(2030, 2050, 2100), "GlobalEURO6"][, , transportNames], "GLO")

  emi[, c(2030, 2050, 2100), "GlobalEURO6"] <- emi[, c(2030, 2050, 2100), "SSP2"]

  # ---------------- MFR Transports -----------------------------
  ef[, c(2030, 2050, 2100), "MFR_Transports"] <- ef[, c(2030, 2050, 2100), "SSP2"]
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "MFR_Transports") <- mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "MFR")

  emi[, c(2030, 2050, 2100), "MFR_Transports"] <- emi[, c(2030, 2050, 2100), "SSP2"]

  # ---------------- FLE_building_transport ------------------------------
  ef[, c(2030, 2050, 2100), "FLE_building_transport"] <- ef[, c(2030, 2050, 2100), "SSP2"]
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data3 = "FLE_building_transport") <- mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data3 = "FLE")
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "FLE_building_transport") <- mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "FLE")

  emi[, c(2030, 2050, 2100), "FLE_building_transport"] <- emi[, c(2030, 2050, 2100), "SSP2"]

  # ---------------- SLCF_building_transport ------------------------------
  ef[, c(2030, 2050, 2100), "SLCF_building_transport"] <- ef[, c(2030, 2050, 2100), "SSP2"]
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data2 = c("BC", "OC"), data3 = "SLCF_building_transport") <- mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data2 = c("BC", "OC"), data3 = "FLE")
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data2 = c("BC", "OC"), data3 = "SLCF_building_transport") <- mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data2 = c("BC", "OC"), data3 = "FLE")

  emi[, c(2030, 2050, 2100), "SLCF_building_transport"] <- emi[, c(2030, 2050, 2100), "SSP2"]

  # ----- Aggregate back to REMIND regions (to speed up processing)
  emiNam <- getNames(ef, TRUE)[2:3]
  newdim <- apply(
    sapply(
      do.call("expand.grid", emiNam), as.character
    ),
    1, paste,
    collapse = "."
  )

  activities.EF <- do.call(
    "mbind",
    lapply(
      newdim,
      function(scen) {
        setNames(activities[, , dimSector_EF], paste(getNames(activities[, , dimSector_EF]), scen, sep = "."))
      }
    )
  )

  getSets(ef) <- c("region", "year", "sector.species.scenario")
  getSets(activities.EF) <- c("region", "year", "sector.species.scenario")

  # ----- EFs for advanced coal and biomass technologies -------
  mapsec <- map_sectors_ECLIPSE2REMIND %>%
    filter(.data$eclipse %in% getNames(ef, dim = 1)) %>%
    select(c(1, 3))

  ef <- toolAggregate(ef, mapsec, dim = 3.1)

  adv_techs <- c("igcc", "igccc", "coalgas", "bioigcc", "bioigccc", "biogas")
  adv_coaltechs <- c("igcc", "igccc")
  adv_specs <- c("NOx", "SO2", "BC", "OC")
  adv_factor <- c(0.85, 0.6, 0.6, 0.6)

  for (kscen in getNames(ef, dim = 6)) {
    for (ktech in adv_techs) {
      curtech <- ifelse(ktech %in% adv_coaltechs, "Power_Gen_Coal.", "Power_Gen_Bio_Trad.")
      for (kspec in adv_specs) {
        nm <- getNames(ef[, , paste0("power.", kspec)][, , ktech][, , kscen])
      }
      ef[, intersect(getYears(ef), getYears(ef_eclipse)), paste0("power.", kspec)][, , ktech][, , kscen] <- setNames(
        base::pmin(
          mbind(lapply(getYears(ef), function(x) {
            setYears(ef_eclipse[, 2030, paste0(curtech, kspec, ".MFR")] / adv_factor[adv_specs == kspec], x)
          }))[, intersect(getYears(ef), getYears(ef_eclipse)), ],
          ef_eclipse[, intersect(getYears(ef), getYears(ef_eclipse)), paste0(curtech, kspec, ".CLE")]
        ),
        nm
      )
    }
  }

  # exogenous emissions
  mapsec <- map_sectors_ECLIPSE2REMIND[map_sectors_ECLIPSE2REMIND$eclipse %in% getNames(emi, dim = 1), c(1, 3)]
  emi <- toolAggregate(emi, mapsec, dim = 3.1)

  # interpolate data (EFs and activities) over time. Remove y2000 from activities (this is the first time item hence -1)
  vcat(2, "  > Interpolate data over time... \n")
  ef <- time_interpolate(ef, interpolated_year = time, integrate_interpolated_years = TRUE, extrapolation_type = "constant")
  emi <- time_interpolate(emi, interpolated_year = time, integrate_interpolated_years = TRUE, extrapolation_type = "constant")

  if (subtype == "emissions") {
    x <- emi
    w <- NULL
  } else if (subtype == "emission_factors") {
    x <- ef
    w <- setYears(activities.EF[, 2010, dimSector_EF])
    mapsec <- map_sectors_ECLIPSE2REMIND %>%
      filter(.data$eclipse %in% getNames(w, dim = 1)) %>%
      select(c(1, 3))
    w <- toolAggregate(w, mapsec, dim = 3.1)
    # avoid zero weights, as they cause a warning in aggregation
    w[w == 0] <- 1e-10
  } else {
    stop("do not know which weight to use for activities")
  }

  return(list(
    x = x,
    weight = w,
    unit = "unit",
    description = "calcECLIPSE substitute"
  ))
}

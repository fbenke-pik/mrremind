#' Calculate selected REMIND energy and emission variables from historical JRC IDEES values
#'
#' @md
#' @param subtype one of
#'   - `'Industry'`: calculate REMIND Industry variables
#'   - `'Transport'`: calculate REMIND Transport variables
#'   - `'ResCom'`: calculate REMIND Residential and Commercial variables
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#'
calcJRC_IDEES <- function(subtype) {

  subtypes <- c("Industry", "Transport", "ResCom")
  if (!subtype %in% subtypes) {
    stop("Invalid subtype -- supported subtypes are: ",
         paste(subtypes, collapse = ", "))
  }

  if (subtype == "Industry") {
    ind <- readSource("JRC_IDEES", subtype = "Industry")
    emi <- readSource("JRC_IDEES", subtype = "Emission")
    energy <- readSource("JRC_IDEES", subtype = "Energy")
    jrc <- mbind(ind, emi, energy)
    mapping <- toolGetMapping("Mapping_JRC_IDEES_REMIND_Industry.csv",
                              type = "reportingVariables", where = "mrremind")
  } else if (subtype == "Transport") {
    transport <- readSource("JRC_IDEES", subtype = "Transport")
    mbunkers <- readSource("JRC_IDEES", subtype = "MBunkers")
    jrc <- mbind(transport, mbunkers)
    mapping <- toolGetMapping("Mapping_JRC_IDEES_REMIND_Transport.csv",
                              type = "reportingVariables", where = "mrremind")
  } else {
    residential <- readSource("JRC_IDEES", subtype = "Residential")
    services <- readSource("JRC_IDEES", subtype = "Tertiary")
    jrc <- mbind(residential, services)
    mapping <- toolGetMapping("Mapping_JRC_IDEES_REMIND_ResCom.csv",
                              type = "reportingVariables", where = "mrremind")
  }

  mapping <- mapping %>%
    mutate("conversion" = as.numeric(!!sym("Factor")) * !!sym("Weight")) %>%
    select("variable" = "JRC_complete", "REMIND_variable", "conversion", "unit" = "Unit_JRC", "Unit_REMIND")

  mapping$variable <- gsub(pattern = "\\.", replacement = "_", mapping$variable) %>% trimws()
  mapping$REMIND_variable <- trimws(mapping$REMIND_variable)

  cntr <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder")
  EU28_regions <- cntr[which(cntr$RegionCode == "EUR"), ]$CountryCode

  x <- left_join(
    jrc %>%
      mselect(iso3c = EU28_regions, variable = unique(mapping$variable)) %>%
      as.data.frame() %>%
      as_tibble() %>%
      select("region" = "Region", "year" = "Year", "variable" = "Data1",
             "unit" = "Data2", "value" = "Value"),
    mapping,
    by = "variable"
  ) %>%
    mutate("value" =  !!sym("value") * !!sym("conversion"),
           "REMIND_variable" = paste0(!!sym("REMIND_variable"),  " (", !!sym("Unit_REMIND"), ")")) %>%
    select("variable" = "REMIND_variable", "region", "year", "value")

  x <- stats::aggregate(value ~ variable + region + year, x, sum) %>%
    as.magpie() %>%
    toolCountryFill(fill = NA, verbosity = 2) %>%
    toolFillEU34Countries()

  # convert currency units from EUR 2010 to $2017
  if (subtype == "Industry") {
    tmp <-  x[, , "EUR2010", pmatch = TRUE]
    x <- x[, , getNames(tmp), invert = TRUE]
    getNames(tmp) <- gsub("EUR2010", "US$2017", getNames(tmp))

    tmp <- GDPuc::toolConvertGDP(
      gdp = tmp,
      unit_in = "constant 2010 EUR",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )
    x <- mbind(x, tmp)
  }

  return(list(x = x, weight = NULL,
              unit = "billion US$2017/yr, EJ/yr, Mt CO2/yr, Mt/yr",
              description = "Historical JRC IDEES values as REMIND variables"))
}

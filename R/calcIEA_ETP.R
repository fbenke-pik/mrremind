#' Calculate REMIND emission variables from IEA ETP values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom rlang sym
#' @importFrom stats aggregate na.pass
#' @export

calcIEA_ETP <- function() {

  mapping <- toolGetMapping("Mapping_IEA_ETP.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(!!sym("REMIND")), !!sym("REMIND") != "") %>%
    mutate("Conversion" = as.numeric(!!sym("Conversion"))) %>%
    select("variable" = "IEA_ETP", "REMIND", "Conversion", "Unit_REMIND")

  mapping$variable <- trimws(mapping$variable)
  mapping$REMIND <- trimws(mapping$REMIND)

  x1 <- readSource("IEA_ETP", subtype = "industry")
  x2 <- readSource("IEA_ETP", subtype = "transport")
  x3 <- readSource("IEA_ETP", subtype = "buildings")
  x4 <- readSource("IEA_ETP", subtype = "summary")

  data <- mbind(x1, x2, x3, x4)

  data <- as.data.frame(data) %>%
    as_tibble() %>%
    select(
      "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
      "year" = "Year", "value" = "Value"
    )

  x <- left_join(
    data,
    mapping,
    by = "variable",
    relationship = "many-to-many"
  ) %>%
    filter(!!sym("REMIND") != "") %>%
    mutate(
      "value" = !!sym("value") * !!sym("Conversion"),
      "REMIND" = paste0(!!sym("REMIND"), " (", !!sym("Unit_REMIND"), ")"),
      "model" = paste0("IEA ETP ", !!sym("scenario")),
      "year" = as.numeric(as.character(!!sym("year")))
    ) %>%
    select("region", "year", "model", "variable" = "REMIND", "value")

  x <- aggregate(value ~ region + year + model + variable, x, sum, na.action = na.pass) %>%
    as.magpie()

  return(list(
    x = x,
    weight = NULL,
    unit = c("EJ/yr", "Mt CO2/yr", "Mt/yr", "bn pkm/yr", "bn tkm/yr"),
    description = "IEA ETP projections as REMIND variables"
  ))

}

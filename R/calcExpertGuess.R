#' Calculate expert guesses
#'
#' @author Falk Benke
#'
#' @param subtype must be one of
#' 'biocharPrices'
#' 'subConvergenceRollback'
#' 'tradeConstraints'
#' 'taxConvergence'
#' 'taxConvergenceRollback'
#' @export
calcExpertGuess <- function(subtype) {

  subtypes <- c(
    "biocharPrices",
    "subConvergenceRollback",
    "tradeConstraints",
    "taxConvergence",
    "taxConvergenceRollback"
  )

  if (!(subtype %in% subtypes)) {
    stop("Invalid subtype. Supported subtypes: ", paste0(subtypes, collapse = ", "))
  }

  isocountries <- c(
    "biocharPrices" = FALSE,
    "subConvergenceRollback" = TRUE,
    "tradeConstraints" = FALSE,
    "taxConvergence" = TRUE,
    "taxConvergenceRollback" = TRUE
  )

  x <- readSource("ExpertGuess", subtype = subtype, convert = isocountries[[subtype]])


  if (subtype == "biocharPrices") {

    unit <- "USD 2015/t biochar"
    description <- glue::glue("Biochar price assumptions over time. Assumptions \\
    based on collection of current bulk sale prices (see Dorndorf et al (submitted)).")
    weight <- NULL

  } else if (subtype == "tradeConstraints") {

    unit <- "unitless"
    description <- glue::glue(
      "parameter by Nicolas Bauer (2024) for the region specific \\
      trade constraints, values different to 1 activate constraints \\
      and the value is used as effectiveness to varying degrees \\
      such as percentage numbers"
    )
    weight <- NULL

  } else if (subtype == "taxConvergence") {


    unit <- "US$2017/GJ"
    description <- glue::glue("Tax convergence level for specific regions, year \\
                              and final energy type")
    weight <- x
    weight[, , ] <- 1

  } else if (subtype == "taxConvergenceRollback") {

    unit <- "US$2017/GJ"
    description <- glue::glue("Tax convergence level for specific regions, year \\
                              and final energy type in rollback scenario")
    weight <- x
    weight[, , ] <- 1

  } else if (subtype == "subConvergenceRollback") {

    unit <- "US$2017/GJ"
    description <- glue::glue("Subsidy convergence level for specific regions, \\
                              year, emission sectors and final energy type in \\
                              rollback scenario")
    weight <- x
    weight[, , ] <- 1

  }

  return(list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    isocountries = isocountries[[subtype]]
  ))
}

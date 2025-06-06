#' Ariadne database scenario data
#' @description  Scenario data from the Ariadne modeling intercomparison project for Germany.
#' See README in input file for more details.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer
#' @importFrom dplyr filter mutate
#'
readAriadneDB <- function() {

  # read in file
  data <- readxl::read_excel(
    "IIASA_DB_Complete_06_03_2024.xlsx",
    sheet = "data",
    col_types = c(
      rep("text", 5),
      rep("numeric", 19)
    )
  )

  # rearrange and convert to magclass object
  out <- data %>%
    tidyr::gather("period", "value", -"model", -"scenario", -"region", -"variable", -"unit") %>%
    filter(!is.na(.data$value)) %>%
    as.magpie(temporal = 6, spatial = 3, datacol = 7)

  return(out)
}

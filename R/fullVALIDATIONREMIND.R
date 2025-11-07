#' Generate Validation Data for REMIND
#'
#' Function that generates the historical regional dataset against which the
#' REMIND model results can be compared.
#'
#' @md
#' @param rev Unused parameter, but required by `madrat`.
#' @author David Klein, Falk Benke
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}}, \code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{
#' fullVALIDATIONREMIND()
#' }

fullVALIDATIONREMIND <- function(rev = 0) {

  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.

  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional", where = "mappingfolder")),
      c("X", "CountryCode")
    )

    if (any(columns %in% rel)) {
      warning(
        "The following column(s) from ", mapping,
        " exist in another mapping an will be ignored: ",
        paste(columns[columns %in% rel], collapse = ", ")
      )
    }

    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub(
    "RegionCode", "region",
    paste(rel, collapse = "+")
  )

  # historical data ----
  valfile <- "historical.mif"

  # Population data from WDI ----

  pop <- calcOutput("PopulationPast", aggregate = columnsForAggregation, try = FALSE)
  getNames(pop) <- paste0("Population (million)")
  write.report(pop, file = valfile, append = TRUE, scenario = "historical", model = "WDI")

  # GDP in ppp from WDI ----

  gdp <- calcOutput("GDPPast", pastData = "WDI", aggregate = columnsForAggregation, try = FALSE) / 1000
  getNames(gdp) <- paste0("GDP|PPP (billion US$2017/yr)")
  write.report(gdp, file = valfile, append = TRUE, scenario = "historical", model = "WDI")

}

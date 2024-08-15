#' Read in IEA World Energy Outlook Data from 2023
#'
#' @author Falk Benke
#' @param subtype, either 'outlook' or 'assumptions'
#' @importFrom dplyr filter distinct group_by ungroup rename_all
#'
readIEA_WorldEnergyOutlook <- function(subtype) { # nolint

  if (subtype == "outlook") {
    data <- rbind(
      read.csv2(
        file = file.path("2023", "complete", "WEO2023_Extended_Data_Regions.csv"),
        sep = ","
      ) %>% rename_all(tolower),
      read.csv2(
        file = file.path("2023", "complete", "WEO2023_Extended_Data_Supply_Refining_H2_Trade_Prices.csv"),
        sep = ","
      ) %>% rename_all(tolower),
      read.csv(
        file = file.path("2023", "complete", "WEO2023_Extended_Data_World.csv"),
        sep = ","
      ) %>% rename_all(tolower),
      read.csv(
        file = file.path("2023", "complete", "WEO2023_Extended_Data_Investment.csv"),
        sep = ","
      ) %>% rename_all(tolower)
    ) %>%
      mutate(
        "value" = ifelse(.data$unit == "PJ", as.numeric(.data$value) / 1000, as.numeric(.data$value)),
        "unit" = ifelse(.data$unit == "PJ", "EJ", .data$unit),
        "variable" = paste0(.data$category, "-", .data$product, "-", .data$flow, " (", .data$unit, ")")
      ) %>%
      select("region", "year", "scenario", "variable", "value") %>%
      group_by(.data$region, .data$year, .data$scenario, .data$variable) %>%
      distinct() %>%
      ungroup()

    # investment data uses yearly ranges and needs special treatment
    # we currently don't read in cumulative investment spending, only annual average spending
    rangeData <- data %>% filter(
      is.na(suppressWarnings(as.numeric(.data$year))),
      grepl("Investment spending, annual average", .data$variable)
    )

    # remove non-annual data
    data <- data %>%
      filter(!is.na(suppressWarnings(as.numeric(.data$year))))

    years <- as.numeric(unique(data$year))

    splitData <- data.frame()

    for (i in seq(1, nrow(rangeData))) {
      d <- rangeData[i, ]
      minY <- as.numeric(sub("-[0-9]{4}", "", d[, "year"]))
      maxY <- as.numeric(sub("[0-9]{4}-", "", d[, "year"]))
      y <- data.frame(year = seq(minY, maxY, 1), variable = d[, "variable"]) %>%
        filter(.data$year %in% years)
      splitData <- rbind(splitData, left_join(y, select(d, -2), by = "variable"))
    }

    data <- rbind(data, splitData)

    as.magpie(data, temporal = 2, spatial = 1, datacol = 5) %>%
      magpiesort() %>%
      return()
  } else if (subtype == "assumptions") {

    colNamesFossil <- c(
      "tech",
      "region",
      "Stated Policies - Capital costs (USD/kW) - 2022",
      "Stated Policies - Capital costs (USD/kW) - 2030",
      "Stated Policies - Capital costs (USD/kW) - 2050",
      "Stated Policies - Annual O&M Costs (USD/kW) - 2022",
      "Stated Policies - Annual O&M Costs (USD/kW) - 2030",
      "Stated Policies - Annual O&M Costs (USD/kW) - 2050",
      "Stated Policies - Efficiency (gross, LHV) - 2022",
      "Stated Policies - Efficiency (gross, LHV) - 2030",
      "Stated Policies - Efficiency (gross, LHV) - 2050",
      "Net Zero Emissions by 2050 - Capital costs (USD/kW) - 2030",
      "Net Zero Emissions by 2050 - Capital costs (USD/kW) - 2050",
      "Net Zero Emissions by 2050 - Annual O&M Costs (USD/kW) - 2030",
      "Net Zero Emissions by 2050 - Annual O&M Costs (USD/kW) - 2050",
      "Net Zero Emissions by 2050 - Efficiency (gross, LHV) - 2030",
      "Net Zero Emissions by 2050 - Efficiency (gross, LHV) - 2050"
    )

    colNamesRenewable <- c(
      "tech",
      "region",
      "Stated Policies - Capital costs (USD/kW) - 2022",
      "Stated Policies - Capital costs (USD/kW) - 2030",
      "Stated Policies - Capital costs (USD/kW) - 2050",
      "Stated Policies - Annual O&M Costs (USD/kW) - 2022",
      "Stated Policies - Annual O&M Costs (USD/kW) - 2030",
      "Stated Policies - Annual O&M Costs (USD/kW) - 2050",
      "Stated Policies - Efficiency (gross, LHV) - 2022",
      "Stated Policies - Efficiency (gross, LHV) - 2030",
      "Stated Policies - Efficiency (gross, LHV) - 2050",
      "Stated Policies - Capacity factor (%) - 2022",
      "Stated Policies - Capacity factor (%) - 2030",
      "Stated Policies - Capacity factor (%) - 2050",
      "Stated Policies - Construction Time (years) - 2022",
      "Stated Policies - Construction Time (years) - 2030",
      "Stated Policies - Construction Time (years) - 2050",
      "Net Zero Emissions by 2050 - Capital costs (USD/kW) - 2030",
      "Net Zero Emissions by 2050 - Capital costs (USD/kW) - 2050",
      "Net Zero Emissions by 2050 - Annual O&M Costs (USD/kW) - 2030",
      "Net Zero Emissions by 2050 - Annual O&M Costs (USD/kW) - 2050",
      "Net Zero Emissions by 2050 - Efficiency (gross, LHV) - 2030",
      "Net Zero Emissions by 2050 - Efficiency (gross, LHV) - 2050",
      "Net Zero Emissions by 2050 - Capacity factor (%) - 2030",
      "Net Zero Emissions by 2050 - Capacity factor (%) - 2050",
      "Net Zero Emissions by 2050 - Construction Time (years) - 2030",
      "Net Zero Emissions by 2050 - Construction Time (years) - 2050"
    )

    sheets <- list(
      "Coal" = list(
        range = "A9:U48",
        colnames = colNamesFossil,
        maintech = "Coal"
      ),
      "Gas" = list(
        range = "A9:U48",
        colnames = colNamesFossil,
        maintech = "Gas"
      ),
      "Fossil fuels equipped with CCUS" = list(
        range = "A9:U48",
        colnames = colNamesFossil,
        maintech = "CCS"
      ),
      "Nuclear" = list(
        range = "H9:AB18",
        colnames = colNamesFossil,
        maintech = "Nuclear"
      ),
      "Renewables" = list(
        range = "A9:AI138",
        colnames = colNamesRenewable,
        maintech = "Renewables"
      )
    )

    out <- NULL

    for (s in names(sheets)) {
      data <- suppressMessages(readxl::read_xlsx(
        path = file.path("2023", "complete", "WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx"),
        sheet = s, range = sheets[[s]][["range"]], col_names = FALSE, na = "n.a."
      ))

      # remove empty columns
      d <- data[, !apply(data, 2, function(x) all(is.na(x)))]

      # shift technology to separate column
      techIndex <- apply(d[, -1], 1, function(x) all(is.na(x)))
      d <- cbind(d[, 1], d)
      colnames(d) <- sheets[[s]][["colnames"]]
      d[!techIndex, 1] <- NA
      # Russia and Japan can also have all-NA rows
      d[d$tech %in% c("Russia", "Japan"), 1] <- NA
      d <- tidyr::fill(d, "tech", .direction = "down")
      d <- d[!techIndex, ]

      # pivot to long format
      d <- pivot_longer(d, cols = -c("tech", "region")) %>%
        mutate(
          "maintech" = sheets[[s]][["maintech"]],
          "scenario" = gsub(" - .*", "", .data$name),
          "variable" = gsub(" - [0-9]{4}$", "", gsub("^[A-Za-z0-9 ]* - ", "", .data$name)),
          "year" = as.numeric(gsub("^.* - .* - ", "", .data$name))
        ) %>%
        select("year", "region", "scenario", "maintech", "tech", "variable", "value")

      out <- rbind(out, d)
    }

    as.magpie(out, temporal = 1, spatial = 2, datacol = 7) %>%
      magpiesort() %>%
      return()
  } else {
    stop("Unsupported subtype. Must be either 'outlook' or 'assumptions'")
  }
}

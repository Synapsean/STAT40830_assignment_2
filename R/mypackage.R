#' @title Read and process an Indicator CSV File
#' @param country_code A character string representing the country code (e.g., "irl", "col").
#'   This code is used to construct the filename `indicators_COUNTRYCODE.csv`.
#'@details
#'  This function performs several steps to ensure data quality and usability
#'  \itemize{
#'  \item It uses 'data.table::fread' to read in the large CSV file
#'  \item It automatically skips the first row, which is metadata, important for later functions
#'  \item It uses 'janitor:clean_names' for standardising column names
#'  \item Columns are cleaned using 'stringr::str_squish', ensuring reliable filtering later
#'  \item Numerical columns are coerced to numeric, and character column are coerced to factors.}
#'  The output is given a custom class 'indicator_data' for further S3 methods.
#' @importFrom data.table fread .SD :=
#' @importFrom janitor clean_names
#' @export

#Funtion for reading indicators_COUNTRYCODE.csv
read_country_indicators = function(country_code){
  filename = paste0("indicators_", tolower(country_code), ".csv")
  full_path = system.file("extdata", filename, package = utils::packageName())
  if (!file.exists(full_path)) {
    stop("File not found: '", full_path, "'. Please ensure the file is in ",
         "the correct path ('", path, "') or provide the full path to it.")
  }

  data = data.table::fread(full_path) #CSV file read using data.table fread
  data = data[-1,] #remove the first line (not the header, row 2 in the csv file) which contains the meta data info
  data = janitor::clean_names(data)

  # This ensures factors are built from clean strings, and that all string cols are trimmed/squished.
  string_cols_to_clean = c("country_name", "country_iso3", "indicator_name", "indicator_code")
  for (col in intersect(string_cols_to_clean, names(data))) {
    # Apply trimws and then str_squish for whitespace handling
    data.table::set(data, j = col, value = stringr::str_squish(base::trimws(data[[col]])))
  }

  # Numeric columns
  data.table::set(data, j = "year", value = base::suppressWarnings(base::as.numeric(data[["year"]])))
  data.table::set(data, j = "value", value = base::suppressWarnings(base::as.numeric(data[["value"]])))
  # Factor columns
  data.table::set(data, j = "country_name", value = base::as.factor(data[["country_name"]]))
  data.table::set(data, j = "country_iso3", value = base::as.factor(data[["country_iso3"]]))
  data.table::set(data, j = "indicator_name", value = base::as.factor(data[["indicator_name"]])) # This was the problematic one before
  data.table::set(data, j = "indicator_code", value = base::as.factor(data[["indicator_code"]]))

  #Define the output object as a new class "indicator_data"
  class(data) = c("indicator_data", class(data))

  return(data)
}

#' @title Print Method for indicator_data Class
#' @description Provides a concise overview when an `indicator_data` object is printed in the console. Each unique indicator is printed along with its first and last year recorded
#' @param x An object of class `indicator_data`, e.g data_irl
#' @param ... Further arguments passed to or from other methods
#' @param width Adjust the width of the output to fit the console
#' @importFrom utils head
#' @importFrom data.table as.data.table .N
#' @export

print.indicator_data = function(x, ..., width= NULL){
  data_dt = data.table::as.data.table(x)
  coverage_summary = data_dt[, .(
    start_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    n_observations = .N
  ), by = indicator_name]

  if (!is.null(width)) {
    options(datatable.width = width) # Set custom width if provided
  } else {
    options(datatable.width = getOption("width"))
  }
  print(coverage_summary)
}

#' @title Overall summary of the new the new object of 'indicator_class' using skimr
#' @param object An "indicator_data" object created by read_indicator_data() function
#' @param ... Further arguments passed to or from other methods
#' @description Provides a brief statistical summary of the `indicator_data` object
#' @importFrom skimr skim
#' @export

summary.indicator_data = function(object, ...){
  skim(object)
}



#' @title Summary of unique 'indicator_name' from object 'indicator_data'
#' @description Summarises indicator data into custom size bins,
#'   calculating the mean value per bin for each country. Uses `data.table` for efficient processing.
#' @param object An object of class `indicator_data`.
#' @param bin_size Numeric, the size of the year bins (e.g., 5 for 5-year bins). Defaults to 5.
#' @param indicator_name A character string specifying the exact name of a single indicator
#'   to summarise. If `NULL` (default), the summary will be performed across all indicators
#'   in the dataset.
#' @param ... Not used.
#' @return A `data.table` with binned and summarised data. The output will
#'   include `country_name`, `year_bin_start`, `mean_value_bin`, `n_observations`,
#'   and `year_bin_label`. If `indicator_name` was provided, an `indicator_name`
#'   column will also be present in the output.
#' @importFrom data.table as.data.table set
#' @importFrom stringr str_squish
#' @export

binned_summary.indicator_data = function(object, bin_size = 10, indicator_name = NULL, ...) {

  # data_"country_code" is definitely a data.table
  data_dt = data.table::as.data.table(object)


  # Calculate min_year (using min on the data.table column)
  min_year = min(data_dt[["year"]], na.rm = TRUE)


  #Filter Data for specific indicator_name (if provided) ---
  if (!is.null(indicator_name)) {

    target_indicator_clean = stringr::str_squish(base::tolower(indicator_name))

    # Ensure indicator_name column in data_dt is character
    data_dt$indicator_name_char = as.character(data_dt$indicator_name)

    # Filter using data.table's 'i' argument
    data_dt_filtered = data_dt[(tolower(data_dt$indicator_name_char)) == target_indicator_clean]

    # Remove the temporary char column
    data_dt_filtered[, indicator_name_char := NULL]

    if (nrow(data_dt_filtered) == 0) {
      warning(paste0("No data found for indicator '", indicator_name, "'. Returning empty summary."))
      return(data.table::data.table()) # Return empty data.table, debug check to ensure that 'indicator_name' is present and can be subsetted
    }
  } else {
    data_dt_filtered = data_dt # If no indicator_name, use the full data
  }

  # Create year_bin_start directly (first recorded year)
  data_dt_filtered[, year_bin_start := base::floor((year - min_year) / bin_size) * bin_size + min_year]

  # Define grouping variables
  group_vars = c("country_name", "year_bin_start")
  if (!is.null(indicator_name)) {
    group_vars <- c(group_vars, "indicator_name") # Add indicator_name to grouping if filtered
  }

  # Summarise with data.table [j, by] syntax
  binned_data = data_dt_filtered[, .(
    mean_value_bin = mean(value, na.rm = TRUE),
    n_observations = .N
  ), by = group_vars]

  # Create year_bin_label
  binned_data[, year_bin_label := paste0(year_bin_start, "-", year_bin_start + bin_size - 1)]

  # Arrange the data
  binned_data = binned_data[order(country_name, year_bin_start)]

  return(binned_data)
}

#' @title Plot binned_summary.indicator_data
#' @description Generates a time series plot of mean binned values from the output
#'   of `summary.indicator_data`. This function visualises the aggregated data,
#'   showing the mean value per year bin for each country. i.e. the mean "Arable land (hectares)" between '60-'64 etc
#'
#' @param binned_data A `data.table` or `tibble` returned by `summary.indicator_data`.
#'   It must contain columns like `country_name`, `year_bin_start`, `mean_value_bin`,
#'   and optionally `indicator_name` (if the summary was for a specific indicator or all).
#' @param title An optional character string for the plot title. If `NULL`, a default title is generated.
#' @param ... Not used.
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal facet_wrap element_text
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @export

plot_binned_summary = function(binned_data, title = NULL, ...) {

  plot_data = tibble::as_tibble(binned_data)

  # Check if indicator_name column exists for faceting (if summary for all indicators)
  has_indicator_name <- "indicator_name" %in% base::names(plot_data)

  # Set default title based on whether a specific indicator was summarised
  if (is.null(title)) {
    if (has_indicator_name && length(unique(plot_data$indicator_name)) == 1) {
      title <- paste0("Mean Binned Value for '", unique(plot_data$indicator_name)[1], "' by Country")
    } else if (has_indicator_name && length(unique(plot_data$indicator_name)) > 1) {
      title = "Mean Binned Values for Multiple Indicators by Country"
    }
    else {
      title <- "Mean Binned Values by Country and Year Bin"
    }
  }

  #Create the Plot using ggplot2
  p = ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$year_bin_start, y = .data$mean_value_bin, color = .data$country_name)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = title,
      x = "Year Bin Start",
      y = "Mean Binned Value",
      color = "Country" # Legend title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Facet by indicator_name if present and if there's more than one unique indicator in the data
  # This handles cases where summary was called for multiple indicators
  if (has_indicator_name && base::length(base::unique(plot_data$indicator_name)) > 1) {
    p = p + ggplot2::facet_wrap(~ .data$indicator_name, scales = "free_y", ncol = 2)
  }

  print(p)
  invisible(NULL) # Return NULL invisibly
}

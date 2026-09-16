# Extract the URLs of the pdf and html versions
get_possible_locations <- function(api_response, format = "pdf") {
  if (format == "pdf") {
    locations <- purrr::map(
      1:length(api_response[["oa_locations"]]),
      \(x) {
        purrr::pluck(api_response, "oa_locations", x, "url_for_pdf")
      }
    )
  } else if (format == "html") {
    locations <- purrr::map(
      1:length(api_response[["oa_locations"]]),
      \(x) {
        purrr::pluck(api_response, "oa_locations", x, "url")
      }
    )
  } else {
    cli::cli_abort("The {.arg format} argument must be either 'pdf' or 'html'")
  }

  return(
    locations |>
      purrr::discard(is.null) |>
      unique()
  )
}

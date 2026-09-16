request_url <- function(url){
  # Query the URL to get the final location, resolving redirects
  content <- httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      `Accept` = "application/pdf,text/html,*/*",
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  if(content[["status_code"]] != 200){
    cli::cli_inform(
      c(
        "Unsuccessful with: {.url {location}}",
        "Returned status code {.field {content$status_code}}"
      )
    )
  } else {
    return(content)
  }
}

try_html_location <- function(location){
  # First query the URL to get the final location, resolving redirects
  # If the request fails for any reason, set result as error and ignore
  resolved_location <- purrr::safely(
    \() {
      httr2::request(location) |>
        httr2::req_headers(
          `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
          `Accept` = "application/pdf,*/*"
        ) |>
        httr2::req_perform() |>
        purrr::pluck(url)
    },
    otherwise = "error"
  )()$result
  # For links that resolved somewhere, scrape the html
  if (resolved_location != "error") {
    # Read the html from the page and look for links
    page_html <- rvest::read_html(resolved_location)
    page_links <- page_html |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
    # Form a list of links that look like a pdf download
    # Ignoring links starting with mailto
    detected_pdf_links <- page_links[
      grepl(
        paste0(pdf_links, collapse = "|"),
        page_links,
        ignore.case = TRUE
      ) &
        !grepl("mailto", page_links, ignore.case = TRUE)
    ] |>
      unique()
    # Then try download each of these links
    if (length(detected_pdf_linked >= 1)) {
      for (location in detected_pdf_links) {
        # First extract the https address of the file if needed
        if (grepl("file=", location) == TRUE) {
          location <- sub(".*file=", "", detected_pdf_links)
        }
        # Then request the document and save it to the specified location
        tryCatch(
          {
            file_request <- httr2::request(location) |>
              httr2::req_headers(
                `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
                `Accept` = "application/pdf,*/*"
              ) |>
              httr2::req_perform()
            writeBin(resp_body_raw(file_request), con = file_name)
            break
          },
          error = function(error) {
            message("Failed: ", location, "\n", error$message)
          }
        )
      }
    } else {
      message("No pdf links detected")
    }
  }
}

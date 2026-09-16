try_html_location <- function(location) {
  # First request the URL to get the final location, resolving redirects
  resolved_location <- request_url(location)

  # If the URL did not resolve, error
  if (resolved_location != 200) {

    cli::cli_warn(
      "Could not resolve URL: {.url {location}}"
    )

  } else if (endsWith(resolved_location, ".pdf")) {

    # If the resolved location is a pdf file, try get it
    try_pdf_location(resolved_location)

  } else {

    # Read the html from the page and look for links
    page_html <- rvest::read_html(resolved_location)

    # Extract the links from the html
    page_links <- page_html |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")

    # From this list of links, keep anything that looks like a link to a pdf
    detected_pdf_links <- page_links[
      grepl(
        "pdf",
        page_links,
        ignore.case = TRUE
      ) &
        # Ignoring links starting with mailto
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
            file_request <- httr2::request(location) |>
              httr2::req_headers(
                `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
                `Accept` = "application/pdf,*/*"
              ) |>
              httr2::req_perform()
            writeBin(resp_body_raw(file_request), con = file_name)
          },
        )
      }
    } else {
      message("No pdf links detected")
    }
  }
}

download_pdf <- function(
  doi,
  download_location = "./",
  view_on_completion = TRUE,
  email,
  force = FALSE
) {
  # Create the file name and location for the PDF download
  file_path <- paste0(
    download_location,
    gsub("/", "\\.", doi), # Replace slashes with dots
    ".pdf"
  )

  if (force == FALSE & file.exists(file_path) == TRUE) {
    cli::cli_inform(
      c(
        "File already exists at {.path {file_path}}",
        "If you want to download the file again, set {.arg force = TRUE}"
      )
    )
  }

  # Send the DOI to the API, return the response body
  api_response <- call_oa_api(doi, email = email)

  # From the response body, check if there are URLs for PDF or HTML formats
  possible_pdf_locations <- get_possible_locations(
    data = api_response,
    format = "pdf"
  )
  possible_html_locations <- get_possible_locations(
    data = api_response,
    format = "html"
  )

  download_pdf(
    pdf_locations = possible_pdf_locations,
    html_locations = possible_html_locations
  )
}

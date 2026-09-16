try_pdf_location <- function(location, path = "./", filename = NULL) {
  # Attempt to request the file
  request_content <- request_url(location)

  # Check the content type
  content_type <- httr2::resp_content_type(request_content)

  # If not a pdf, error
  if(content_type != "application/pdf") {
    cli::cli_warn(
      c(
        "Unsuccessful with: {.url {location}}",
        "Returned content type was not pdf: {.field {content_type}}"
      )
    )
  }

  # Put together the file name and output path
  if (is.null(filename)) {
    # If no file name is provided, use the URL
    filename <- tail(strsplit(location, "/")[[1]], n = 1)
  }
  output <- paste0(path, filename)

  # If the returned content is not NULL, try write it
  if (!is.null(request_content)){
    # Write the file
    writeBin(
      httr2::resp_body_raw(request_content),
      con = output
    )

    # Check the write was successful
    if (file.exists(output)) {
      cli::cli_inform("Success! Saved to {.file {output}}")
    } else {
      cli::cli_warn("Could not write to {.file {output}}")
    }
  }
}

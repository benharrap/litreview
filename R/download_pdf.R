download_pdf <- function(
    pdf_locations = NULL,
    html_locations = NULL
  ) {
  # If there are PDF locations, prioritise them
  if (length(pdf_locations >= 1)) {
    for (url in pdf_locations) {
      try_pdf_location(url)
      }
  } else if (html_locations >= 1) {
    for (url in html_locations) {
      try_html_location(url)
    }
  } else {
    cli::cli_alert_warning(
      c(
        "No candidates found for download",
        "You'll have to find a copy yourself :("
      )
    )
  }

  if (file.exists(file_name)) {
    message("Saved to ", file_name)

    if (view_on_completion == TRUE) {
      pdfviewer::view_pdf(file_name)
    }
  } else {
    message("Downloads unsuccessful, no files saved")
    message(
      "Try ",
      cli::style_hyperlink(
        text = content$best_oa_location$url,
        url = content$best_oa_location$url
      )
    )
  }
}

try_pdf_location(location){
  file_request <- httr2::request(location) |>
    httr2::req_headers(
      `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      `Accept` = "application/pdf,text/html,*/*",
    ) |>
    httr2::req_perform()

  writeBin(resp_body_raw(file_request), con = file_name)
}

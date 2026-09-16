call_oa_api <- function(doi, email){
  # Perform the request to check the availability of the DOI
  api_response <- httr2::request(
    paste0(
      "https://api.unpaywall.org/v2/",
      doi,
      "?email=",
      email
    )
  ) |>
    httr2::req_perform()

  return(httr2::resp_body_json(api_response))
}

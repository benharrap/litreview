check_doi_format <- function(doi) {
  if(!is.character(doi)){
    cli::cli_abort(
      c(
        "The DOI must be a character",
        "You provided a {.cls {class(doi)}}"
      )
    )
  } else if(grepl("^10\\.", doi) == FALSE) {
    cli::cli_abort(
      c(
        "DOIs are expected to begin with {.strong 10.XXXX/}",
        "Please check the DOI format and try again"
      )
    )
  } else {
    return(doi)
  }
}

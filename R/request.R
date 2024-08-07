#' @title Request Function for PubChem API
#'
#' @description Constructs a URL for the PubChem API based on the provided parameters.
#' This function used internally by all get_* functions, and users will not typically run this function directly.
#'
#' @param identifier The identifier for the compound.
#' @param namespace The namespace for the identifier (default: 'cid').
#' @param domain The domain for the request (default: 'compound').
#' @param operation The operation to be performed (default: NULL).
#' @param output The desired output format (default: 'JSON').
#' @param searchtype The type of search to be performed (default: NULL).
#' @param options Additional parameters. Currently has no effect on the results.
#'
#' @return A constructed URL for the PubChem API.
#'
#' @importFrom utils URLencode
#'
#' @keywords internal
#' @noRd
request <- function(identifier = NULL, namespace = 'cid', domain = 'compound',
                    operation = NULL, output = 'JSON', searchtype = NULL, options = NULL) {

  # Check for missing identifier
  if (is.null(identifier)) {
    stop("identifier/cid cannot be NULL")
  }

  if (is.numeric(identifier)) {
    identifier <- as.character(identifier)
  }

  # If identifier is a list, join with commas into string
  if (length(identifier) > 1) {
    identifier <- paste(identifier, collapse = ',')
  }

  # Build API URL
  urlid <- NULL
  postdata <- NULL

  if(!is.null(options)){

    options <- paste0("?", paste0(unlist(options), collapse = "&"))
    options <- gsub(" ","", options, fixed = TRUE)
  }

  if (!is.null(namespace) && namespace == 'sourceid') {
    identifier <- gsub("/", ".", identifier)
  }

  api_base <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

  # Ensure the identifier is URL encoded
  urlid <- URLencode(identifier)

  # Adjusting the logic for building the URL components
  # The identifier now comes before the output format
  comps <- Filter(Negate(is.null), list(api_base, domain, searchtype, namespace, urlid, operation, output))
  apiurl <- paste0(paste(comps, collapse = '/'), options)

  # if (length(params) > 0) {
  #   apiurl <- paste0(apiurl, "?", paste(names(params), params, sep = "=", collapse = "&"))
  # }

  # Return the constructed URL
  return(apiurl)
}

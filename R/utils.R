# Defining basic urls

baseurl = "https://io.apps.fao.org/gismgr/api/v1"
dataurl =  "https://io.apps.fao.org/gismgr/api/v1/catalog/workspaces"
downloadurl =  "https://io.apps.fao.org/gismgr/api/v1/download"
queryurl =  "https://io.apps.fao.org/gismgr/api/v1/query"
cubeurl = "https://io.apps.fao.org/gismgr/api/v1/cubes"
ua <- httr::user_agent("http://github.com/goergen95/wapoR")

#' Sign in with API key at FAO GISManager to retrieve access token
#'
#' Function is used internally to sign in and parse the access_token.
#'
#' @param APIkey A length one character containing a user-specific API key.
#'
#' @return An access token.
#' @export wapor_signin
#' @keywords internal
#'
#' @importFrom httr POST content
wapor_signin <- function(APIkey){
  url = login_url = 'https://io.apps.fao.org/gismgr/api/v1/iam/sign-in'
  response = POST(login_url, config = add_headers("X-GISMGR-API-KEY" = APIkey), ua)
  access_token = content(response)$response$accessToken
  return(access_token)
}


#' Helper function to check the status of a response object
#'
#' Parses the error number and message
#'
#' @param res A httr response object.
#'
#' @importFrom httr status_code content http_error
check_status <- function(res){
  if(http_error(res)){
    parsed = content(res)
    stop(
      sprintf("API request failed [%s]\n%s",
              status_code(res),
              parsed$message))
  }
}

#' Helper function to query an URL
#'
#' @param url Character vector of the url
#'
#' @return The parsed content of the response. An error message in cases the
#'   GET request fails
#' @export get_and_parse
#' @keywords internal
#'
#' @importFrom httr GET content
get_and_parse <- function(url){
  response = GET(url, query = list(overview = "false", paged = "false"), ua)
  check_status(response)
  response =  content(response)
  if(!"response" %in% names(response)){
    stop("URL DOES NOT CONTAIN RESPONSE OBJECT")
  }
 response$response
}

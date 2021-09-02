#' List collections at FAO GISManager
#'
#' This function lists available data collections from the FAO GISManager API.
#' Only the code name used for other functions and the caption (e.g.) long name
#' is returned. This function is not equivalent to \code{wapor_collection()}
#' which returns the metadata of a *singular* data collection.
#'
#' @return A data.frame object with code and captions of the available collections.
#' @export wapor_collections
#'
wapor_collections <- function(){
  parsed = get_and_parse(dataurl)
  collections = do.call(rbind, lapply(parsed, function(x){
    as.data.frame(x[c("code", "caption")])
  }))
  collections
}


#' FAo GIS List available Products in collection
#'
#' This function lists all available datasets within a collection. The result
#' is returned as a list where each element represents a distinct product and is
#' named accordingly. One level lower, there are two objects one containing general
#' information on the product and the other one containing additional metadata,
#' such as spatio-temporal resolution, extent in space and time, coordinate system etc.
#'
#' @param collection A character vector of length one specifying the collection
#'   for which to retrieve available products-
#'
#' @return A list object with all available products. One product object contains
#'   of the two data.frames \code{product} and \code{meta} where meta can have
#'   variable column number due to differences in the available meta data.
#' @export wapor_products
#'
#' @importFrom stringr str_remove_all
wapor_products <-function(collection){
  url = paste(dataurl, collection, "cubes", sep = "/")
  parsed = get_and_parse(url)

  products = lapply(parsed, function(x){
    y = as.data.frame(x)
    product = y[, c("code", "caption", "description")]
    meta = y[grep("additional", names(y))]
    names(meta) = str_remove_all(names(meta), "additionalInfo.")
    list(product = product, meta = meta)
  })

  prod_names = unlist(lapply(products, function(x) x$product$code))
  names(products) = prod_names
  products
}


#' FAO GIS get product metadata
#'
#' This function retrieves metadata of a specific product within a collection.
#' The returned object is a three-leveled list with the first object containing
#' some general information on the product. The second object contains information
#' of the dimensions the data set is associated with wile the third element contains
#' additional metadata e.g. on spatio-temporal resolution or the methodology
#' how the product was generated.
#'
#' @param collection A length one character vector specifying the collection.
#' @param product A length one character vector specifying the product..
#'
#' @return A list object with the obhects \code{info}, \code{dimensions}, and \code{meta}.
#' @export wapor_metadata
#'
#' @importFrom stringr str_remove_all
wapor_metadata <- function(collection, product){

  url = paste(dataurl, collection, "cubes", product, "measures", sep = "/")
  parsed = get_and_parse(url)

  info = data.frame(code = parsed[[1]]$code,
                    caption = parsed[[1]]$caption,
                    unit = parsed[[1]]$unit,
                    scale = parsed[[1]]$scale,
                    multiplier = parsed[[1]]$multiplier)


  url = paste(dataurl, collection, "cubes", product, "dimensions", sep = "/")
  parsed = get_and_parse(url)

  dimensions = do.call(rbind, lapply(parsed,  function(x){
    data.frame(code = x$code,
               caption = x$caption,
               type = x$type)
  }))


  url = paste(dataurl, collection, "cubes", product, sep = "/")
  parsed = get_and_parse(url)
  meta =  as.data.frame(parsed)
  meta = meta[grep("additional", names(meta))]
  names(meta) = str_remove_all(names(meta), "additionalInfo.")

  list(info = info, dimensions = dimensions, meta = meta)
}


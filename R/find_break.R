#' Calculate breakpoints
#'
#' @param x a numeric vector
#'
#' @return the index of breakpoint or NA
#' @export find_break
#' @importFrom zoo rollapply
find_break = function(x){
  x = as.numeric(x)
  idNA = which(is.na(x))
  if (length(idNA) >= as.integer(0.75*length(x))){
    return(NA)
  } else {
    origin = sort(table(x[1:3]), decreasing = TRUE)[1] # get the majority class of first three years
    ending = sort(table(x[(length(x)-2):length(x)]), decreasing = T)[1]
    window = zoo::rollapply(x, width = 3, FUN = function(x) as.numeric(sort(table(x), decreasing = TRUE)), fill = c(origin, NA, ending)) # apply rolling median to get the majority class for every time step
    changepoint = which(window  != origin)[1]
  }
  # determine the first change point
  if (is.na(changepoint)){ # check if there was any change
    return(0) # if not return 0
  }else{
    return(changepoint) # if there was return the index of the raster it occured
  }
}

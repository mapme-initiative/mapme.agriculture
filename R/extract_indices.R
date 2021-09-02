#' Extract and summarize index values per polygon
#'
#' This function can be used to extract and summarise the data output by
#' the wapor_indices function on a polygon basis. The data is returned
#' as a list object with several tables in long-format. These objects are:
#'   - Land Cover Statistics (lcc_stats): breakpoint, median land use, modal land use
#'   - Land Cover Frequency (lcc_freq): the averaged frequency of occurence for selected land cover classes
#'   - Indices (indices): Averaged yearly statistics of the raw parameters as well as LP1_global and LP1_yearly
#'   - Double Season: Class specific occurence of double seasons per year
#'   - Double Season Difference: Difference in number of double season in pre- and post epoch
#'   - Epoch Differences: Differences in productivity for raw parameters
#'   - Trends: Estimates and p-values of a linear trend per parameter
#'
#' @param input_files A charachter vector pointing to a complete set of
#'   output files from the wapor_indices() function
#' @param aoi A sf-object with optionally multiple polygons for which to extract
#'   the index information.
#'
#' @return A list of tibble objects. See the decription for details.
#' @export wapor_extract
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr group_by summarise ungroup as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_split str_remove
#' @importFrom terra extract rast vect
wapor_extract <- function(input_files, aoi){

  ID = NULL
  stat = NULL
  value = NULL
  year = NULL

  trend_files = input_files[grep("trend", basename(input_files))]
  trend_values = do.call(rbind, lapply(trend_files, function(x){
    splitted = str_split(basename(x), "-")[[1]]
    class = splitted[1]
    para = splitted[2]
    season = splitted[3]
    indicator = paste0(str_remove(splitted[4], ".tif"), "_trend")
    tmp = rast(x)
    tmp_names = names(tmp)
    values = as_tibble(terra::extract(tmp, vect(aoi)))
    values %<>%
      pivot_longer(cols = 2:ncol(values), names_to = "stat") %>%
      group_by(ID, stat) %>%
      summarise(value = mean(value, na.rm = T)) %>%
      ungroup()
    values$class = class
    values$parameter = para
    values$season = season
    values$indicator = indicator
    values
  }))

  # extract index values
  ind_files = input_files[-grep("trend|lcc|epoch|diff|seasons", basename(input_files))]
  ind_values = do.call(rbind, lapply(ind_files, function(x){
    splitted = str_split(basename(x), "-")[[1]]
    class = splitted[1]
    para = splitted[2]
    season = splitted[3]
    indicator = str_remove(splitted[4], ".tif")
    tmp = rast(x)
    tmp_names = names(tmp)
    layer_year = unlist(lapply(tmp_names, function(y){
      str_split(y, "_")[[1]][5]
    }))
    names(tmp) = layer_year
    values = as_tibble(terra::extract(tmp, vect(aoi)))
    values %<>%
      pivot_longer(cols = 2:ncol(values), names_to = "year") %>%
      group_by(ID, year) %>%
      summarise(value = mean(value, na.rm = T)) %>%
      ungroup()
    values$class = class
    values$parameter = para
    values$season = season
    values$indicator = indicator
    values
  }))


  # double seasons
  dseason_files = input_files[grep("double-seasons.tif", basename(input_files))]
  dseason_values = do.call(rbind, lapply(dseason_files, function(x){
    splitted = str_split(basename(x), "-")[[1]]
    class = splitted[1]
    indicator = "double-season"
    tmp = rast(x)
    tmp_names = names(tmp)
    layer_year = unlist(lapply(tmp_names, function(y){
      str_split(y, "_")[[1]][3]
    }))
    names(tmp) = layer_year
    values = as_tibble(terra::extract(tmp, vect(aoi)))
    values %<>%
      pivot_longer(cols = 2:ncol(values), names_to = "year") %>%
      group_by(ID, year) %>%
      summarise(value = median(value, na.rm = T)) %>%
      ungroup()
    values$class = class
    values$indicator = indicator
    values
  }))

  diffseason_files = input_files[grep("seasons-diff", basename(input_files))]
  diffseason_values = do.call(rbind, lapply(diffseason_files, function(x){
    splitted = str_split(basename(x), "-")[[1]]
    class = splitted[1]
    indicator = "double-season-diff"
    tmp = rast(x)
    values = as_tibble(terra::extract(tmp, vect(aoi)))
    values %<>%
      pivot_longer(cols = 2:ncol(values), names_to = "stat") %>%
      group_by(ID, stat) %>%
      summarise(value = median(value, na.rm = T)) %>%
      ungroup()
    values$class = class
    values$indicator = indicator
    values$stat = NULL
    values
  }))


  # epoch metrics
  epoch_files = input_files[grep("epoch", basename(input_files))]
  epoch_values = do.call(rbind, lapply(epoch_files, function(x){
    splitted = str_split(basename(x), "-")[[1]]
    class = splitted[1]
    para = splitted[2]
    season = splitted[3]
    indicator = "epoch-diffs"
    tmp = rast(x)
    values = as_tibble(terra::extract(tmp, vect(aoi)))
    values %<>%
      pivot_longer(cols = 2:ncol(values), names_to = "stat") %>%
      group_by(ID, stat) %>%
      summarise(value = median(value, na.rm = T)) %>%
      ungroup()
    values$class = class
    values$parameter = para
    values$season = season
    values$indicator = indicator
    values
  }))


  # lcc metrics
  lcc_files = input_files[grep("lcc_freq", basename(input_files))]
  lcc_freq = do.call(rbind, lapply(lcc_files, function(x){
    splitted = str_split(basename(x), "-")[[1]]
    class = splitted[1]
    tmp = rast(x)
    values = as_tibble(terra::extract(tmp, vect(aoi)))
    values %<>%
      pivot_longer(cols = 2:ncol(values), names_to = "stat") %>%
      group_by(ID, stat) %>%
      summarise(value = median(value, na.rm = T)) %>%
      ungroup()
    values$class = class
    values$indicator = "lcc_freq"
    values$stat = NULL
    values
  }))

  lcc_breakpoint = rast(input_files[grep("breakpoint", basename(input_files))])
  breakpoint_values = as_tibble(terra::extract(lcc_breakpoint, vect(aoi)))
  breakpoint_values %<>%
    pivot_longer(cols = 2:ncol(breakpoint_values), names_to = "stat") %>%
    group_by(ID, stat) %>%
    summarise(value = median(value, na.rm = T)) %>%
    ungroup()
  breakpoint_values$indicator = "lcc_breakpoint"
  breakpoint_values$stat = NULL


  lcc_median = rast(input_files[grep("median", basename(input_files))])
  median_values = as_tibble(terra::extract(lcc_median, vect(aoi)))
  median_values %<>%
    pivot_longer(cols = 2:ncol(median_values), names_to = "stat") %>%
    group_by(ID, stat) %>%
    summarise(value = median(value, na.rm = T)) %>%
    ungroup()
  median_values$indicator = "lcc_median"
  median_values$stat = NULL

  lcc_modal = rast(input_files[grep("modal", basename(input_files))])
  modal_values = as_tibble(terra::extract(lcc_modal, vect(aoi)))
  modal_values %<>%
    pivot_longer(cols = 2:ncol(modal_values), names_to = "stat") %>%
    group_by(ID, stat) %>%
    summarise(value = terra::modal(value, na.rm = T)) %>%
    ungroup()
  modal_values$indicator = "lcc_modal"
  modal_values$stat = NULL

  lcc_stats = do.call(rbind, list(breakpoint_values, median_values, modal_values))


  list(lcc_stats = lcc_stats,
       lcc_freq = lcc_freq,
       indices = ind_values,
       double_seasons = dseason_values,
       double_seasons_diff = diffseason_values,
       epoch_diffs = epoch_values,
       trends = trend_values)

}

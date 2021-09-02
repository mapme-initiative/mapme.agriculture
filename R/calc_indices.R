#' Calculate WaPOR productivity indices
#'
#' This function calculates several productivity indices based on WaPOR inputs.
#' Users can specify a vector of classes for which to calculate the metrics.
#' From the land cover classification mask layers are calculated and the
#' indices are returned seperatly for each class. The function writes raster
#' files with the calculated indices to the specified output directory.
#' The calculated indices are:
#'   - Land Use Median (lcc_median)
#'   - Land Use Mode (lcc_mode)
#'   - Land Use Frequency (lcc_freq)
#'   - Breakpoint year of land use change (lcc_break)
#'   - Global productivity index (LP1_global)
#'   - Yearly productivity index (LP2_yearly)
#'   - Epoch differences (epoch_diff)
#'   - Linear trends (trend)
#'   - Number of double seasons (double_season)
#'
#' Where applicable, above mentioned metrics are calculated for Net Biomass
#' Water Productivity (NBWP), Gross Biomass Water Productivity (GBWP), Total
#' Biomass Production (TBP) and the productivity indices seperatly. This
#' will be denoted in the resulting filename that are structured according to:
#' {int}-{measurment}-{season}-{index}.tif. E.g. the filename for land use class 30 the trend
#' of NBWP for the first season looks like this: "30-nbwp-s1-raw-trend.tif"
#'
#' @param input_files A character vector pointing to the raster files of Land
#'  cover classification (LCC), Total Biomass Productivity (TBP), Net Biomass
#'  Water Productivity (NBWP), Gross Biomass Water Produtivity (GBWP).
#' @param years The years for which to calculate the indices.
#' @param mask_values A numeric vector indicatiing the class codes for which to
#'   calculate the indices.
#' @param ncores The number of cores used for parallel processing of some
#'   index calculations.
#' @param epoch1 A vector of years representing the first epoch for epoch comparisons.
#' @param epoch2 A vector of years representing the second epoch for epoch comparisons.
#' @param outdir A charachter vector pointing to an output directory.
#' @param overwrite Logical wether to overwrite existing raster files.
#' @param verbose A logical indicating if informative messages should be printed.
#' @return A character vector pointing to the output files.
#' @export wapor_indices
#' @importFrom terra rast classify modal median nlyr values app mean writeRaster
#' @importFrom stats quantile
#' @importFrom trend sens.slope
wapor_indices <- function(
  input_files = NULL,
  years,
  mask_values,
  ncores = 1,
  epoch1 = 2010:2011,
  epoch2 = 2012:2014,
  outdir,
  overwrite = TRUE,
  verbose = T
){

  if(!dir.exists(outdir)) dir.create(outdir, showWarnings = F)
  # separate input files
  input_files = input_files[grep(paste(years, collapse = "|"), input_files)]
  lcc_files = input_files[grep("LCC", input_files)]
  tbp_files = input_files[grep("TBP", input_files)]
  nbwp_files = input_files[grep("NBWP", input_files)]
  gbwp_files = input_files[grep("GBWP", input_files)]


  # get lcc raster for each value in mask_values
  if(verbose){message("Starting to mask LCC...")}
  lcc_stack = rast(lcc_files)
  if(is.null(mask_values)){
    lcc_masks = lcc_stack
    lcc_masks[] = 1
    lcc_masks = list(lcc_masks)
    names(lcc_masks) = "all"
    mask_values = "all"
  } else {
    lcc_masks = lapply(mask_values, function(val){
      m = rbind(c(val, 1))
      classify(lcc_stack, m, othersNA = TRUE)
    })
    names(lcc_masks) = mask_values
  }
  # read and mask other data sets for each of the mask layers
  masked_data =
    lapply(lcc_masks, function(mask){
      lapply(list(tbp = tbp_files,
                  gbwp = gbwp_files,
                  nbwp = nbwp_files),
             function(x){
               s1 = rast(x[grep("S1",x)])
               s1 = mask(s1, mask)
               s2 = rast(x[grep("S2",x)])
               s2 = mask(s2, mask)
               # pre-process of TBP
               if(length(grep("TBP", x)>0)) {
                 s1 = classify(s1, rcl = cbind(-9996, NA))
                 s2 = classify(s2, rcl = cbind(-9996, NA))
                 s1 = s1 * 10
                 s2 = s2 * 10
               }

               # pre-process of GBWP
               if(length(grep("GBWP", x)>0)) {
                 s1 = classify(s1, rcl = cbind(-9999, NA))
                 s2 = classify(s2, rcl = cbind(-9999, NA))
                 s1 = s1 * 0.001
                 s2 = s2 * 0.001
               }
               list(s1 = s1, s2 = s2)
             })
    })

  ###############################################################################
  # WP-1 Changes in land use / land cover
  if(verbose){message("Calculation of LCC metrics...")}
  lcc_median = median(lcc_stack)
  lcc_mode = modal(lcc_stack)
  lcc_freq = lapply(lcc_masks, function(mask){
    mask[!is.na(mask)] = 1
    sum(mask, na.rm = T)
  })
  names(lcc_freq) = mask_values
  breakpoint = T
  if(nlyr(lcc_stack)<=3){
    warning("Only three or less years specified. Cannot conduct breakpoint detection.")
    breakpoint = F
  }

  if(breakpoint){
    lcc_breakpoint = app(lcc_stack, fun = find_break, cores = ncores)
  }
  ###############################################################################
  # WP-2 Land Productivity
  # LP-1 Global Land productivity (Number of years a pixel falls above global 95% value)
  if(verbose){message("Calculation to mask global productivity metric...")}
  lp1_global =
    lapply(masked_data, function(class){
      lapply(class, function(para){
        lapply(para, function(season){
          q95 = as.numeric(quantile(values(season), probs = .95, na.rm = T))
          season  / q95
        })
      })
    })

  # LP-2 Yearly Land productivity (Number of years a pixel falls above yearly 95% value)
  if(verbose){message("Calculation to mask yearly productivity metric...")}
  lp1_yearly =
    lapply(masked_data, function(class){
      lapply(class, function(para){
        lapply(para, function(season){
          for(i in 1:nlyr(season)){
            q95 = as.numeric(quantile(values(season[[i]]), probs = .95, na.rm = T))
            season[[i]] = season[[i]]  / q95
          }
          season
        })
      })
    })

  # Epoch comparisons of productivity
  if(verbose){message("Calculation of epoch differences...")}
  epoch_diffs = lapply(masked_data, function(class){
    lapply(class, function(para){
      lapply(para, function(season){
        e1 = season[[grep(paste(epoch1, collapse="|"), names(season))]]
        e2 = season[[grep(paste(epoch2, collapse="|"), names(season))]]
        e1_avg = mean(e1, na.rm = T)
        e2_avg = mean(e2, na.rm = T)
        e1_med = median(e1, na.rm = T)
        e2_med = median(e2, na.rm = T)
        diff_avg = e2_avg - e1_avg
        diff_med = e2_med - e1_med
        c(diff_avg, diff_med)
      })
    })
  })

  # Trend analysis for productivity metrics and LPs
  if(verbose){message("Calculation of trends. This could take a while...")}
  trend_data = lapply(list(masked_data, lp1_global, lp1_yearly), function(dataset){
    lapply(dataset, function(class){
      lapply(class, function(para){
        lapply(para, function(season){
          out = app(season, fun = function(x){
            idNA = which(is.na(x))
            if(length(idNA) != 0) {
              if(length(idNA) >= as.integer(0.75 * length(x))){
                return(c(NA,NA))
              } else {
                x = x[-idNA]
              }
            }
            results = trend::sens.slope(x, conf.level = 0.95)
            c(results$estimates, results$p.value)
          }, cores = ncores)
          names(out) = c("estimate", "pvalue")
          out
        })
      })
    })
  })
  names(trend_data) = c("raw", "lp1_global", "lp1_yearly")

  # identify pixels with double season
  if(verbose){message("Double season identification...")}
  double_seasons = lapply(masked_data, function(class){
    double_years = lapply(1:nlyr(class$tbp$s1), function(i){
      season = c(class$tbp$s1[[i]], class$tbp$s2[[i]])
      season = sum(season)
      season[!is.na(season)] = 1
      season
    })
    double_years = do.call(c, double_years)
    names(double_years) = paste("double_season_", years, sep = "")
    double_years
  })

  # difference in number of seasons between epochs
  if(verbose){message("Calculation differences in season numbers...")}
  diff_seasons = lapply(double_seasons, function(class){
    e1 = class[[grep(paste(epoch1, collapse="|"), names(class))]]
    e2 = class[[grep(paste(epoch2, collapse="|"), names(class))]]
    e1 = median(e1, na.rm =T)
    e2 = median(e2, na.rm =T)
    e2 - e1
  })


  # writing output
  if(verbose){message("Writting output to disk...")}
  dir.create(outdir, showWarnings = F)
  for(class in names(masked_data)){
    for(para in names(masked_data[[class]])){
      for(season in names(masked_data[[class]][[para]])){
        writeRaster(masked_data[[class]][[para]][[season]],
                    filename = file.path(outdir, paste0(paste(class, para, season, "raw", sep ="-"), ".tif")),
                    overwrite=overwrite
        )
      }
    }
  }

  writeRaster(lcc_median, filename = file.path(outdir, "lcc_median.tif"), overwrite = overwrite)
  writeRaster(lcc_mode, filename = file.path(outdir, "lcc_modal.tif"), overwrite = overwrite)
  for(class in names(lcc_freq)){
    writeRaster(lcc_freq[[class]], filename = file.path(outdir, paste0(class,"-lcc_freq.tif")), overwrite=overwrite)
  }
  if(breakpoint){
    writeRaster(lcc_breakpoint, filename = file.path(outdir, "lcc_breakpoint.tif"), overwrite = overwrite)
  }

  for(class in names(lp1_global)){
    for(para in names(lp1_global[[class]])){
      for(season in names(lp1_global[[class]][[para]])){
        writeRaster(lp1_global[[class]][[para]][[season]],
                    filename = file.path(outdir, paste0(paste(class, para, season, "lp1_global", sep ="-"), ".tif")),
                    overwrite=overwrite
        )
      }
    }
  }

  for(class in names(lp1_yearly)){
    for(para in names(lp1_yearly[[class]])){
      for(season in names(lp1_yearly[[class]][[para]])){
        writeRaster(lp1_yearly[[class]][[para]][[season]],
                    filename = file.path(outdir, paste0(paste(class, para, season, "lp1_yearly", sep ="-"), ".tif")),
                    overwrite=overwrite
        )
      }
    }
  }

  for(class in names(epoch_diffs)){
    for(para in names(epoch_diffs[[class]])){
      for(season in names(epoch_diffs[[class]][[para]])){
        writeRaster(epoch_diffs[[class]][[para]][[season]],
                    filename = file.path(outdir, paste0(paste(class, para, season, "epoch_diffs", sep ="-"), ".tif")),
                    overwrite=overwrite
        )
      }
    }
  }

  for(dataset in names(trend_data)){
    for(class in names(trend_data[[dataset]])){
      for(para in names(trend_data[[dataset]][[class]])){
        for(season in names(trend_data[[dataset]][[class]][[para]])){
          writeRaster(trend_data[[dataset]][[class]][[para]][[season]],
                      filename = file.path(outdir, paste0(paste(class, para, season, dataset, "trend", sep ="-"), ".tif")),
                      overwrite=overwrite
          )
        }
      }
    }
  }

  for(class in names(double_seasons)){
    writeRaster(double_seasons[[class]], filename = file.path(outdir, paste0(class, "-double-seasons.tif")), overwrite=overwrite)
  }

  for(class in names(diff_seasons)){
    writeRaster(diff_seasons[[class]], filename = file.path(outdir, paste0(class, "-double-seasons-diff.tif")), overwrite=overwrite)
  }
  if(verbose){message("Done!")}
  list.files(outdir, pattern = ".tif", full.names = TRUE)
}


#' Calculate Net Biomass Water Productivity
#'
#' This function is used to calculate the seasonal net water productivity
#' based on the phenologocial information (PHE) and the seasonal total
#' biomass production (TBP). Additionally, a LCC land use mask can be used
#' to restrict the analysis to pixels of a certain class. The function writes
#' the seasonal total sum of transpiration (T_S) and the net biomass productivity
#' (NBWP) to respective directories in the output directory.
#'
#' @param input_files A character vector with the file paths to the seasonal
#'   phenological metrics (PHE), the decadal transpiration (T), the seasonal
#'   total biomass production (TBP) and possibly the annual land cover classification (LCC).
#' @param years The years for which to calculate NBWP as a numeric vector. Note,
#'   NBWP can be calculated only for years for which data of a previous and a
#'   subsequent years are available. Usually that means that the earliest possible
#'   starting year is 2010 because data availability starts in 2009. The latest
#'   possible year for that NBWP can be calculated is usually two years
#'   before the current year.
#' @param aoi An optional sf object that can be used to set the calculation of
#'   NBWP to its bounding box.
#' @param verbose A logical indicating if informative messages should be printed.
#' @param outdir A character vector indicating the directory where the output shall
#'   be written to. The function will create two new sub-directories (T_S and NBWP)
#'   and overwrite potentially already available files in these sub-directories.
#'
#' @return A character vector with the file locations of the NBWP rasters.
#' @export wapor_nbwp
#' @importFrom sf st_bbox st_crs st_transform
#' @importFrom terra rast writeRaster classify rapp
wapor_nbwp <- function(input_files = NULL,
                       years = NULL,
                       aoi = NULL,
                       verbose = TRUE,
                       outdir = "."
){

  if(years[1]<2010){
    stop("Start year is smaller than 2010. Cannot calculate metrics before
         the year 2010.")
  }

  if(years[length(years)] >= (as.numeric(format(Sys.Date(), "%Y")) -1 )){
    stop("The last year should be at least one year before the current date.")
  }

  if(!is.null(aoi)){
    if(st_crs(4326) != st_crs(aoi)){
      if(verbose) message("CRS of aoi is different from cube WGS84. Trying to reproject")
      aoi = st_transform(aoi, st_crs(436))
    }
  }

  for (year in years){

    if(verbose) message(sprintf("Starting calculation for year %s.", year))
    # prepare year vectors
    pre_year = year - 1
    post_year = year + 1
    # split input files
    phe_files = input_files[grep("_PHE_S", input_files)]
    # lcc_files = input_files[grep("_LCC_A", input_files)]
    t_files = input_files[grep("_T_D", input_files)]
    tbp_files = input_files[grep("_TBP_S", input_files)]

    phe_files = phe_files[grep(year, phe_files)]
    # lcc_files = lcc_files[grep(year, lcc_files)]
    tbp_files = tbp_files[grep(year, tbp_files)]
    t_files = t_files[grep(paste(c(pre_year,year,post_year), collapse = "|"), t_files)]
    if(length(t_files) > 108) stop("More than 108 decadal layers showed up as transpiration files.")
    if(verbose) message("Starting to mask raster files according to mask values...")
    # lcc = rast(lcc_files)
    # rcl = do.call(rbind, lapply(mask_values, function(val) c(val, val)))
    # lcc = classify(lcc, rcl = rcl, othersNA=TRUE)
    # lcc_mask = is.na(lcc)

    phe = rast(phe_files[grep("SOS|EOS", phe_files)]) # only grep SOS and EOS
    # phe = mask(phe, lcc_mask, maskvalues = 1)
    phe = classify(phe, cbind(251, NA)) # out of season value
    tbp = rast(tbp_files)
    # tbp = mask(tbp, lcc_mask, maskvalues = 1)
    tbp = classify(tbp, cbind(-9996, NA)) # no data value
    tbp = tbp * 10 # tbp unit is now kg/ha
    trans = rast(t_files)
    # trans = mask(trans, lcc_mask, maskvalues = 1)
    # trans = trans * 0.1 # transpiration unit is now mm/day for each decad

    dir.create(file.path(outdir, "T_S"), showWarnings = F)
    for(s in c("S1", "S2")){
      if(verbose) message(sprintf("Starting calculation of seasonal transpiration for season %s...", s))
      if(file.exists(file.path(outdir, "T_S" , paste("L2_T_S", s, year, "SEASON.tif", sep = "_")))) next
      sos = phe[[grep(paste0(s, "_SOS"), names(phe))]]
      eos = phe[[grep(paste0(s, "_EOS"), names(phe))]]
      t_seasonal = rapp(trans, first = sos, last = eos, fun = sum)
      # t_seasonal = mask(t_seasonal, lcc_mask)
      writeRaster(t_seasonal, filename = file.path(outdir, "T_S" , paste("L2_T_S", s, year, "SEASON.tif", sep = "_")))
      rm(sos, eos, t_seasonal); gc()
    }
    rm(trans,phe); gc()

    if(verbose) message("Starting calculation of seasonal NBWP...")
    dir.create(file.path(outdir, "NBWP"), showWarnings = F)
    ts_files = list.files(file.path(outdir, "T_S"), full.names = T, pattern = as.character(year))
    tseason = rast(ts_files)
    names(tseason) = c("S1", "S2")
    nbwp = (tbp/10000) / (tseason/1000) # to kg/m³
    names(nbwp) = c(sprintf("L2_NBWP_S_S1_%s_SEASON", year), sprintf("L2_NBWP_S_S2_%s_SEASON", year))
    nbwp = classify(nbwp, cbind(Inf, NA))
    for(i in 1:nlyr(nbwp)){
      if(file.exists(file.path(outdir, "NBWP" , sprintf("L2_NBWP_S_S%s_%s_SEASON.tif", i, year)))) next
      writeRaster(nbwp[[i]], filename = file.path(outdir, "NBWP" , sprintf("L2_NBWP_S_S%s_%s_SEASON.tif", i, year)))
    }
  }
  list.files(file.path(outdir, "NBWP"), full.names = T)
}

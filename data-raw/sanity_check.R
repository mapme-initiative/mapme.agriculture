library(terra)
cubes_nbwp = rast(list.files("inst/extdata/NBWP/", pattern = "S1", full.names = T))

phe_sos = rast(list.files("inst/extdata/PHE/", pattern = "S1_SOS", full.names = T))
phe_eos = rast(list.files("inst/extdata/PHE/", pattern = "S1_EOS", full.names = T))
lcc = rast(list.files("inst/extdata/LCC/", full.names = T))
trans = rast(list.files("inst/extdata/T/", full.names = T)) * 0.1
tbp = rast(list.files("inst/extdata/TBP/", full.names = T)) * 10

sos_2010 = phe_sos$L2_PHE_S_S1_SOS_2010_YEAR
eos_2010 = phe_eos$L2_PHE_S_S1_EOS_2010_YEAR
trans_2010 = trans[[1:108]]
tbp_2010 = tbp$L2_TBP_S_S1_2010_YEAR

test = c(trans_2010, sos_2010, eos_2010)

trans_s1 = app(test, fun = function(x){
  vals = x[(x[109]):(x[110])]
  sum(vals*10)
})

gbwp_2010 = rast("inst/GBWP/L2_GBWP_S_S1_2010_YEAR.tif") * 0.001
nbwp_2010 = (tbp_2010/10000) / (trans_s1/1000)
mask_layer = lcc$L2_LCC_A_2010_YEAR
mask_layer[!mask_layer %in% c(20,30)] = NA
nbwp_2010[is.na(mask_layer)] = NA
gbwp_2010[is.na(mask_layer)] = NA
plot(nbwp_2010)
plot(gbwp_2010)
plot(cubes_nbwp$NBWP_S1_2010_SEASON)
cubes_nbwp$NBWP_S1_2010_SEASON*1

##
## Create UK-wide shapefiles/geojsons for Local Resilience Forums and devolved equivalents
## - England & Wales: Local Resilience Forum (LRF)
## - Scotland: Local Resilience Partnership (LRP)
## - Northern Ireland: Emergency Preparedness Group (EPG)
##
library(tidyverse)
library(sf)
library(rmapshaper)

source("urls and filenames.r")


##
## Load files
##
lrf_hi = read_sf(url_ew_hi)
lrf_lo = read_sf(url_ew_lo)
lad_hi = read_sf(url_lad_hi)
lad_lo = read_sf(url_lad_lo)
lgd    = read_sf(url_lgd)

# load lookup files for Scotland and NI
ni_lgd_epg_lookup = read_csv(file_ni_lgd_epg_lookup)
sco_lad_lrp_lookup = read_csv(file_sco_lad_lrp_lookup)


##
## create polygons for Scotland's LRPs 
##
# get LRP names for each Local Authority in Scotland
lad_hi_sco = lad_hi %>% 
  filter(startsWith(lad19cd, "S")) %>% 
  left_join(sco_lad_lrp_lookup, by = c("lad19cd" = "LACode"))

lad_lo_sco = lad_lo %>% 
  filter(startsWith(lad19cd, "S")) %>% 
  left_join(sco_lad_lrp_lookup, by = c("lad19cd" = "LACode"))

# combine Local Authorities into LRPs
lrp_hi = lad_hi_sco %>% 
  group_by(LRPName) %>% 
  summarise(st_areashape = sum(st_areashape)) %>%  # arbitrary variable for summarising to combine polygons
  ungroup() %>% 
  
  # create a dummy code for LRPs
  mutate(LRFCode = paste0("S", row_number()))

lrp_lo = lad_lo_sco %>% 
  group_by(LRPName) %>% 
  summarise(st_areashape = sum(st_areashape)) %>%  # arbitrary variable for summarising to combine polygons
  ungroup() %>% 
  
  # create a dummy code for LRPs
  mutate(LRFCode = paste0("S", row_number()))

# DEBUG: check plots
# plot(lad_lo_sco$geometry)
# plot(lrp_lo$geometry)


##
## create polygons for Northern Ireland's EPGs
##
epg = lgd %>% 
  # get EPG names from the lookup table
  left_join(ni_lgd_epg_lookup, by = "LGDCode") %>% 
  
  # combine Local Government Districts into EPGs
  group_by(EPG) %>% 
  summarise(AREA = sum(AREA)) %>%  # arbitrary variable for summarising to combine polygons
  ungroup() %>% 
  
  # create a dummy code for EPGs
  mutate(LRFCode = paste0("NI", row_number()))

# DEBUG: check plots
# plot(lgd$geometry)
# plot(epg$geometry)


##
## keep only names and codes for LRFs (and equivalents) so combining the sf objects will work
##
lrf_hi = lrf_hi %>% select(LRFCode = lrf17cd, LRFName = lrf17nm)
lrf_lo = lrf_lo %>% select(LRFCode = lrf17cd, LRFName = lrf17nm)

lrp_hi = lrp_hi %>% select(LRFCode, LRFName = LRPName)
lrp_lo = lrp_lo %>% select(LRFCode, LRFName = LRPName)

epg = epg %>% select(LRFCode, LRFName = EPG)


##
## create hi-res map
##
uk_lrf_hi = rbind(lrf_hi, lrp_hi, epg)

# plot(uk_lrf_hi$geometry)


##
## create lo-res map
##
# simplify polygons for NI
epg_lo = ms_simplify(epg, keep = 0.01, keep_shapes = TRUE)

uk_lrf_lo = rbind(lrf_lo, lrp_lo, epg_lo)

# plot(uk_lrf_lo$geometry)


##
## save shapefiles and geojsons
##
write_sf(uk_lrf_hi, paste0(output_file, "_high_res.shp"))
write_sf(uk_lrf_hi, paste0(output_file, "_high_res.geojson"))

write_sf(uk_lrf_lo, paste0(output_file, "_low_res.shp"))
write_sf(uk_lrf_lo, paste0(output_file, "_low_res.geojson"))

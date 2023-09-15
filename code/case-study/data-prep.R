# data-prep.R: this script prepares the Hubbard Brook data for analysis 
#              and extracts precipitation and temperature data from PRISM
# Author: Jeffrey W. Doser
rm(list = ls())
library(tidyverse)
library(spOccupancy)
library(sf)
library(stars)
library(prism)

# Load HBEF data from spOccupancy ----------------------------------------
data(hbefTrends)
# Extract data set for one species of interest
curr.sp <- which(dimnames(hbefTrends$y)[[1]] == 'OVEN')
data.list <- hbefTrends
data.list$y <- data.list$y[curr.sp, , , ]
# CRS for the HBEF data
my.crs <- st_crs("+proj=utm +zone=19 +units=m +datum=NAD83")
J <- nrow(data.list$coords)
n.years <- dim(data.list$y)[2]
coords.sf <- st_as_sf(x = as.data.frame(data.list$coords), 
		      coords = c('X', 'Y'), 
		      crs = my.crs)

# Get Temperature and Precipitation data from PRISM -----------------------
# Set PRISM directory -----------------
prism_set_dl_dir("data/prism")
tmax.vals <- matrix(NA, J, n.years)
ppt.vals <- matrix(NA, J, n.years)
years <- 2010:2018 
for (i in 1:n.years) {
  curr.year <- years[i]
  print(paste("Currently on year ", years[i], sep = ''))
  # TMAX ------------------------------
  get_prism_monthlys(type = "tmax", year = curr.year, mon = 5:6, keepZip = FALSE)
  # Get file name to data of interest
  tmax.curr.path.5 <- prism_archive_subset("tmax", "monthly", years = curr.year, mon = 5)
  tmax.curr.path.6 <- prism_archive_subset("tmax", "monthly", years = curr.year, mon = 6)
  # Get absolute file path
  tmax.curr.abs.5 <- pd_to_file(tmax.curr.path.5)
  tmax.curr.abs.6 <- pd_to_file(tmax.curr.path.6)
  # Download the raster
  tmax.curr.5 <- read_stars(tmax.curr.abs.5)
  tmax.curr.6 <- read_stars(tmax.curr.abs.6)
  # Take the average value across the two months 
  tmax.all <- st_apply(c(tmax.curr.5, tmax.curr.6, along = 3), c('x', 'y'), mean)
  coords.proj <- coords.sf %>%
    st_transform(crs = st_crs(tmax.curr.5))
  tmp <- st_extract(tmax.all, at = coords.proj, FUN = mean, na.rm = TRUE)
  # Save the mean values in the list
  tmax.vals[, i] <- tmp[[1]]
  # Precipitation ---------------------
  get_prism_monthlys(type = "ppt", year = curr.year, mon = 5:6, keepZip = FALSE)
  # Get file name to data of interest
  ppt.curr.path.5 <- prism_archive_subset("ppt", "monthly", years = curr.year, mon = 5)
  ppt.curr.path.6 <- prism_archive_subset("ppt", "monthly", years = curr.year, mon = 6)
  # Get absolute file path
  ppt.curr.abs.5 <- pd_to_file(ppt.curr.path.5)
  ppt.curr.abs.6 <- pd_to_file(ppt.curr.path.6)
  # Download the raster
  ppt.curr.5 <- read_stars(ppt.curr.abs.5)
  ppt.curr.6 <- read_stars(ppt.curr.abs.6)
  # Sum the total precipitation across the ttwo months 
  ppt.all <- st_apply(c(ppt.curr.5, ppt.curr.6, along = 3), 
        	       c('x', 'y'), sum)
  coords.proj <- coords.sf %>%
    st_transform(crs = st_crs(ppt.curr.5))
  tmp <- st_extract(ppt.all, at = coords.proj, FUN = mean, na.rm = TRUE)
  # Save the mean values in the list
  ppt.vals[, i] <- tmp[[1]]
}

# Format everything together for spOccupancy ------------------------------
data.list$occ.covs$tmax <- tmax.vals
data.list$occ.covs$ppt <- ppt.vals

# Save to hard drive ------------------------------------------------------
save(data.list, file = 'data/hbef-spOccupancy-data.rda')

# This code downloads mean temp of driest quarter 
# across wbb range from 1998-2020. Data from GRIDMET.
# driest quarter defined as least total precip in Q1/2/3/4
# mean temp is mean of mean daily temp during Q1/2/3/4 as selected above
# Study area is defined using uploaded shapefile.

# install.packages('rgee')

# Authenticate the GEE environment
library(rgee)
ee_Initialize(drive = T)

# Import reticulate
library(reticulate)

# Install and run geetools
# py_install("geetools")
gt <- import("geetools")

# Create clipping function
clipper <- function(image) {
  return(image$clip(polygon)) 
}

# Load the output area shp file (uploaded to GEE as asset)
polygon <- ee$FeatureCollection("users/bermane/wbb_conus_10km_buffer")

# Set start date and define years of data wanted
start <- ee$Date$fromYMD(1998, 1, 1)
years <- ee$List$sequence(0, 22)

# run function to create list of start dates
startDates <- years$map(ee_utils_pyfunc(function (d) {return(start$advance(d, 'year'))}))

# Define name of variables wanted from gridmet dataset
var_precip <- 'pr'
var_max <- 'tmmx'
var_min <- 'tmmn'

# Create sequence of first month of each quarter
quarters <- ee$List(c(1,4,7,10))

# mean temp function for GRIDMET. changes Kelvin to Celsius
calc_mean_temp <- function(image) {
  mean_temp <- image$select(var_max)$add(image$select(var_min))$
    divide(2)$subtract(273.15)$rename('mean_temp')
  return(image$addBands(mean_temp))
}

# Define function to calculate total quarterly precip
calc_tot_precip <- function(y){
  
  # Load start date
  start <- ee$Date(y)
  
  # Create end date, in this case running function for whole year
  end <- ee$Date(y)$advance(1, 'year')
  
  dataset <- ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
    filter(ee$Filter$date(start, end))$
    map(clipper)
  
  # Calculate total precip of each quarter
  tot_precip <- ee$ImageCollection$fromImages(
    quarters$map(ee_utils_pyfunc(function (m) {
      return(dataset$filter(ee$Filter$calendarRange(m, ee$Number(m)$add(2), 'month'))$
               filterBounds(polygon)$
               select(var_precip)$sum()$
               set('quarter', m))
      })))
  
  # Stack bands from img collections
  tot_precip <- tot_precip$toBands()$reproject('EPSG:5070', NULL, 10000)
  
  # Rename bands
  tot_precip <- tot_precip$rename(c('Q1', 'Q2', 'Q3', 'Q4'))
  
  # Set year id
  tot_precip <- tot_precip$set('out_name', ee$String('total_precip_quarter_')$
                                 cat(start$format('YYYY')))
  
  return(tot_precip)
}

# Define function to calculate mean quarterly temp
calc_mean_qtemp <- function(y){
  
  # Load start date
  start <- ee$Date(y)
  
  # Create end date, in this case running function for whole year
  end <- ee$Date(y)$advance(1, 'year')
  
  dataset <- ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
    filter(ee$Filter$date(start, end))$
    map(clipper)
  
  # Caluclate mean temp band
  dataset <- dataset$map(calc_mean_temp)
  
  # Calculate mean temp of each quarter
  mean_temp <- ee$ImageCollection$fromImages(
    quarters$map(ee_utils_pyfunc(function (m) {
      return(dataset$filter(ee$Filter$calendarRange(m, ee$Number(m)$add(2), 'month'))$
               filterBounds(polygon)$
               select('mean_temp')$mean()$
               set('quarter', m))
    })))
  
  # Stack bands from img collections
  mean_temp <- mean_temp$toBands()$reproject('EPSG:5070', NULL, 10000)
  
  # Rename bands
  mean_temp <- mean_temp$rename(c('Q1', 'Q2', 'Q3', 'Q4'))
  
  # Set year id
  mean_temp <- mean_temp$set('out_name', ee$String('mean_temp_quarter_')$
                               cat(start$format('YYYY')))
  
  return(mean_temp)
}

# Run functions over all years
img_tot_precip <- startDates$map(ee_utils_pyfunc(calc_tot_precip))
# print('list of tot precip layers', img_tot_precip);

img_mean_temp <- startDates$map(ee_utils_pyfunc(calc_mean_qtemp))
# print('list of mean temp layers', img_mean_temp);

# Change list of images to image collection
img_tot_precip <- ee$ImageCollection(img_tot_precip)
# print('img collection of tot precip layers', img_tot_precip);

img_mean_temp <- ee$ImageCollection(img_mean_temp)
# print('img collection of mean temp layers', img_mean_temp);

# use geetools to export all images to drive
# you will still have to download the files to local disk manually
# but this method at least preserves file names
#otherwise try function annotated below to download directly to disk
# CRS is now set throughout the code, look for "reproject"

task_img1 <- gt$batch$Export$imagecollection$toDrive(
  collection = img_tot_precip,
  folder = 'gee_tot_precip_q',
  region = polygon$geometry(),
  namePattern = '{out_name}',
  scale = 10000
)

task_img2 <- gt$batch$Export$imagecollection$toDrive(
  collection = img_mean_temp,
  folder = 'gee_mean_temp_q',
  region = polygon$geometry(),
  namePattern = '{out_name}',
  scale = 10000
)

# Batch download imagecollection to disk
# ee_imagecollection_to_local(ic = img_collect,
#                             region = polygon$geometry(),
#                             dsn = file.path(getwd(), '{out_name}'),
#                             via = 'drive',
#                             container = 'gee_max_mon_temp_rel_hist',
#                             scale = 10000,
#                             metadata = FALSE)
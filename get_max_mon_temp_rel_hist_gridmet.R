# This code downloads maximum mean monthly temperature relative to historic 
# across wbb range from 1998-2020. Data from GRIDMET.
# historic defined as mean of monthly max temperature from 1979-1997
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

######################################
###CALCULATE HISTORICAL MEANS FIRST###
######################################

# Define years of historical data 1979-1997 
start <- ee$Date$fromYMD(1979, 1, 1)
end <- ee$Date$fromYMD(1998, 1, 1)

# Load the GRIDMET data that is being processed
# Filter by years of historical data 1979-1997

hist_data <- ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
  filter(ee$Filter$date(start, end))$
  map(clipper)

#  Create sequence of months
months <- ee$List$sequence(1,12)

# Group by month, and then reduce within groups by mean();
# the result is an ImageCollection with one image for each
# month.

hist_month <- ee$ImageCollection$fromImages(
  months$map(ee_utils_pyfunc(function(m) {
    return(hist_data$filter(ee$Filter$calendarRange(m, m, 'month'))$
             filterBounds(polygon)$
             select('tmmx')$mean()$subtract(273.15)$
             set('month', m))
    })))

# Stack all bands from image collection
hist_month <- hist_month$toBands()$reproject('EPSG:5070', NULL, 10000);

###################################
###CALCULATE COMTEMPORARY VALUES###
###################################

# Set start date and define years of data wanted
start <- ee$Date$fromYMD(1998, 1, 1)
years <- ee$List$sequence(0, 22)

# run function to create list of start dates
startDates <- years$map(ee_utils_pyfunc(function (d) {return(start$advance(d, 'year'))}))

# Define name of variables wanted from gridmet dataset
var_max <- 'tmmx'
var_min <- 'tmmn'

# mean temp function for GRIDMET. changes Kelvin to Celsius
calc_mean_temp <- function(image) {
  mean_temp <- image$select(var_max)$add(image$select(var_min))$
    divide(2)$subtract(273.15)$rename('mean_temp')
  return(image$addBands(mean_temp))
}

# Define function to process imagery
yearmap <- function(y) {
  
  # Load start date
  start <- ee$Date(y)
  
  # Create end date, in this case running function for whole year
  end <- ee$Date(y)$advance(1, 'year')
  
  dataset <- ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
    filter(ee$Filter$date(start, end))$
    map(clipper)
  
  dataset <- dataset$map(calc_mean_temp)
  
  # Calculate mean monthly temp for year
  dataset <- ee$ImageCollection$fromImages(
    months$map(ee_utils_pyfunc(function (m) {
      return(dataset$filter(ee$Filter$calendarRange(m, m, 'month'))$
               filterBounds(polygon)$
               select('mean_temp')$max()$
               set('month', m))
      })))
  
  # Stack all bands from image collection
  dataset = dataset$toBands()$reproject('EPSG:5070', NULL, 10000)
  
  # subtract historical from contemp
  dataset <- dataset$subtract(hist_month)
  
  # Rename bands
  dataset <- dataset$rename(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  
  # Set year id
  dataset <- dataset$set('out_name', ee$String('max_mon_temp_rel_hist_')$
                           cat(start$format('YYYY')))
  
  return(dataset)
}

# Run function over all years
list_of_images <- startDates$map(ee_utils_pyfunc(yearmap))
# print('list of annual layers', list_of_images);

# Change list of images to image collection
img_collect <- ee$ImageCollection(list_of_images)
# print('img collection of annual layers', img_collect);

# use geetools to export all images to drive
# you will still have to download the files to local disk manually
# but this method at least preserves file names
#otherwise try function annotated below to download directly to disk
# CRS is now set throughout the code, look for "reproject"

task_img <- gt$batch$Export$imagecollection$toDrive(
  collection = img_collect,
  folder = 'gee_max_mon_temp_rel_hist',
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
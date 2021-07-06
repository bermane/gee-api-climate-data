# This code downloads mean summer temp across wbb range from June 1 - August 31
# covering years 1998-2020. Data from GRIDMET.
# Study area is defined using uploaded shapefile.

# Authenticate the GEE environment
library(rgee)
ee_Initialize(drive = T)

# Import reticulate
library(reticulate)

# Set start date and define years of data wanted
start <- ee$Date$fromYMD(1998, 6, 1)
years = ee$List$sequence(0, 22)

# run function to create list of start dates
#startDates <- years$map(function(d) {start$advance(d, 'year')})
startDates <- years$map(ee_utils_pyfunc(function(d) {start$advance(d, 'year')}))
# print("Start dates", startDates)

# Load the output area shp file (uploaded to GEE as asset)
polygon = ee$FeatureCollection("users/bermane/wbb_conus_10km_buffer")

# Define name of variables wanted from gridmet dataset
var_max = 'tmmx'
var_min = 'tmmn'

# mean temp function for GRIDMET. changes Kelvin to Celsius
calc_mean_temp <- function(image){
  mean_temp <- image$select(var_max)$add(image$select(var_min))$
    divide(2)$subtract(273.15)$rename('mean_temp')
  return(image$addBands(mean_temp))
  }

# Define function to process imagery
yearmap <- function(y){
  
  # Load start date
  start <- ee$Date(y)
  
  # Create end date, in this case 3 month period
  end <- ee$Date(y)$advance(3, 'month')
  
  # Load dataset from GEE, filter by timeframe
  dataset <- ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$filter(ee$Filter$date(start, end))
  
  # calculate mean daily temp
  dataset <- dataset$map(calc_mean_temp)
  
  # Calculate mean over annual time frame
  dataset <- dataset$select('mean_temp')$reduce(ee$Reducer$mean())
  
  # Clip dataset to only cover area of interest
  dataset <- dataset$clip(polygon)
  
  # Rename bands to reflect year of data
  dataset <- dataset$rename(ee$String('mean_summer_temp_')$cat(start$format('YYYY')))
  
  return(dataset)
}
  
# Run function to generate list of images for each year
list_of_images <- startDates$map(ee_utils_pyfunc(yearmap))

# Change list of images to image collection
img_collect = ee$ImageCollection(list_of_images)

# Stack all bands from image collection
img_out = img_collect$toBands()

# Create task to export the image to google drive
task_img <- ee_image_to_drive(img_out, 'mean_summer_temp_gridmet_wbb_1998_2020', folder = 'gee',
                  scale = 10000, crs = 'EPSG:5070', region = polygon$geometry())

# Run export task
task_img$start()
ee_monitoring(task_img)

# Move results from Drive to local
ee_drive_to_local(task = task_img, dsn = 'mean_summer_temp_gridmet_wbb_1998_2020')

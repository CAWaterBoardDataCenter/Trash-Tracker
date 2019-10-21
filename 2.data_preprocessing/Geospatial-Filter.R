library(magick)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)
library(units)

#---- USER SUPPLIED INFORMATION --------------------------------------------------------------------------------------------#
# enter the tolerance (in meters)
    dist_tolerance <- 1.5
    dist_tolerance <- set_units(x = dist_tolerance, value = 'm')
    
# set the path to the source (original images) and destination (filtered images) folders
    survey_day <- 'Survey_20170809'
    survey_subfolder <- '100GOPRO'
    filtered_images_folder <- 'Filtered-Images-2019-10'
    base_folder <- 'E:\\TrashTracker\\'
    
# flag to determine whether or not to copy duplicates (if true, creates a subfolder in the destination folder with all of the duplicate images - just used as a check, not needed)
    copy_duplicates <- FALSE
#---------------------------------------------------------------------------------------------------------------------------#

# create the paths
    image_source_path <- paste0(base_folder, 'Trash_Images_Originals', '\\', survey_day, '\\', survey_subfolder)
    image_filter_path <- paste0(base_folder, filtered_images_folder, '\\', survey_day, '\\', survey_subfolder)
    
# get a list of the images in the source folder
    files_list <- list.files(image_source_path)
    
# create empty data frame
    image_metadata_names <- c('file_name', 'date_time', 'latitude', 'longitude')
    df_image_metadata <- data.frame(matrix(nrow = 0, 
                                            ncol = length(image_metadata_names), 
                                            dimnames = list(c(), image_metadata_names)
                                            )
                                     ) %>% as_tibble()

# cycle through images and get the metadata
    time_check <- vector()
    for (image_numb in seq(length(files_list))) {
        time_check[image_numb] <- now()
        # read the image
            trash_image <- image_read(paste0(image_source_path, '\\', files_list[image_numb]))
        # extract the full metadata
            image_metadata_full <- image_attributes(trash_image)
        # extract and format the image coordinates
            image_metadata_coord <- image_metadata_full %>% 
                filter(property == 'exif:GPSLatitude' | property == 'exif:GPSLongitude') %>% 
                separate(col = value, into = c('deg','min','sec'), sep = ",")
            image_metadata_coord <- image_metadata_coord %>% 
                mutate(property = str_remove(string = property, pattern = 'exif:GPS')) %>% 
                mutate(deg = str_remove(string = deg, pattern = '/1') %>%
                           as.numeric()
                       ) %>% 
                mutate(min = str_remove(string = min, pattern = '/1') %>%
                           as.numeric()
                       ) %>% 
                mutate(sec = str_remove(string = sec, pattern = '/10000000') %>%
                           as.numeric() / 10000000
                       ) %>% 
                mutate(value = deg + min / 60 + sec / 60^2) %>% 
                mutate(value = case_when(property == 'Longitude' ~ value * -1, TRUE ~ value))
        # extract the image date and time information
            image_metadata_date <- image_metadata_full %>% 
                filter(property == 'exif:DateTime') %>% 
                mutate(property = str_remove(string = property, pattern = 'exif:')) %>% 
                mutate(value = ymd_hms(value)) %>% 
                mutate(date = date(value))
        # combine the coordinate and date/time information in a single data frame
            image_metadata_temp <- tibble(file_name = files_list[image_numb], 
                                          date_time = image_metadata_date$value,
                                          # date = image_metadata_date$date,
                                          latitude = as.numeric(image_metadata_coord %>% filter(property == 'Latitude') %>% select(value)),
                                          longitude = as.numeric(image_metadata_coord %>% filter(property == 'Longitude') %>% select(value)))
        # add the metadata for the current image to the data frame containing metadata for all previous images
            df_image_metadata <- rbind(df_image_metadata, image_metadata_temp)
    }
    
# time check
    time_elapsed <- time_check[length(time_check)] - time_check[1]

# arrange by timestamp
    df_image_metadata <- df_image_metadata %>% arrange(date_time)
    
# convert to sf
    image_metadata_sf <- st_as_sf(x = df_image_metadata, coords = c('longitude', 'latitude'), crs = 4326)
    
# plot points
    # mapview::mapview(image_metadata_sf)
    
# compute distance (NOTE - for more info see: https://github.com/r-spatial/sf/issues/799)
    empty <- st_as_sfc("POINT(EMPTY)")
    image_metadata_sf <- image_metadata_sf %>% 
        mutate(time_to_next = lead(date_time) - date_time, 
               distance_to_next = sf::st_distance(geometry,
                                                  lead(geometry, default = empty),
                                                  by_element = TRUE),
               time_from_previous = (lag(date_time) - date_time) * -1,
               distance_from_previous = sf::st_distance(geometry,
                                                        lag(geometry, default = empty),
                                                        by_element = TRUE)
               )
    image_metadata_sf <- image_metadata_sf %>% mutate(duplicate = case_when(distance_from_previous > dist_tolerance ~ FALSE, 
                                                                            TRUE ~ TRUE))
    image_metadata_sf <- image_metadata_sf %>% mutate(duplicate_2 = case_when(distance_to_next > dist_tolerance ~ FALSE, 
                                                                            TRUE ~ TRUE))
# copy the unique (and if needed the duplicates too) images to new folders (NOTE: Duplicates also includes the first (distinct) image in a string of duplicate images, for comparison (this first image is also copied to the distinct folder). So, the sum of the number of images in the distinct and duplicate folders is greater than the number of original images.
    if (!dir.exists(image_filter_path)) {
        dir.create(image_filter_path, recursive = TRUE)
    }
    # copy distinct images
    for (i in seq(nrow(image_metadata_sf))) {    
        if (image_metadata_sf$duplicate[i] == FALSE | i == 1) {
            file.copy(from = paste0(image_source_path, '\\', image_metadata_sf$file_name[i]), 
                      to = paste0(image_filter_path, '\\', image_metadata_sf$file_name[i]))
        }    
    }
    # copy duplicates (not needed)
    if (copy_duplicates == TRUE) {
        if (!dir.exists(paste0(image_filter_path, '\\Duplicates'))) {
            dir.create(paste0(image_filter_path, '\\Duplicates'), recursive = TRUE)
        }
        for (i in seq(nrow(image_metadata_sf))) {
            if (image_metadata_sf$duplicate[i] == TRUE | image_metadata_sf$duplicate_2[i] == TRUE) {
                file.copy(from = paste0(image_source_path, '\\', image_metadata_sf$file_name[i]),
                          to = paste0(image_filter_path, '\\Duplicates\\', image_metadata_sf$file_name[i]))
            }
        }
    }

    # as a check, calculate percentage of images that are classified as duplicates
        sum(image_metadata_sf$duplicate / nrow(image_metadata_sf))
     
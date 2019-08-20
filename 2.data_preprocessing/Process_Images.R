# load packages
    library(tidyverse)
    library(sf)
    library(magick)

# define path to image library
    image_library <- "C:\\David\\Trash-ComputerVision\\test_2019-07-08\\"
    image_library_chopped <- "C:\\David\\Trash-ComputerVision\\test_2019-07-18_Chopped_coco-format\\"

# define path to the JSON file
    json_file_name <- 'test_2019-07-18_coco-format.json'
    json_file_folder <- 'C:\\David\\Trash-ComputerVision\\'  
    json_file_path <- paste0(json_file_folder, json_file_name)
    
# create directories if needed
    if (!dir.exists(image_library_chopped)) {
        dir.create(image_library_chopped)
    }
    if (!dir.exists(paste0(image_library_chopped, 'Images'))) {
        dir.create(paste0(image_library_chopped, 'Images'))
    }
    if (!dir.exists(paste0(image_library_chopped, 'Plots'))) {
        dir.create(paste0(image_library_chopped, 'Plots'))
    }
    
# read in the JSON file
    raw_text <- readr::read_lines(json_file_path)
    json_list <- jsonlite::fromJSON(raw_text)
    
# retrieve info from the JSON file
    # create empty data frame to store info extracted from the JSON file
        json_col_names <- c('img_name', 'label', 'img_object_no',
                          'x_cent', 'y_cent', 'trash_poly',
                          'bounding_cell_poly') # 'adjusted_x_cent', 'adjusted_y_cent', 'adjusted_trash_poly', 'cell_xmin', 'cell_ymin'
        tbl_jsoninfo <- data.frame(matrix(nrow = 0,
                                          ncol = length(json_col_names),
                                          dimnames = list(c(),
                                                          json_col_names)
                                          )) %>% as_tibble()
    # get  a list of all the labels
        obj_labels <- json_list$categories %>% select(c('id', 'name'))

    # loop through all images - within each image loop through all trash items annotated, and retrieve information about the annotation
            # img_no <- 1
            # obj_no <- 1
        for (img_no in seq(nrow(json_list$images))) {
            # get the annotations for the current image
                img_annotations <- json_list$annotations %>% filter(image_id == img_no) 
                if (nrow(img_annotations) > 0) {
                # loop through all of the trash objects in the image and extract the info for each object
                    for (obj_no in seq(nrow(img_annotations))) {
                    # get the object label
                        obj_label_id <- img_annotations$category_id[obj_no]
                        obj_label <- obj_labels$name[obj_label_id]                
                    # get the coordinate of the polygon defining the current trash object
                        obj_poly <- img_annotations$segmentation[obj_no][[1]]
                    # get the locations of the x and y coordinates in the obj_poly character string
                        obj_poly_x_coords_tf <- (seq(length(obj_poly)) %% 2) == 1
                        obj_poly_y_coords_tf <- !obj_poly_x_coords_tf
                    # separate out the x and y coordinates
                        obj_poly_x_coords <- obj_poly[obj_poly_x_coords_tf] %>% as.matrix()
                        obj_poly_y_coords <- obj_poly[obj_poly_y_coords_tf] %>% as.matrix()
                    # add a coordinate that is the same as the first element to close the polygon
                        obj_poly_x_coords <- rbind(obj_poly_x_coords, obj_poly_x_coords[1,1])
                        obj_poly_y_coords <- rbind(obj_poly_y_coords, obj_poly_y_coords[1,1])
                    # convert polygon coordinates to a 2-column maxtrix with the x, y pairs
                        obj_poly_matrix <- cbind(obj_poly_x_coords, obj_poly_y_coords)
                    # reverse the y coordinates
                        obj_poly_matrix_reversed <- obj_poly_matrix
                        obj_poly_matrix_reversed[,2] <- (obj_poly_matrix[,2]-3000)*-1
                    # convert to an sf object
                        obj_poly_reversed_list <- list(obj_poly_matrix_reversed)
                        obj_poly_reversed_sf <- st_polygon(obj_poly_reversed_list) %>% st_sfc()
                    # get info about the centroid
                        centroid <- st_centroid(obj_poly_reversed_sf)
                        x_cent <- centroid[[1]][1]
                        y_cent <- centroid[[1]][2]
                        df_cent <- data.frame(x = x_cent, y = y_cent)
                    # form 1000x1000 bounding box around each item
                        if (x_cent <= 500) {
                            xmin <- 0
                            xmax <- 1000
                        } else if (x_cent >= 3500) {
                            xmin = 3000
                            xmax = 4000
                        } else {
                            xmin = x_cent - 500
                            xmax = x_cent + 500
                        }
                        if (y_cent <= 500) {
                            ymin <- 0
                            ymax <- 1000
                        } else if (y_cent >= 2500) {
                            ymin = 2000
                            ymax = 3000
                        } else {
                            ymin = y_cent - 500
                            ymax = y_cent + 500
                        }
                        bounding_cell_poly <- st_polygon(list(rbind(
                            c(xmin, ymax),
                            c(xmax, ymax),
                            c(xmax, ymin),
                            c(xmin, ymin),
                            c(xmin, ymax)))) %>% st_sfc()
                    # # plot
                    #     g <- ggplot() +
                    #         geom_sf(data = bounding_cell_poly) +
                    #         geom_sf(data = obj_poly_reversed_sf, fill = 'orange') +
                    #         geom_point(data = df_cent, aes(x = x_cent, y = y_cent)) +
                    #         labs(x = 'X', y = 'Y') +
                    #         annotate(geom = 'text', x = x_cent, y = y_cent, label = obj_label) +
                    #         xlim(0, 4000) + ylim(0, 3000)
                    # put into data frame
                        temp_tbl_json <- tibble('img_name' = json_list$images$file_name[img_no],
                                              'label' = obj_label,
                                              'img_object_no' = obj_no,
                                              'x_cent' = x_cent,
                                              'y_cent'= y_cent,
                                              # 'adjusted_x_cent' = x_cent - xmin,
                                              # 'adjusted_y_cent' = y_cent - ymin,
                                              'trash_poly' = obj_poly_reversed_sf,
                                              # 'adjusted_trash_poly' = obj_poly_sf - c(xmin, ymin), # calculate the adjusted polygon here
                                              'bounding_cell_poly' = bounding_cell_poly,
                                              # 'cell_xmin' = xmin,
                                              # 'cell_ymin' = ymin#,
                                              # 'plot' = list(g)
                                           )
                        tbl_jsoninfo <- rbind(tbl_jsoninfo, temp_tbl_json)
                    }
                }
        }
    # convert data frame to sf
        tbl_jsoninfo_sf <- st_as_sf(tbl_jsoninfo)        

        
       
    # # plot to check
    #     ggg <- ggplot(data = tbl_jsoninfo_sf) +
    #         geom_sf() +
    #         geom_point(aes(x = x_cent, y = y_cent)) +
    #         xlim(0, 4000) + ylim(0, 3000) +
    #         geom_text(aes(x = x_cent, y = y_cent, label = label))
    #     ggg

# # NOT PART OF PROGRAM -- JUST TO CHECK
#     # Create a grid spatial object (i.e., create 12 1000 x 1000 polygons covering the 4000 x 3000 raw image area)
#         df_cells <- data.frame(matrix(nrow = 0, ncol = 2, dimnames = list(c(), c('cell_no', 'geom'))))
#         for (cell_id in 1:12) {
#             up_left <- c((cell_id - 1 - (4 * (cell_id-1) %/% 4)) * 1000, 3000 - ((cell_id-1) %/% 4 * 1000))
#             up_right <- up_left + c(1000, 0)
#             down_right <- up_right + c(0, -1000)
#             down_left <- down_right + c(-1000, 0)
#             cell_polygon <- st_polygon(list(rbind(up_left, up_right, down_right, down_left, up_left))) %>% st_sfc
#             df_cells <- rbind(df_cells, data.frame('cell_no' = cell_id, 'geom' = cell_polygon))
#         }
#         # convert data frame to sf
#             df_cells_sf <- st_as_sf(df_cells)
#         # plot cells to check
#             # cells_plot <- ggplot(data = df_cells_sf) + geom_sf(aes(fill = cell_no)) + geom_sf_label(aes(label = cell_no))
# 
#     # plot objects on the  grid (change the "image_number" variable below to cycle through images)
#         image_number <- 3
#         df_plot <- tbl_jsoninfo_sf %>% filter(img_name == json_list$images$file_name[[image_number]])
#         objects_plot <- ggplot() +
#             geom_sf(data = df_cells_sf, aes(fill = cell_no)) + scale_fill_gradient(low = 'lightskyblue', high = 'dodgerblue') +
#             geom_sf_label(data = df_cells_sf, aes(label = cell_no)) +
#             geom_sf(data = df_plot$bounding_cell_poly, alpha = 0.3, color = 'red') +
#             geom_sf(data = df_plot$trash_poly, fill = 'orange') +
#             geom_point(data = df_plot, aes(x = x_cent, y = y_cent)) +
#             xlim(0, 4000) + ylim(0, 3000) +
#             geom_text(data = df_plot, aes(x = x_cent, y = y_cent, label = paste0(img_object_no, '-', label)), color = 'black') +
#             theme_minimal() +
#             labs(title = json_list$images$file_name[[image_number]])
#         objects_plot
#         # show the original image for comparison
#             original_image <- image_read(path = paste0(image_library, json_list$images$file_name[[image_number]]))
#             image_scale(original_image, '680')
# 
# ########################### THIS BLOCK NOT MODIFIED FOR COCO FORMAT  ###########################################   
#         # NOT CURRENTLY WORKING ###############
#             # plot the individual objects for the given image
#                     cut_obj <- 1
#                     cut_image_plot <- ggplot() +
#                         geom_sf(data = df_plot[cut_obj,]$adjusted_trash_poly, fill = 'orange') +
#                         geom_text(data = df_plot[cut_obj,], aes(x = adjusted_x_cent, y = adjusted_y_cent, label = paste0(img_object_no, '-', label))) +
#                         geom_point(data = df_plot[cut_obj,], aes(x = adjusted_x_cent, y = adjusted_y_cent)) +
#                         xlim(0,1000) + ylim(0,1000)
#                     cut_image_plot
# 
#                 # crop the image (crop out the 1000x1000 section) and view for comparison
#                     img.temp <- image_crop(image = original_image,
#                                            geometry = geometry_area(width = 1000,
#                                                                     height = 1000,
#                                                                     x_off = df_plot[cut_obj,]$cell_xmin,
#                                                                     y_off = 3000 - (df_plot[cut_obj,]$cell_ymin + 1000)))
#                     image_scale(img.temp, '510')
#         ######################################
            
            
############################################ RESUME PROGRAM ###############################################################
        # loop through all images - for each image, loop through all annotated trash items to extract the 1000x1000 image containing the item
        # also check to see if there are any other trash items within that same image boundary, and clip those polygons to the image boundary
        # and check whether any other trash items are completely containined within the boundary of an image for a previous trash item from the same original image (to avoid creating duplicates of the same items in multiple images)
            # create an empty list to save the outputs to
                cells_list <- list()
            # loop through all images 
            for (img_no in seq(nrow(json_list$images))) {
                # just for testing
                    # img_no <- 3
                    # obj_no <- 3
                # create a counter for the number of new (chopped) images created from this original image
                    chopped_image_counter <- 0
                # for each original image, create another empty list within the cells_list object to save the outputs from the cropped images
                    cells_list[[img_no]] = list()
                # create a data frame with just the info for this original image
                    temp_tbl_image <- tbl_jsoninfo_sf %>% filter(img_name == json_list$images$file_name[[img_no]])
                # get the original image (used below for creating and saving the chopped image)
                    original_image <- image_read(path = paste0(image_library, json_list$images$file_name[[img_no]]))
                    # image_scale(original_image, '680') # View image
                # loop through all annotated trash items in this original image to create the processed (chopped) images
                if (nrow(temp_tbl_image) > 0) {
                for (obj_no in seq(nrow(temp_tbl_image))) {
                    # first check whether the object is completely contained within the bounding cell of a previous object processed from this image
                        if (obj_no > 1) {
                            contained_check <- st_covered_by(x = temp_tbl_image$trash_poly[[obj_no]], 
                                                             y = st_buffer(x = temp_tbl_image$bounding_cell_poly[c(1:(obj_no - 1))], 
                                                                           dist = 2), 
                                                             sparse = FALSE
                                                             )
                            if (sum(contained_check >= 1)) {
                                create_adj_poly = FALSE # item is completley contained within a previous bounding cell/clipped image - don't need to create a new image and set of adjusted polygons
                                } else {
                                    create_adj_poly = TRUE # item is not completely contained within a previous bounding cell/clipped image
                                    } 
                        } else {create_adj_poly = TRUE} # 'first item in image'
                    
                    # if the object is not completely contained by the bounding box of a previous item in this image, create a new clipped image 
                    # and adjust coordinates of any items so they reflect the position within the clipped image
                    if (create_adj_poly == TRUE) {
                        # increment the counter
                            chopped_image_counter <- chopped_image_counter + 1
                        # get the bounding cell for this object
                            bound_cell <- temp_tbl_image$bounding_cell_poly[obj_no]
                            bound_cell_xmin <- st_bbox(bound_cell)['xmin']
                            bound_cell_ymin <- st_bbox(bound_cell)['ymin']
                            bound_cell_ymax <- st_bbox(bound_cell)['ymax']
                        # get intersecting polygons
                            overlap_check <- st_intersects(x = bound_cell, y = temp_tbl_image$trash_poly, sparse = FALSE)
                            # clip the overlapping polygons to the cell border
                                polygons_overlapping <- temp_tbl_image[overlap_check,]$trash_poly
                                clipped_poly_list <- list()
                                # clipped_poly_df <- tbl_df(data = NULL)
                                for (i in seq(sum(overlap_check))) {
                                    polygon_overlap <- st_intersection(bound_cell, polygons_overlapping[i])
                                    clipped_poly_list[[i]] <- polygon_overlap
                                }
                                # # check
                                #     plot(bound_cell)
                                #     plot(polygons_overlapping, col = 'grey', add = TRUE)
                                #     for (j in seq(length(clipped_poly_list))) {
                                #         plot(clipped_poly_list[[j]], col = 'blue', add = TRUE)
                                #     } # things in blue are the objects clipped to the bounding cell
                        # create data frame with all (clipped) trash item polygons in the bounding cell
                            temp_tbl_cell <- temp_tbl_image[overlap_check,] %>% select(img_name, label, x_cent, y_cent, trash_poly)
                            temp_tbl_cell <- temp_tbl_cell %>% rename(original_img_name = img_name)
                            temp_tbl_cell <- temp_tbl_cell %>% mutate(clipped_img_name = paste0(json_list$images$file_name[[img_no]], 
                                                                      '-x', round(bound_cell_xmin,1), 
                                                                      '-y', round(bound_cell_ymin,1), 
                                                                      '-', str_c(unique(label), collapse = '-'), 
                                                                      '.jpg'))
                            temp_tbl_cell <- temp_tbl_cell %>% mutate(bounding_poly = bound_cell)
                            temp_tbl_cell <- temp_tbl_cell %>% mutate(clipped_trash_poly = trash_poly)
                            for (p in seq(length(clipped_poly_list))) {
                                temp_tbl_cell$clipped_trash_poly[p] <- clipped_poly_list[[p]]
                            }
                        # compute the id number of each object in the clipped image
                            temp_tbl_cell <- temp_tbl_cell %>% mutate(obj_id = row_number())
                        # # check - things in blue are retained in the bounding cell
                            #     plot(bound_cell)
                            #     plot(polygons_overlapping[1], add = T)
                            #     plot(temp_tbl_cell$trash_poly[1], col = 'grey', add = T)
                            #     plot(temp_tbl_cell$clipped_trash_poly[1], col = 'blue', add = T)
                            #     plot(polygons_overlapping[2], add = T)
                            #     plot(temp_tbl_cell$trash_poly[2], col = 'grey', add = T)
                            #     plot(temp_tbl_cell$clipped_trash_poly[2], col = 'blue', add = T) 
                            temp_tbl_cell <- temp_tbl_cell %>% mutate(clipped_trash_poly_adj = clipped_trash_poly - c(bound_cell_xmin, bound_cell_ymin),
                                                                      x_cent_adj = x_cent - bound_cell_xmin,
                                                                      y_cent_adj =y_cent - bound_cell_ymin)
                        # compute the area of the clipped/adjusted polygon
                            temp_tbl_cell <- temp_tbl_cell %>% mutate(clipped_trash_poly_area = st_area(clipped_trash_poly_adj))
                        # add the temp_tbl_cell data frame as an element in the output list
                            cells_list[[img_no]][[chopped_image_counter]] <- temp_tbl_cell
                        # add the temp_tbl_cell data frame to a complete data frame of all trash objects
                            if (!exists('df_objects_combined')) {
                                df_objects_combined <- temp_tbl_cell} else {
                                    df_objects_combined <- bind_rows(df_objects_combined, temp_tbl_cell)
                                }
                        # create and save the chopped image
                            img.temp <- image_crop(image = original_image, 
                                                   geometry = geometry_area(width = 1000, 
                                                                            height = 1000, 
                                                                            x_off = bound_cell_xmin, 
                                                                            y_off = 3000 - bound_cell_ymax))
                            # image_scale(img.temp, '510') # View image
                            # save the image
                                image_write(image = img.temp,
                                            path = paste0(image_library_chopped, 'Images\\',
                                                          temp_tbl_cell$original_img_name[1], 
                                                          '-x', round(bound_cell_xmin,1), 
                                                          '-y', round(bound_cell_ymin,1), 
                                                          '-', str_c(unique(temp_tbl_cell$label), collapse = '-'), 
                                                          '.jpg'))
                            # plot to check
                                # g <- ggplot() +
                                #     geom_sf(data = temp_tbl_cell$bounding_poly[1]) +
                                #     geom_sf(data = temp_tbl_cell$clipped_trash_poly) +
                                #     geom_point(data = temp_tbl_cell, aes(x = x_cent, y = y_cent)) +
                                #     geom_text(data = temp_tbl_cell, aes(x = x_cent, y = y_cent, label = label))
                                # g
                                g_adjusted <- ggplot() +
                                    geom_sf(data = temp_tbl_cell$clipped_trash_poly_adj) +
                                    geom_point(data = temp_tbl_cell, aes(x = x_cent_adj, y = y_cent_adj)) +
                                    geom_text(data = temp_tbl_cell, aes(x = x_cent_adj, y = y_cent_adj, label = label)) +
                                    xlim(0,1000) + ylim(0,1000) +
                                    labs(x = 'X Adjusted', y = 'Y Adjusted')
                                g_adjusted
                                ggsave(filename = paste0(image_library_chopped, 'Plots\\',
                                                         temp_tbl_cell$original_img_name[1], 
                                                         '-x', round(bound_cell_xmin,1), 
                                                         '-y', round(bound_cell_ymin,1), 
                                                         '-', str_c(unique(temp_tbl_cell$label), collapse = '-'), '-PLOT', 
                                                         '.jpg'), 
                                       plot = g_adjusted)
                    }
                }
                }
            }
            
            
        # # plot to test
        #     img_no <- 2 # image 16 has multiple items within one chopped image
        #         length(cells_list[[img_no]])
        #     chopped_img_no <- 1
        # 
        #     g <- ggplot() + 
        #         geom_sf(data = cells_list[[img_no]][[chopped_img_no]]$clipped_trash_poly_adj) + 
        #         geom_point(data = cells_list[[img_no]][[chopped_img_no]], aes(x = x_cent_adj, y = y_cent_adj)) + 
        #         geom_text(data = cells_list[[img_no]][[chopped_img_no]], aes(x = x_cent_adj, y = y_cent_adj, label = label)) +
        #         xlim(0,1000) + ylim(0,1000)
        #     g
        #     
        #     # View the original image
        #         original_image <- image_read(path = paste0(image_library, 
        #                                                    cells_list[[img_no]][[chopped_img_no]]$original_img_name))
        #         image_scale(image = original_image, '680')
        #     
        #     # View the image
        #         cropped_image <- image_read(path = paste0(image_library_chopped, 
        #                                                   cells_list[[img_no]][[chopped_img_no]]$clipped_img_name[1]))
        #         image_scale(image = cropped_image, '510')
                
                

            
# Re-create the coco json file structure, but with the chopped images rather than the originals
    # create an empty list to store the outputs in
        json_out <- list()
        
    # Part 1: images
        image_names <- data.frame('file_name' = df_objects_combined$clipped_img_name) %>% distinct(file_name)
        image_names <- image_names %>% mutate('height' = 1000, 'width' = 1000, 'id' = row_number())
        json_out[['images']] <- image_names
        
    # Part 2: type
        json_out[['type']] <- 'instances'
        
    # Part 3: annotations
        segmentation_list_out <- list()
        obj_bbox_list_out <- list()
        # convert the clipped/adjusted polygons back to the coco format
            for (obj_num_out in seq(nrow(df_objects_combined))) {
                poly_convert <- df_objects_combined$clipped_trash_poly_adj[[obj_num_out]][[1]]
                # drop the last row (revert back to the imglab format)
                poly_convert_2 <- poly_convert[-nrow(poly_convert),]
                # reverse the y coordinates (revert back to the imglab format) (y measured from the top down)
                poly_convert_3 <- poly_convert_2
                poly_convert_3[,2] <- 1000 - poly_convert_2[,2]
                # compute the bounding box around the object (x, y, width, height -- where x,y is the upper left corner, y measured from the top down)
                    obj_bbox_x <- min(poly_convert_3[,1])
                    obj_bbox_y <- min(poly_convert_3[,2])
                    obj_bbox_width <- max(poly_convert_3[,1]) - obj_bbox_x
                    obj_bbox_height <- max(poly_convert_3[,2]) - obj_bbox_y
                    obj_bbox <- c(obj_bbox_x, obj_bbox_y, obj_bbox_width, obj_bbox_height)
                # convert to a 1 row matrix with x,y pairs grouped together (revert back to the imglab format)
                poly_convert_4 <- matrix(data = poly_convert_3[1,], nrow = 1, ncol = 2)
                for (row_num in 2:nrow(poly_convert_3)) {
                    poly_convert_4 <- cbind(poly_convert_4, matrix(data = poly_convert_3[row_num,], nrow = 1, ncol = 2))
                }
                segmentation_list_out[[obj_num_out]] <- poly_convert_4
                obj_bbox_list_out[[obj_num_out]] <- obj_bbox
            }
        # combine everything into a single data frame
            df_annotations <- data.frame('segmentation'=I(segmentation_list_out))
            df_annotations <- df_annotations %>% mutate('area' = df_objects_combined$clipped_trash_poly_area)
            df_annotations <- df_annotations %>% mutate('iscrowd' = 0)
            df_annotations <- df_annotations %>% mutate('image_id' = match(df_objects_combined$clipped_img_name, image_names$file_name))
            df_annotations <- df_annotations %>% mutate('bbox' = obj_bbox_list_out)
            df_annotations <- df_annotations %>% mutate('category_id' = match(df_objects_combined$label, obj_labels$name))
            df_annotations <- df_annotations %>% mutate('id' = df_objects_combined$obj_id) 
            df_annotations <- df_annotations %>% mutate('ignore' = 0)
        # add the data frame to the json_out object
            json_out[['annotations']] <- df_annotations
        
    # Part 4: categories
        json_out[['categories']] <- json_list$categories
        
    # write to json file
        json_out_formatted <- jsonlite::toJSON(json_out)
        json_file_out <- paste0(json_file_folder, gsub(pattern = '.json', replacement = '', x = json_file_name), '_Processed.json')
        write_file(x = json_out_formatted, path = json_file_out)
        
        
        
        
        
        
########################### OLD STUFF NOT MODIFIED FOR COCO FORMAT BELOW THIS POINT ########################################### 
         
    # # loop through each element in the cells_list object
    #         # original_image_num <- 3
    #         # cropped_image_num <- 4
    #     for (original_image_num in seq(length(cells_list))) {
    #         for (cropped_image_num in seq(length(cells_list[[original_image_num]])))
    #             for (obj_num_in_image in seq(nrow(cells_list[[original_image_num]][[cropped_image_num]])))
    #         
    #         for (cropped_image_num in seq(length(cells_list[[original_image_num]]))) {
    #             json_out[['annotations']] <- as.data.frame(df_objects_combined %>% select(clipped_img_name))
    #             
    #             # cropped_image_num <- 1
    #             image_name <- cells_list[[original_image_num]][[cropped_image_num]]$clipped_img_name[1] # original_img_name[1]
    #             json_out[[image_name]] <- list()
    #             json_out[[image_name]][['imagename']] <- image_name
    #             json_out[[image_name]][['attributes']] <- list()
    #             json_out[[image_name]][['tags']] <- list()
    #             json_out[[image_name]][['size']] <- list('width' = 1000L, 'height' = 1000L)
    #             json_out[[image_name]][['shapes']] <- list()
    #                 # json_out[[image_name]][['shapes']][['id']] <- obj_id
    #                 json_out[[image_name]][['shapes']][['label']] <- cells_list[[original_image_num]][[cropped_image_num]]$label
    #                 json_out[[image_name]][['shapes']][['type']] <- rep(x = 'polygon', times = nrow(cells_list[[original_image_num]][[cropped_image_num]]))
    #                     points_list <- list()
    #                     for (clip_img_obj in seq(nrow(cells_list[[original_image_num]][[cropped_image_num]]))) {
    #                         points_list[[clip_img_obj]] <- cells_list[[original_image_num]][[cropped_image_num]]$clipped_trash_poly_adj[[clip_img_obj]][[1]]
    #                     }
    #                 json_out[[image_name]][['shapes']][['points']] <- points_list
    #                 # json_out[[image_name]][['shapes']][['bbox']] <- data.frame('x' = c(1,1),
    #                 #                                                            'y' = c(2,2),
    #                 #                                                            'cx' = c(3,3),
    #                 #                                                            'cy' = c(4,4),
    #                 #                                                            'w' = c(5,5),
    #                 #                                                            'h' = c(6,6),
    #                 #                                                            'width' = c(7,7),
    #                 #                                                            'height' = c(8,8),
    #                 #                                                            stringsAsFactors = FALSE)
    #                 json_out[[image_name]][['shapes']][['attributes']] <- matrix(data = rep(list(list()), nrow(cells_list[[original_image_num]][[cropped_image_num]])), 
    #                                                                              nrow = nrow(cells_list[[original_image_num]][[cropped_image_num]]), 
    #                                                                              ncol = 1)
    #                 json_out[[image_name]][['shapes']][['tags']] = matrix(data = obj_attributes, nrow = length(obj_id), ncol = 1)
    #                 # json_out[[image_name]][['shapes']][['featurePoints']] = matrix(data = obj_feature_points, nrow = length(obj_id), ncol = 1) 
    #                 # json_out[[image_name]][['shapes']][['zoomScale']] = zoom_scale
    #                 # json_out[[image_name]][['shapes']][['defaultZoomScale']] = default_zoom_scale
    #                 # json_out[[image_name]][['shapes']][['category']] = obj_category
    #             
    #             json_out[[image_name]][['shapeIndex']] <- 0L
    #             json_out[[image_name]][['pointIndex']] <- 0L
    #             json_out[[image_name]][['featurePointSize']] <- 3L
    #         }
    #     }
    # View(json_out)
    # 
    # 
    # obj_id <- c('id_1', 'id_2')
    # obj_label <- cells_list[[original_image_num]][[cropped_image_num]]$label # c('obj_label_1', 'obj_label_2')
    # obj_category <- c('obj_cat_1', 'obj_cat_2')
    # zoom_scale <- c(0.1, 0.1)
    # default_zoom_scale <- c(0.9, 0.9)
    # obj_attributes <- rep(list(list()), length(obj_id))
    # obj_tags <- rep(list(list()), length(obj_id))
    # obj_feature_points <- rep(list(list()), length(obj_id))
    # obj_bbox = data.frame('x' = c(1,1),
    #                     'y' = c(2,2),
    #                     'cx' = c(3,3),
    #                     'cy' = c(4,4),
    #                     'w' = c(5,5),
    #                     'h' = c(6,6),
    #                     'width' = c(7,7),
    #                     'height' = c(8,8),
    #                     stringsAsFactors = FALSE)
    # obj_points <- list(cells_list[[2]][[1]]$clipped_trash_poly_adj[[1]][[1]], 
    #                    cells_list[[2]][[2]]$clipped_trash_poly_adj[[1]][[1]])
    # 
    # 
    # 
    # json_out <- list()
    # json_out[[image_name]] <- list()
    # json_out[[image_name]][['imagename']] <- image_name
    # json_out[[image_name]][['attributes']] <- list()
    # json_out[[image_name]][['tags']] <- list()
    # json_out[[image_name]][['size']] <- list('width' = 1000L, 'height' = 1000L)
    # json_out[[image_name]][['shapes']] <- list()
    # json_out[[image_name]][['shapes']][['id']] <- obj_id
    # json_out[[image_name]][['shapes']][['label']] <- obj_label
    # json_out[[image_name]][['shapes']][['type']] <- rep(x = 'polygon', times = length(obj_id))
    # json_out[[image_name]][['shapes']][['points']] <- obj_points
    # json_out[[image_name]][['shapes']][['bbox']] <- data.frame('x' = c(1,1),
    #                                                            'y' = c(2,2),
    #                                                            'cx' = c(3,3),
    #                                                            'cy' = c(4,4),
    #                                                            'w' = c(5,5),
    #                                                            'h' = c(6,6),
    #                                                            'width' = c(7,7),
    #                                                            'height' = c(8,8),
    #                                                            stringsAsFactors = FALSE)
    # json_out[[image_name]][['shapes']][['attributes']] <- matrix(data = obj_attributes, nrow = length(obj_id), ncol = 1)
    # json_out[[image_name]][['shapes']][['tags']] = matrix(data = obj_attributes, nrow = length(obj_id), ncol = 1)
    # json_out[[image_name]][['shapes']][['featurePoints']] = matrix(data = obj_feature_points, nrow = length(obj_id), ncol = 1) 
    # json_out[[image_name]][['shapes']][['zoomScale']] = zoom_scale
    # json_out[[image_name]][['shapes']][['defaultZoomScale']] = default_zoom_scale
    # json_out[[image_name]][['shapes']][['category']] = obj_category
    # 
    # 
    # # json_out[[image_name]][['shapes']] <- data.frame('id' = obj_id,
    # #                                                  'label' = obj_label,
    # #                                                  'type' = rep(x = 'polygon', times = length(obj_id)), 
    # #                                                  # 'points' = list(),
    # #                                                  'bbox' = data.frame('x' = c(1,1),
    # #                                                                      'y' = c(2,2),
    # #                                                                      'cx' = c(3,3),
    # #                                                                      'cy' = c(4,4),
    # #                                                                      'w' = c(5,5),
    # #                                                                      'h' = c(6,6),
    # #                                                                      'width' = c(7,7),
    # #                                                                      'height' = c(8,8)),
    # #                                                                      # stringsAsFactors = FALSE),
    # #                                                  'attributes' = matrix(data = obj_attributes, nrow = length(obj_id), ncol = 1),
    # #                                                  'tags' = matrix(data = obj_attributes, nrow = length(obj_id), ncol = 1),
    # #                                                  'featurePoints' = matrix(data = obj_feature_points, nrow = length(obj_id), ncol = 1), 
    # #                                                  'zoomScale' = zoom_scale, 
    # #                                                  'defaultZoomScale' = default_zoom_scale,
    # #                                                  'category' = obj_category,
    # #                                                  stringsAsFactors = FALSE)
    # json_out[[image_name]][['shapeIndex']] <- 0L
    # json_out[[image_name]][['pointIndex']] <- 0L
    # json_out[[image_name]][['featurePointSize']] <- 3L
    # View(json_out)

            
# Old Stuff ---------------------------------------------------------------------------------------------
# # crop and save images for each trash item
#     # img_no <- 2
#     for (img_no in seq(length(json_list))) {
#         original_image <- image_read(path = paste0(image_library, json_list[[img_no]]$imagename))
#         tbl_temp_images <- tbl_jsoninfo_sf %>% filter(img_name == json_list[[img_no]]$imagename)
#         # image_scale(original_image, '680') # View image
#         for (obj_no in seq(nrow(json_list[[img_no]]$shapes))) {
#             img.temp <- image_crop(image = original_image, 
#                                    geometry = geometry_area(width = 1000, 
#                                                             height = 1000, 
#                                                             x_off = tbl_temp_images$cell_xmin[obj_no], 
#                                                             y_off = 3000 - (tbl_temp_images$cell_ymin[obj_no] + 1000)))
#             # image_scale(img.temp, '510') # View image
#             image_write(image = img.temp,
#                         path = paste0(image_library_chopped, 
#                                       tbl_temp_images$img_name[obj_no], '-', obj_no, '-', tbl_temp_images$cell_xmin[obj_no], '-', tbl_temp_images$cell_ymin[obj_no], '-', tbl_temp_images$label[obj_no], '.jpg')) 
#             
#         }
#     }
            
    # # loop through all images and objects to extract the 1000 x 1000 cells containing trash objects
    #     tbl_jsoninfo_sf <- tbl_jsoninfo_sf %>% mutate(adjusted_poly = poly - c())
    #     for (img_no in seq(length(json_list))) { # img_no <- 10
    #         for (cut_obj_no in seq(nrow(json_list[[img_no]]$shapes))) {
    #             # cut out (custom) 1000 x 1000 cells containing trash objects
    #                 tbl_cut <- tbl_jsoninfo_sf %>% filter(img_name == json_list[[img_no]]$imagename)
    #                 # cut_obj_no <- 1
    #                 
    #                 xmin_bbox <- st_bbox(tbl_cut$cell_poly[cut_obj_no])[[1]]
    #                 ymin_bbox <- st_bbox(tbl_cut$cell_poly[cut_obj_no])[[2]]
    #                 ymax_bbox <- st_bbox(tbl_cut$cell_poly[cut_obj_no])[[4]]
    #             
    #         }
    #     }
    #     
    # 
    #     # adjust object polygons
    #         adj_poly <- df_plot$trash_poly[cut_obj_no] - c(xmin_bbox, ymin_bbox)
    #         # # manual method - not needed
    #         #     x_coords <- df_plot$trash_poly[cut_obj_no][[1]][1][[1]][,1]
    #         #     x_coords_adj <- x_coords - xmin_bbox
    #         #     y_coords <- df_plot$trash_poly[cut_obj_no][[1]][1][[1]][,2]
    #         #     y_coords_adj <- y_coords - ymin_bbox
    #         #     # convert adjusted coordinates back to object polygons
    #         #         adj_coords_matrix <- list(matrix(data = c(x_coords_adj, y_coords_adj), nrow = length(x_coords_adj), ncol = 2))
    #     df_cut_image_plot <- df_plot[cut_obj_no,]
    #     df_cut_image_plot <- df_cut_image_plot %>% mutate(adjusted_poly = adj_poly,
    #                                                       adjusted_x_cent = x_cent - xmin_bbox,
    #                                                       adjusted_y_cent = y_cent - ymin_bbox)
    #     cut_image_plot <- ggplot() + 
    #         geom_sf(data = df_cut_image_plot$adjusted_poly, fill = 'orange') +
    #         geom_text(data = df_cut_image_plot, aes(x = adjusted_x_cent, y = adjusted_y_cent, label = label)) + 
    #         geom_point(data = df_cut_image_plot, aes(x = adjusted_x_cent, y = adjusted_y_cent)) +
    #         xlim(0,1000) + ylim(0,1000)
    #     
    #     
    # # crop the image (crop out the 1000x1000 section)
    #     img.temp <- image_crop(image = original_image, 
    #                            geometry = geometry_area(width = 1000, 
    #                                                     height = 1000, 
    #                                                     x_off = xmin_bbox, 
    #                                                     y_off = 3000-ymax_bbox))
    #     image_scale(img.temp, '510')
    #     image_write(image = img.temp,
    #                 path = paste0('C:\\David\\Trash-ComputerVision\\test_2019-07-08_Chopped\\', 
    #                               json_list[[image_number]]$imagename, '-', cut_obj_no, '-', xmin_bbox, '-', ymin_bbox, '-', df_cut_image_plot$label, '.jpg')) 
    #     
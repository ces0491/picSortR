#' Compare a single pair of images
#'
#' @param img_tbl a tibble containing columns image and image_comp
#'
#' @return a list with the distortion value as a double
#'
get_img_dist_single <- function(img_tbl) {
  
  assertthat::assert_that(all(names(img_tbl) %in% c("image", "image_comp")))
  
  result <- magick::image_compare_dist(img_tbl$image[[1]], img_tbl$image_comp[[1]])
  result[[1]]
}

#' Get distortion values for cartesian product of images 
#'
#' @param image_source_path string indicating the location of images
#' @param images optional - character vector of images to compare from image_source_path
#'
#' @return a tbl_df with columns id as chr, images as list and distortion as dbl
#'
get_image_dist <- function(image_source_path, images = NULL) {
  
  ### identical images should have a distortion value of ~1, the less similar, the closer to 0.  
  image_paths <- data.frame(path = list.files(image_source_path, full.names = TRUE))
  image_path_df <- dplyr::mutate(image_paths, name = gsub(".*[//]([^.]+)[.].*", "\\1", path))
  
  image_list <- list()
  
  for(img in image_path_df$path) {
    pic <- magick::image_read(img)
    image_list[[img]] <- pic
  }
  
  image_df <- image_list %>% 
    tibble::enframe(name = "path", value = "image") %>% 
    dplyr::left_join(image_path_df, by = "path")
  
  if(!is.null(images)) {
    image_df <- dplyr::filter(image_df, name %in% !!images)
  }
  
  dist_df <- image_df %>% 
    tidyr::crossing(name_comp = .$name) %>% 
    dplyr::left_join(image_df, by = c("name_comp" = "name")) %>%
    tidyr::unite("id", c("name", "name_comp"), sep = "_vs_") %>% 
    dplyr::select(id, image.x, image.y) %>%
    dplyr::rename(image = image.x,
                  image_comp = image.y) %>%
    tidyr::nest(-id, .key = images) %>% 
    dplyr::mutate(distortion = purrr::map(images, get_img_dist_single)) %>% 
    tidyr::unnest(distortion)  
  
  dist_df
}


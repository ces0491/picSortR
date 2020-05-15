#' Sort Images
#' 
#' Sorts image pairs into buckets of 'duplicate', 'original', 'low_similarity' and 'high_similarity'.
#'
#' @param dist_df a tbl_df with columns id as chr, images as list and distortion as dbl
#' @param dupe_threshold minimum distortion value at which image will be classified as a duplicate
#' @param orig_threshold maximum distortion value at which image will be classified as an original
#'
#' @return tbl with cols id (chr), distortion (dbl), bucket (chr), image (list) and image_comp (list)
#'
sort_images <- function(dist_df, dupe_threshold = 0.9, orig_threshold = 0.4) {
 
  assertthat::assert_that(all(names(dist_df) %in% c("id", "images", "distortion")))
  
  sim_split <- (dupe_threshold + orig_threshold) / 2
  
  sort_df <- dist_df %>% 
    dplyr::mutate(bucket = dplyr::case_when(
      distortion >= dupe_threshold ~ "duplicate",
      distortion <= orig_threshold ~ "original",
      distortion > orig_threshold & distortion < sim_split ~ "low_similarity",
      distortion < dupe_threshold & distortion >= sim_split ~ "high_similarity")
      ) %>% 
    tidyr::unnest(images)
  
  sort_df
}
image_source_path <- "./inst/extdata/sample_pics"

dist_df <- get_image_dist(image_source_path)

sort_tbl <- sort_images(dist_df)
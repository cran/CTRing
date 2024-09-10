#' convert pith coordinates from pixels to length units
#'
#' @param pith_coord Pith coordinates in pixels
#' @param pixel_size_x Pixel size in x
#' @param pixel_size_y Pixel size in y
#'
#' @return Pixel coordinates in length units
#' @export
#'
#'
pithCoordinates <- function(pith_coord, pixel_size_x, pixel_size_y) {
  pith_coord["x"] <- pith_coord["x"] * pixel_size_x
  pith_coord["y"] <- pith_coord["y"] * pixel_size_y
  return(pith_coord)
}

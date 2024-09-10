#' Change from relative to fixed pixel coordinate system
#'
#' @param pith_coord Pith coordinates in relative space (x, y)
#' @param im Density matrix
#'
#' @return Pixel coordinates in number of pixels (x, y)
#' @export
#'
#'
relToPixel <- function(pith_coord, im){
  pith_coord["x"] <- pith_coord["x"]*dim(im)[1]
  pith_coord["y"] <- pith_coord["y"]*dim(im)[2]

  return(pith_coord)
}

#' Get coordinates of the end of the path on a CT scan image
#'
#' @param im CT scan image
#' @param pithCoord X,Y coordinates of the pith
#'
#' @return Coordinates of the end of the path
#' @export
#'
#' @examples
#'
#' library(oro.dicom)
#' file_path <- system.file("extdata", "disk.dcm", package = "CTRing")
#' dcm <-  readDICOM(file_path)
#' hdr_df <- dcm$hdr[[1]]
#' image_info <- getImageInfo(hdr = hdr_df)
#'
#' im <- imageToMatrix(dcm$img)
#' im_8bit <- xBitTo8Bit(im, image_info$grayScale)
#' im_dens <- grayToDensity(im_8bit)
#'
#' pith_coord <- detect_pith(im_dens, n_segments = 12, pixel = TRUE, toPlot = FALSE)
#'
#' endPath <- c(472, 284) # manual
#' # not run - endPath <- locatePathEnd(im_dens, pith_coord) # using the image
#'
locatePathEnd <- function(im, pithCoord){
  dev.new()
  image(im)
  points(x =  pithCoord["x"]/dim(im)[1], y =  pithCoord["y"]/dim(im)[2], pch = 19)

  coord <- locator(1)
  coord$x = coord$x * dim(im)[1]
  coord$y = coord$y * dim(im)[2]

  out <- c(x = coord$x, y = coord$y)
  return(out)
}

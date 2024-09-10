#' Convert from 8bit gray scale to density
#'
#' @param im Matrix of CT scan image in 8bit gray scale
#' @param a Intercept of the calibration curve
#' @param b Slope of the calibration curve
#'
#' @return Matrix of density values
#' @export
#'
#' @examples
#' library(oro.dicom)
#' file_path <- system.file("extdata", "disk.dcm", package = "CTRing")
#' dcm <-  readDICOM(file_path)
#' hdr_df <- dcm$hdr[[1]]
#' image_info <- getImageInfo(hdr = hdr_df)
#'
#' im <- imageToMatrix(dcm$img)
#' im_8bit <- xBitTo8Bit(im, image_info$grayScale)
#' range(im_8bit)
#'
#' im_dens <- grayToDensity(im_8bit)
#' range(im_dens)
#'
grayToDensity <- function(im, a = -0.1321, b = 0.01834) {
  im <- a + b*im
  im <- pmax(im, 0)
  return(im)
}

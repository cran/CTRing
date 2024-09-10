#' Convert gray scale from measured bits to 8bit
#'
#' @param im Matrix of values in x bits
#' @param bits Number of bits of the original gray scale
#'
#' @return Matrix of gray scale values in 8bits
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
#' range(im)
#'
#' im_8bit <- xBitTo8Bit(im, image_info$grayScale)
#' range(im_8bit)
#'
xBitTo8Bit <- function(im, bits) {
  nGray <- 2^bits
  im_8bit <- im/ (nGray/2^8)
  return(im_8bit)
}

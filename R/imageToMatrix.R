#' Convert dicom image to matrix
#'
#' @param img Dicom image
#'
#' @return Matrix of image
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
#' dim(im)
#' image(im)
#'
imageToMatrix <- function(img){
  matImage <- lst2arr(img)[,,1]
  return(matImage)
}



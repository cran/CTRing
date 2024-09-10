#' Extract from header of CT scan image grayscale number of bits and pixel size
#'
#' @param hdr Header dataframe
#'
#' @return List with grayscale values, and pixel size
#' @export
#'
#' @examples
#' library(oro.dicom)
#' file_path <- system.file("extdata", "disk.dcm", package = "CTRing")
#' dcm <-  readDICOM(file_path)
#' hdr_df <- dcm$hdr[[1]]
#' getImageInfo(hdr = hdr_df)
#'
getImageInfo <- function(hdr) {
  #gray scale of CT scan
  grayScale <- as.numeric(hdr$value[hdr$name == "BitsStored"])
  size <- hdr$value[hdr$name == "PixelSpacing"]

  # pixel size
  size <- as.numeric(unlist(strsplit(size, " +")))
  pixel_size_x <- size[1]
  pixel_size_y <- size[2]

  list(grayScale = grayScale,
              pixel_size_x = pixel_size_x,
              pixel_size_y = pixel_size_y)

}

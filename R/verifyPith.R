#' Check if pith location is correct
#'
#' @param im Density matrix of image
#' @param pith_coord Pith coordinates
#'
#' @return Corrected pith coordinates
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
#' im_dens <- grayToDensity(im_8bit)
#'
#' pith_coord <- detect_pith(im_dens, n_segments = 12, pixel = TRUE, toPlot = FALSE)
#'
#' pith_coord_checked <- verifyPith(im_dens, pith_coord)
#'
verifyPith <- function(im, pith_coord) {
  dev.new()
  image(im)
  abline(v = pith_coord["x"]/dim(im)[1], col = 'blue')
  abline(h = pith_coord["y"]/dim(im)[2], col = 'blue')

  message("Is pith correctly located (y/n)? ")
  correct <- readline()

  if (correct == "n"){
    message("Please click on pith")
    pith <- locator(1)
    out <- c(x = pith$x, y = pith$y)
    return(out)
  } else if (correct == "y") {
    return(pith_coord)
  } else {
    message("Incorrect answer. No changes to pith coordinates")
    return(pith_coord)
  }
}

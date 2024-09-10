#' Get profile between two points of the CTScan image matrix
#'
#' @param im Density matrix
#' @param imHeader image header
#' @param beginPath X,Y coordinates of the start point of the path
#' @param endPath X,Y coordinates of the start point of the path
#' @param r Profile width
#' @param k Rolling window width, integer
#' @param threshold Threshold value between maximum and minimum density to establish change of ring
#'
#' @return Density profile
#'
#' @export
#' @import xRing
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
#' endPath <- c(472, 284) # manual
#' # not run - endPath <- locatePathEnd(im_dens, pith_coord) # using the image
#'
#' path <- extractProfile(im_dens, image_info, pith_coord, endPath, k = 2, r = 5, threshold = 0.002)
#'
extractProfile <- function(im,
                           imHeader,
                           beginPath,
                           endPath,
                           r = 10,
                           k = 2,
                           threshold = 0.01) {
  x <- c(beginPath[1], endPath[1])
  y <- c(beginPath[2], endPath[2])
  rings <- ringLimits(im, x, y, r, k, threshold, imHeader)
  t1 <- list(distRingChange = rings$distFromPith[rings$ring_limits])
  rings <- append(rings, t1)
  rings <- removeDuplicates(rings)

  return(rings)
}

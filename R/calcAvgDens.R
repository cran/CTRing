#' Calculate average wood, earlywood and latewood density for every ring
#'
#' @param densProfile Density profile
#'
#' @return List with several vectors
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
#' endPath <- c(472, 284) # manual
#' # not run - endPath <- locatePathEnd(im_dens, pith_coord) # using the image
#'
#' path <- extractProfile(im_dens, image_info, pith_coord, endPath, k = 2, r = 5, threshold = 0.002)
#'
#' pathEwLw <- getEwLw(path)
#' plotProfile(pathEwLw)
#' path_avgDens <- calcAvgDens(pathEwLw)
#' names(path_avgDens)
calcAvgDens <- function(densProfile) {
  ringList <- splitAt(densProfile$dens, densProfile$ring_limits)
  densProfile$avgDens <- unlist(lapply(ringList, mean))

  ringList <- splitAt(densProfile$distFromPith, densProfile$ring_limits)
  ringListLag <- splitAt(densProfile$distFromPith, densProfile$ring_limits-1)
  densProfile$rw <- mapply(calcRingWidth, ringList, ringListLag)

  if ("ew_limits" %in% names(densProfile)){
    ewLwList <- splitAt(densProfile$dens, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwAvgDens <- unlist(lapply(ewLwList, mean))

    densProfile$avgDensEw <- ewLwAvgDens[c(TRUE, FALSE)]
    densProfile$avgDensLw <- ewLwAvgDens[c(FALSE, TRUE)]

    ewLwList <- splitAt(densProfile$distFromPith, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwListLag <- splitAt(densProfile$distFromPith, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwRw <- mapply(calcRingWidth, ewLwList, ewLwListLag)

    densProfile$rwEw <- ewLwRw[c(TRUE, FALSE)]
    densProfile$rwLw <- densProfile$rw - densProfile$rwEw
  }
  return(densProfile)
}

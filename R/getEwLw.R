#' Establish the transition point from earlywood to latewood for a series of rings
#'
#' @param densProfile Density profile
#'
#' @return xRingList with EW to LW transition points with transition type added (1: low number of points in ring; 2: inflexion point estimated by polynomial; 3: min or max are out of range; 4: inflexion point close to min or max; 5: convex-concave)
#'
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
#' path <- extractProfile(im_dens, image_info, pith_coord, endPath, k = 2, r = 5, threshold = 0.002)
#'
#' pathEwLw <- getEwLw(path)
#'
#' densityDf <- densityDataFrame(path)
getEwLw <- function(densProfile) {

  # split to individual rings
  dist <- densProfile$distFromPith
  dens <- densProfile$dens
  cutPoints <- densProfile$ring_limits
  ringList <- splitAt(dens, cutPoints)
  distList <- splitAt(dist, cutPoints)

  ringDistList <- mapply(cbind, distList, ringList)

  ewList <- lapply(ringDistList, findEwToLwTransition)

  # transfer from intra-ring index to profile index
  ewDataFrame <- do.call(rbind.data.frame, ewList)
  names(ewDataFrame) <- names(ewList[[1]])

  ewDataFrame$totPoints <- c(0, cumsum(ewDataFrame$nPoints)[-length(ewDataFrame$nPoints)])
  ewDataFrame$limits.EW <- ewDataFrame$totPoints + floor(ewDataFrame$EW)
  # ewDataFrame$limits.LW <- ewDataFrame$totPoints + ceiling(ewDataFrame$EW)

  densProfile$ew_limits <- ewDataFrame$limits.EW[1:length(ewDataFrame$limits.EW)]
  # densProfile$limits.lw <- ewDataFrame$limits.LW[2:length(ewDataFrame$limits.LW)]

  densProfile$transitionType <- ewDataFrame$type

  return(densProfile)
}

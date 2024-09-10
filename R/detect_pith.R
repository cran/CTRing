#' Automatically detect pith in a CT scan image
#'
#' @param im Matrix of the CT scan image
#' @param toPlot Boolean to plot the location of the pith on the image
#' @param n_segments Number of segements used to locate pith
#' @param flag FALSE if pith location is known
#' @param x_0 Estimate of pith location in x
#' @param y_0 Estimate of pith location in y
#' @param n_run_max Maximum number of iterations
#' @param threshold Thershold value for identifying ring transition points
#' @param pixel If TRUE, returns x,y coordinates in pixel numbers, else FALSE returns x,y coordinates in relative values of x and y
#'
#' @return x,y pith coordinates
#'
#' @import xRing
#'
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

detect_pith <- function(im,
                        toPlot = TRUE,
                        n_segments = 25,
                        flag = TRUE,
                        x_0 = 0.5,
                        y_0 = 0.5,
                        n_run_max = 15,
                        threshold = 0.1,
                        pixel = TRUE
) {
  n_runs <- 0
  xy_pith_pixel_previous <- cI(c(x_0, y_0) * dim(im))

  if (toPlot == TRUE) image(im, asp = 1)

  while (flag && n_runs < n_run_max) {
    n_runs <- n_runs + 1
    #single function to get the segments, using degree get from the number of segments
    # if n_segments=6 then 60=360/6 -> get a segment every 60º
    xy_up <- get_segments_up(im, n_segments, x0 = x_0, y0 = y_0)
    xy_right <- get_segments_right(im, n_segments, x0 = x_0, y0 = y_0)
    xy_down <- get_segments_down(im, n_segments, x0 = x_0, y0 = y_0)
    xy_left <- get_segments_left(im, n_segments, x0 = x_0, y0 = y_0)
    xy_all <- rbind(xy_up, xy_right, xy_down, xy_left)

    out <- get_ring_limits_list(xy_all, im, threshold)


    if (toPlot) {
      lapply(out, function(x)
        points(x$x / dim(im)[1], x$y / dim(im)[2], col = "#40404040"))
    }

    segments <-
      out_pair_of_points <- vector("list", length = length(out) - 1)

    for (i in 2:length(out)) {
      xy <- get_closest_point(out[[i]], out[[i - 1]])
      out_pair_of_points[[i - 1]] <- xy
      if (toPlot == TRUE) {
        lines(xy$x / dim(im)[1],
              xy$y / dim(im)[2],
              lwd = 4,
              col = "red2")
      }


      x <- xy$x
      y <- xy$y
      xDelta <- x[2] - x[1]

      yDelta <- y[2] - y[1]


      angle1 <- atan2(-yDelta, -xDelta) * 180.0 / pi
      ang <- (angle1 + c(1,-1) * 90) * pi / 180

      r <- 1000
      x_mean <- mean(x)
      y_mean <- mean(y)
      xVertice <- c((x_mean + r * cos(ang[1])),
                    (x_mean + r * cos(ang[2])))
      yVertice <- c((y_mean + r * sin(ang[1])),
                    (y_mean + r * sin(ang[2])))

      segments[[i - 1]] <-
        list(x = xVertice / dim(im)[1], y = yVertice / dim(im)[2])
    }




    i <- 1
    n_seg <- length(segments)

    possible_piths <-
      matrix(NA, 4 * n_segments ^ 3, 2, dimnames = list(1:(4 * n_segments ^ 3), c("x", "y")))
    p_i <- 0
    while (i < n_seg) {
      j <- i + 1
      while (j <= n_seg) {
        a1 <- diff(segments[[i]]$y) / diff(segments[[i]]$x)
        a2 <- diff(segments[[j]]$y) / diff(segments[[j]]$x)
        b1 <- -segments[[i]]$x[2] * a1 + segments[[i]]$y[2]
        b2 <- -segments[[j]]$x[2] * a2 + segments[[j]]$y[2]
        #
        xy1 <- intersect_2lines(c(b1, a1), c(b2, a2))
        if (length(xy1) < 2)
          xy1 <- c(NA, NA)
        if (any(c(is.infinite(xy1), is.na(xy1))))
          xy1 <- c(NA, NA)
        possible_piths[p_i <- p_i + 1,]   <- xy <- xy1
        j <- j + 1
      }
      i <- i + 1
    }

    possible_piths <- apply(possible_piths, 2, outliers2NA)
    possible_piths <-
      possible_piths[!is.na(possible_piths[, 1]) &
                       !is.na(possible_piths[, 2]),]
    if (toPlot == TRUE) {
      abline(
        v = colMeans(possible_piths)[1],
        h = colMeans(possible_piths)[2],
        col = "green4",
        lty = 2
      )
    }
    xy_pith <- colMeans(possible_piths)
    xy_pith_pixel <- cI(xy_pith * dim(im))
    pith_x_pixel <- xy_pith_pixel[1]
    pith_y_pixel <- xy_pith_pixel[2]
    message(paste0("x=", pith_x_pixel, ", y=", pith_y_pixel))
    if (all((xy_pith_pixel - xy_pith_pixel_previous) == 0))
      flag <- FALSE
    x_0 <- xy_pith[1]
    y_0 <- xy_pith[2]
    xy_pith_pixel_previous <- xy_pith_pixel
  }

  if (pixel == TRUE) {
    out <- xy_pith_pixel
  } else {
    out <- xy_pith
  }

  return(out)
}

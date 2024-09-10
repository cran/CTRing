cI <- function(...) {
  out <- c(...)
  storage.mode(out) <- "integer"
  out
}

outliers2NA <- function(x, probs = c(.25, 0.75)) {
  x <- x[!is.na(x)]
  Q <- stats::quantile(x, probs = probs, na.rm = TRUE)
  iqr <- IQR(x)
  low <-  Q[1] - 1 * iqr # lower range
  up <-  Q[2] + 1 * iqr # upper range
  x[x < low | x > up] <- NA
  x
}

#function to get the closest points between two adjacent sections
get_closest_point <- function(xy1, xy2) {
  r <- 0
  out <- out1 <- rep(NA, nrow(xy1))
  while (r < nrow(xy1)) {
    r <- r + 1
    x0 <- xy1[r, 1]
    y0 <- xy1[r, 2]
    dist_xy <- (xy2[[1]] - x0) ^ 2 + (xy2[[2]] - y0) ^ 2
    out[r] <- which.min(dist_xy)
    out1[r] <- min(dist_xy)
  }
  point_selected <- which.min(out1)
  rbind(xy1[point_selected, ],
        xy2[out[point_selected], ])
}


rollMax <-
  function (x,
            k,
            align = c("center", "left", "right"),
            fill = NA)
  {
    n_s <- length(x) - k + 1
    out <- rep(NA, n_s)
    for (i in seq(1, n_s)) {
      out[i] <- max(x[i + 0:(k - 1)])
    }
    out <- switch(
      match.arg(align),
      left = {
        c(out, rep(fill, k - 1))
      },
      center = {
        c(rep(fill, floor((k - 1) / 2)), out, rep(fill, ceiling((k -
                                                                   1) / 2)))
      },
      right = {
        c(rep(fill, k - 1), out)
      }
    )
    out
  }

dif <- xRing:::dif
border <-  function (x, k = 3, threshold = 0.1)
{
  x.dif <- rollMax(x,
                   k = k,
                   fill = NA,
                   align = "right") +
    rollMax(-x,
            k = k,
            fill = NA,
            align = "center")
  x0 <- which(x.dif > threshold)
  breaks <- unique(c(1, which(dif(x0) != 1), length(x0)))
  out <- NULL
  if (length(breaks) > 1) {
    for (i in 2:length(breaks)) {
      win <- x0[breaks[(i - 1)]:(breaks[(i)] - 1)]
      if (length(win) > 1) {
        win.dif <- dif(x[win])
        out <- c(out, win[which(win.dif == min(win.dif,
                                               na.rm = TRUE))])
      }
    }
  }
  floor(out)
}


lst2arr <- function(lst) {
  nRow_nCol <- dim(lst[[1]])
  n_layers <- length(lst)
  arr <- array(NA, dim = c(nRow_nCol, n_layers))
  colnames(arr) <- colnames(lst[[1]])
  row.names(arr) <- row.names(lst[[1]])
  for (i in 1:n_layers) {
    arr[, , i] <- lst[[i]]
  }
  arr
}

pow <- function(x, y)
  x ^ y


# function to define segments to find tree-ring borders
get_segments_up <- function(im,
                            n,
                            r = 0.95,
                            x0 = 0.5,
                            y0 = 0.5) {
  d_im <- dim(im)
  n <- n + 1
  y <- x <- c(1:(n - 1) * 1 / n)
  out <-
    round(data.frame(
      x0 = rep(x0 * d_im[1], n - 1),
      y0 = c(rep(y0, n - 1)) * d_im[2],
      x1 = c(rev(x)) * d_im[1],
      y1 = c(rep(r, n - 1)) * d_im[2]
    ))
  out[order(out[, 3]),]
}


get_segments_right <- function(im,
                               n,
                               r = 0.95,
                               x0 = 0.5,
                               y0 = 0.5) {
  d_im <- dim(im)
  n <- n + 1
  y <- x <- c(1:(n - 1) * 1 / n)
  round(data.frame(
    x0 = rep(x0 * d_im[1], n - 1),
    y0 = c(rep(y0, n - 1)) * d_im[2],
    x1 = c(rep(r, n - 1)) * d_im[1],
    y1 = c(rev(x)) * d_im[2]
  ))
}

get_segments_down <- function(im,
                              n,
                              r = 0.05,
                              x0 = 0.5,
                              y0 = 0.5) {
  d_im <- dim(im)
  n <- n + 1
  y <- x <- c(1:(n - 1) * 1 / n)
  round(data.frame(
    x0 = rep(x0 * d_im[1], n - 1),
    y0 = c(rep(y0, n - 1)) * d_im[2],
    x1 = c(rev(x)) * d_im[1],
    y1 = c(rep(r, n - 1)) * d_im[2]
  ))
}


get_segments_left <- function(im,
                              n,
                              r = 0.05,
                              x0 = 0.5,
                              y0 = 0.5) {
  d_im <- dim(im)
  n <- n + 1
  y <- x <- c(1:(n - 1) * 1 / n)
  out <-
    round(data.frame(
      x0 = rep(x0 * d_im[1], n - 1),
      y0 = c(rep(y0, n - 1)) * d_im[2],
      x1 = c(rep(r, n - 1)) * d_im[1],
      y1 = c(rev(x)) * d_im[2]
    ))
  out[order(out[, 4]),]
}

get_ring_limits <- function(xy, im, threshold) {
  out <- c()
  r <- 1
  n <- nrow(xy)
  while (r <= n) {
    x <- c(xy[[r, 1]], xy[[r, 3]])
    y <- c(xy[[r, 2]], xy[[r, 4]])
    out <- rbind(out, get_profile(im, x = x, y = y, threshold = threshold))
    r <- r + 1
  }
  out
}

get_ring_limits_list <- function(xy, im, threshold) {
  r <- 1
  n <- nrow(xy)
  out <- vector("list", n)
  while (r <= n) {
    x <- c(xy[[r, 1]], xy[[r, 3]])
    y <- c(xy[[r, 2]], xy[[r, 4]])
    out[[r]] <-  get_profile(im, x = x, y = y, threshold = threshold)
    r <- r + 1
  }
  out
}

intersect_2lines <- function(l1, l2) {
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  c(x, y)
}

#profile of the tree rings between two points (X and Y) using rotated rectangular
ringLimits <- function(im,
                       x,
                       y,
                       r = 10,
                       k = 5,
                       threshold = 0.1,
                       imHeader = list()
) {
  xDelta <- x[2] - x[1]

  yDelta <- y[2] - y[1]

  dist <- sqrt(pow(xDelta, 2) + pow(yDelta, 2) * 1.0)
  profileWidth <- 2 * r + 1
  angle1 <- atan2(-yDelta,-xDelta) * 180.0 / pi
  ang <- (angle1 + c(1, -1) * 90) * pi / 180

  xVertice <- c(round(x[1] + r * cos(ang[1])),
                round(x[1] + r * cos(ang[2])))
  yVertice <- c(round(y[1] + r * sin(ang[1])),
                round(y[1] + r * sin(ang[2])))

  xLenIncr <- xDelta / (dist - 1)
  yLenIncr <- yDelta / (dist - 1)
  xWidthIncr <- (1.0 * xVertice[2] - xVertice[1]) / profileWidth
  yWidthIncr <- (1.0 * yVertice[2] - yVertice[1]) / profileWidth

  xyLengthDelta_x <-  xyLengthDelta_y <- rep(0, dist)
  xyWidthDelta_x <- xyWidthDelta_y <- rep(0, profileWidth)
  xyWidthDelta_x[1] <-  (xVertice[2] - xVertice[1])
  xyWidthDelta_y[1] <-  (yVertice[2] - yVertice[1])

  i <- 2
  while (i < dist) {
    xyLengthDelta_x[i] <- xyLengthDelta_x[i - 1] + xLenIncr
    xyLengthDelta_y[i] <- xyLengthDelta_y[i - 1] + yLenIncr
    i <- i + 1
  }

  i <- 2
  while (i < profileWidth) {
    xyWidthDelta_x[i] <- xyWidthDelta_x[i - 1] - xWidthIncr
    xyWidthDelta_y[i] <- xyWidthDelta_y[i - 1] - yWidthIncr
    i <- i + 1
  }
  xx <- cI(xVertice[1] + xyLengthDelta_x)
  yy <- cI(yVertice[1] + xyLengthDelta_y)
  xx_delta <- cI(xyWidthDelta_x)
  yy_delta <- cI(xyWidthDelta_y)
  r <- 1
  out <- matrix(NA, dist, profileWidth)

  while (r < dist) {
    c <- 1
    while (c <= profileWidth) {
      out[r, c] <- im[xx[r] + xx_delta[c], yy[r] + yy_delta[c]]
      c <- c + 1
    }
    r <- r + 1
  }

  # Tree-ring limits using border function derived from xRing package
  ring_limits <- border(x = rowMeans(out), k, threshold)

  # density profile
  dens <- rowMeans(out)

  # Calculate distance from pith
  distFromPith <- c()
  if (length(imHeader)!=0){
    xCoord <- xx * imHeader$pixel_size_x
    yCoord <- yy * imHeader$pixel_size_y
    distFromPith <- sqrt((xCoord - xCoord[1])^2 + (yCoord - yCoord[1])^2)
  }

  returnList <- list(ring_limits = ring_limits,
                     xx = xx,
                     yy = yy,
                     xx_delta = xx_delta,
                     yy_delta = yy_delta,
                     c = c,
                     dens = dens,
                     distFromPith = distFromPith
                     )
  return(returnList)
}

get_profile <- function(im,
                        x,
                        y,
                        r = 10,
                        k = 5,
                        threshold = 0.1) {

  temp <- ringLimits(im, x, y, r, k, threshold)
  xy <-
    data.frame(x = temp$xx + temp$xx_delta[temp$c / 2], y = temp$yy + temp$yy_delta[temp$c / 2])
  xy[temp$ring_limits, ]
}



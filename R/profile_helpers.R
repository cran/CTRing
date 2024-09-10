#### Density profile helper functions
# calculate distance from pith
calcDistance <- function(ref_point, interest_point) {
  dist <- sqrt((ref_point["x"] - interest_point$x)^2 + (ref_point["y"] - interest_point$y)^2)
  return(dist)
}

# rotate matrix
rotate <- function(x) t(apply(x, 2, rev))



#### density profile helper functions####

# split vector function taken from https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
splitAt <- function(x, pos) {
  pos <- c(1L, pos, length(x) + 1L)
  Map(function(x, i, j) x[i:j], list(x), utils::head(pos, -1L), utils::tail(pos, -1L) - 1L)
}

# polynomial function
poly.fn <- function(x, a, b, c, d) {
  a + b*x + c*x^2 + d*x^3
}

# get 3rd degree polynomial coefficients
getPolyCoefs <- function(x, y) {
  fit.3deg <- lm(y ~ x + I(x^2) + I(x^3))
  polyCoefs <- coef(fit.3deg)
  return(polyCoefs)
}

# find min and max from 3rd degree polynomial
findMax <- function(limits, polyCoefs, fn) {
  formals(fn)$a <- polyCoefs[1]
  formals(fn)$b <- polyCoefs[2]
  formals(fn)$c <- polyCoefs[3]
  formals(fn)$d <- polyCoefs[4]

  opt.res <- optimize(fn, limits, maximum = T)
  opt.res2 <- optimize(fn, limits, maximum = F)

  return(c(min = opt.res2$minimum, max = opt.res$maximum))
}

# inflection point of 3rd degree polynomial function, i.e. 2nd derivative = 0
poly.fn.inf <- function(a, b, c, d) {
  x = -1 * (2*c) / (6*d)
  names(x) <- c()
  return(x)
}

# find index of closest value in a vector
indexClosestVal <- function(x, target) {
  out <- which(abs(x - target) == min(abs(x - target)))
  return(out)
}

# transition function
findEwToLwTransition <- function(ringDist, ringID = NaN, toPlot=F) {

  #ringDist <- ringDistList[[3]]
  ring <- ringDist[,2]
  dist <- ringDist[,1]
  type <- 0 # set value to 0

  nPoints <- length(ring)
  if (nPoints <= 4){
    minMax <- c(min = 1, max = 3)
    inflexPoint <- 2
    type <- 1 # low number of points in ring
  } else {
    polyCoefs <- getPolyCoefs(dist, ring)

    if (is.na(polyCoefs[4])){
      inflexPoint <- stats::quantile(dist, 0.5)
      minMax <- stats::quantile(dist, c(0.1, 0.9))
      names(minMax) <- c("min", "max")
    } else {
      inflexPoint <- poly.fn.inf(a = polyCoefs[1],
                                 b = polyCoefs[2],
                                 c = polyCoefs[3],
                                 d = polyCoefs[4])
      minMax <- findMax(dist, polyCoefs, poly.fn)
    }

    inflexPoint <- indexClosestVal(dist, inflexPoint)
    minMax["min"] <- indexClosestVal(dist, minMax["min"])
    minMax["max"] <- indexClosestVal(dist, minMax["max"])
    type <- 2 # inflexion point estimated by polynomial

    #for cases where inflection point is outside of observed min or max
    if ((inflexPoint <= minMax["min"]) | (inflexPoint >= minMax["max"])) {
      densHatMin <- poly.fn(dist[minMax["min"]],
                            a = polyCoefs[1],
                            b = polyCoefs[2],
                            c = polyCoefs[3],
                            d = polyCoefs[4])
      densHatMax <- poly.fn(dist[minMax["max"]],
                            a = polyCoefs[1],
                            b = polyCoefs[2],
                            c = polyCoefs[3],
                            d = polyCoefs[4])
      densHalf <- densHatMin + 0.5*(densHatMax - densHatMin)
      inflexPoint <- approx(x = c(densHatMin, densHatMax),
                            y = minMax,
                            xout = densHalf)$y
      type <- 3 # min or max are out of range
    }

    #for cases where inflection point is very close to the min or max
    else if (abs(minMax["min"]-inflexPoint) < 2 | (abs(minMax["max"]-inflexPoint) < 2)) {
      densHatMin <- poly.fn(dist[minMax["min"]],
                            a = polyCoefs[1],
                            b = polyCoefs[2],
                            c = polyCoefs[3],
                            d = polyCoefs[4])
      densHatMax <- poly.fn(dist[minMax["max"]],
                            a = polyCoefs[1],
                            b = polyCoefs[2],
                            c = polyCoefs[3],
                            d = polyCoefs[4])
      densHalf <- densHatMin + 0.5*(densHatMax - densHatMin)
      inflexPoint <- approx(x = c(densHatMin, densHatMax),
                            y = minMax,
                            xout = densHalf)$y
      type <- 4 # inflexion point close to min or max
    }

    # for cases where cuvre is convex-concave
    else if (minMax["min"] >= minMax["max"]) {
      minMax['max'] <- length(ring)
      densHatMin <- poly.fn(dist[minMax["min"]],
                            a = polyCoefs[1],
                            b = polyCoefs[2],
                            c = polyCoefs[3],
                            d = polyCoefs[4])
      densHatMax <- poly.fn(dist[minMax['max']],
                            a = polyCoefs[1],
                            b = polyCoefs[2],
                            c = polyCoefs[3],
                            d = polyCoefs[4])
      densHalf <- densHatMin + 0.5*(densHatMax - densHatMin)
      inflexPoint <- approx(x = c(densHatMin, densHatMax),
                            y = minMax,
                            xout = densHalf)$y
      type <- 5 # convex-concave
    }
  }

  if (toPlot == T) {
    if (is.nan(ringID)) {
      title = ''
    } else {
      title = paste("Ring number ",ringID)
    }
    poly.fn.local <- functional::Curry(poly.fn,
                           a = polyCoefs[1],
                           b = polyCoefs[2],
                           c = polyCoefs[3],
                           d = polyCoefs[4])
    plot(x = dist, y = ring, main = title)
    if (nPoints > 3){
      curve(poly.fn.local,
            from = min(dist),
            to = max(dist),
            add = T
      )
    }

    abline(v=dist[inflexPoint], col='green')
    abline(v=dist[minMax], col=c('red', 'blue'))
  }

  out <- c(minMax, inflexPoint, nPoints, type)
  names(out) <- c("min", "max", "EW", "nPoints", "type")
  return(out)
}

#### Density dataframe helper functions####
# transition limits to pointID
limitSeq <- function(tmp){
  seq(from = tmp[1] - tmp[2] + 1, to = tmp[1])
}

limitVector <- function(vect) {
  out <- apply(cbind(vect, c(vect[1],diff(vect))),
               1,
               limitSeq)
  out <- data.frame(pointID=unlist(out),
                    ringAge=rep(seq(length(out)), lengths(out)))
  return(out)
}

# calculate rw
calcRw <- function(df){
  ringStart <- min(df$pith_distance)
  ringEnd <- max(df$pith_distance)
  rw <- ringEnd - ringStart
  return(rw)
}

# calculate eww
calcEww <- function(df){
  ringStart <- min(df$pith_distance)
  ringEnd <- max(df$pith_distance[df$ewAge == min(df$ewAge)])
  eww <- ringEnd - ringStart
  return(eww)
}

# remove duplicate distances from density profile
removeDuplicates <- function(densProfile) {
  freqDistances <- which(duplicated(densProfile$distFromPith))

  for (i in rev(freqDistances)){
    densProfile$xx <- densProfile$xx[-i]
    densProfile$yy <- densProfile$yy[-i]
    densProfile$dens <- densProfile$dens[-i]
    densProfile$distFromPith <- densProfile$distFromPith[-i]
    densProfile$ring_limits[densProfile$ring_limits > i] <- densProfile$ring_limits[densProfile$ring_limits > i] - 1
  }
  return(densProfile)
}

# calculate ring width

calcRingWidth <- function(distance, lagDistance){
  width <- max(distance) - min(lagDistance)
}

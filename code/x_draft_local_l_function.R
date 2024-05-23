# This is code from the paper titled "L-function of geographic flows"
# I've adapted it from the code they shared, but it is not working properly

# It is meant to separate the flows as done in Section 5.2 of:
# "Density-based clustering for bivariate flow data"
# as flows cluster at different scales (lengths)


# TODO: implement my own local L function or
# find a function that takes in a distance matrix, not a ppp


FlowKfunction <- function(flow, o_area, d_area, scale) {
  LocalKfunction <- function(flow, o_area, d_area, scale) {
    N <- length(scale)
    n <- nrow(flow)
    localK <- matrix(0, n, N)
    ox_all <- flow[, 1]
    oy_all <- flow[, 2]
    dx_all <- flow[, 3]
    dy_all <- flow[, 4]

    for (i in 1:n) {
      ones_vector <- rep(1, times = n)
      dist <- pmax(sqrt((flow[i, 1]* ones_vector - ox_all)^2 + (flow[i, 2]* ones_vector - oy_all)^2),
                   sqrt((flow[i, 3]* ones_vector - dx_all)^2 + (flow[i, 4]* ones_vector - dy_all)^2))
      ind <- order(dist)
      kvalue <- 0
      for (j in 1:N) {
        if (j == 1) {
          c <- which(dist <= scale[j])
          if (length(c) >= 2) {
            central_flow <- flow[i, ]
            in_circle_flow <- flow[ind[c[-1]], ]
            weight <- EdgeCorrection(central_flow, in_circle_flow, o_area, d_area)
            kvalue <- kvalue + sum(weight)
            localK[i, j] <- kvalue
          } else {
            localK[i, j] <- 0
          }
        } else {
          c <- which(dist > scale[j-1] & dist <= scale[j])
          if (length(c) >= 1) {
            central_flow <- flow[i, ]
            in_circle_flow <- flow[ind[c], ]
            weight <- EdgeCorrection(central_flow, in_circle_flow, o_area, d_area)
            kvalue <- kvalue + sum(weight)
            localK[i, j] <- kvalue
          } else {
            localK[i, j] <- kvalue
          }
        }
      }
      if (i %% 100 == 0) {
        print(i)
      }
    }
    return(localK)
  }

  localK <- LocalKfunction(flow, o_area, d_area, scale)
  lambda <- nrow(flow) / (polyarea(o_area[, 1], o_area[, 2]) * polyarea(d_area[, 1], d_area[, 2]))
  localK <- localK / lambda
  n <- nrow(localK)
  # N added by Hussein
  N <- length(scale)
  localL <- ((localK / pi^2)^(1/4)) - matrix(scale, n, N, byrow = TRUE)
  K <- cbind(scale, colMeans(localK))
  L <- K
  L[, 2] <- (L[, 2] / pi^2)^(1/4) - scale
  DL <- cbind(L[-n, 1], diff(L[, 2]) / diff(L[, 1]))
  #DL[, 2] <- smooth(DL[, 2])  # Smooth is not defined here, you need to define your own smoothing function
  return(list(K = K, L = L, localL = localL, DL = DL))
}


# Example usage
xv <- c(0, 0, 1, 1, 0)
yv <- c(0, 1, 1, 0, 0)
o_area <- cbind(xv, yv)
d_area <- o_area
scale <- seq(0.01, 0.6, 0.01)
flow <- matrix(runif(4000, 0, 1), ncol = 4)
result <- FlowKfunction(flow, o_area, d_area, scale)


maxx - minx * mayy- miny


#polyarea function

polyarea = function(x, y){
  area = (max(x) - min(x)) * (max(y) - min(y))

  return(area)
}


# prep the data
flow <- od_demand_xyuv %>%
  select(x, y, u, v)
# as matrix
flowm = as.matrix(flow)


o_area <- cbind(flow$x, flow$y)
d_area <- cbind(flow$u, flow$v)

scale <- seq(0.01, 0.3, 0.6)

result <- FlowKfunction(flowm, o_area, d_area, scale)


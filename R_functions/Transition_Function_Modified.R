
#Transition function modified from the gdistance package:

#Includes an automatic slope modifier

setGeneric("transition_mod", function(x, transitionFunction, directions, ...) standardGeneric("transition_mod"))

#' @exportMethod transition
setMethod(
  "transition_mod",
  signature(x = "RasterLayer"),
  function(x, transitionFunction, directions, dem, Use.Slope=FALSE, symm = TRUE, intervalBreaks = NULL) {
    if (.isGlobalLonLat(x)) {
      message("The extent and CRS indicate this raster is a global lat/lon raster. ",
              "This means that transitions going off of the East or West edges ",
              "will 'wrap' to the opposite edge.")
    }
    
    if(is(transitionFunction, "character"))	{
      if(transitionFunction != "barriers" & transitionFunction != "areas") {
        stop("argument transitionFunction invalid")
      }
      if(transitionFunction == "barriers") {
        return(.barriers(x, directions, symm, intervalBreaks))
      }
      if(transitionFunction == "areas") {
        return(.areas(x, directions))
      }
    } else {
      if (directions %in% c(4, 8)) {
        if (.isGlobalLonLat(x)) {
          message("Global lat/lon rasters are not supported under new ",
                  "optimizations for 4 and 8 directions with custom transition ",
                  "functions. Falling back to old method.")
          return(.TfromR_old(x, transitionFunction, directions, symm))
        } else if(Use.Slope){ 
          
          return(.TfromR_new_dem(x, transitionFunction, directions, symm, dem))
        
          } else{
          
          return(.TfromR_new(x, transitionFunction, directions, symm))
        }
      } else {
        return(.TfromR_old(x, transitionFunction, directions, symm))
      }
    }
  }
)


#Modification to include a slope modifier for upslope vs.downslope movement
.tr_vals_simple_dem <- function(data, fun, dir, sym, dem) {
    
    nrows <- terra::nrow(data)
    ncols <- terra::ncol(data)
    
    if (sym) {
      result <- numeric(nrows * ncols * dir / 2)
    } else {
      result <- numeric(nrows * ncols * dir)
    }
    index <- 0
    
    if (dir == 4) {
      dir <- c(2, 4, 6, 8)
    } else if (dir == 8) {
      dir <- c(1:4, 6:9)
    } else if (dir == 16) {
      # TODO 16 directions
      stop("16 directions not reimplemented yet")
    } else {
      stop("Bad directions", call. = FALSE)
    }
    
    if (sym) dir <- dir[1:(length(dir)/2)]
    
    for (r in 1:nrows) {
      # Get focal values for current row
      vals <- terra::focalValues(data, 3, r, 1)
      # Get corresponding elevation values
      elev_vals <- terra::focalValues(dem, 3, r, 1)
      
      for (c in 1:ncols) {
        v <- vals[c, 5]
        v_elev <- elev_vals[c, 5]
        
        for (d in dir) {
          index <- index + 1
          
          if (is.finite(v) & is.finite(vals[c, d])) {
            # Calculate elevation change
            target_elev <- elev_vals[c, d]
            elev_change <- target_elev - v_elev
            
            if (is.na(elev_change) || is.infinite(elev_change)) {
            
                result[index] <- fun(c(vals[c, d], v))
                
            } else if(elev_change > 0) {
              
              # Upslope: 
              slope_multiplier <- 1 + (tan(atan(elev_change / res(dem)[1])))^2
              result[index] <- fun(c(vals[c, d], v)) * slope_multiplier
            
              } else if (elev_change < 0) {
              
              # Downslope: 
                slope_multiplier <- 1 / (1 + (tan(atan(abs(elev_change) / res(dem)[1])))^2)              
                result[index] <- fun(c(vals[c, d], v)) * slope_multiplier
              
            } else {
              # Flat terrain: use original function result
              
              result[index] <- fun(c(vals[c, d], v))
            }
          }
        }
      }
    }
    result
  }
  
# Modified from https://github.com/andrewmarx/samc/blob/1d9973882477180fa90ca7a570c3a0db8cadfbe2/R/internal-functions.R#L332
.tr_vals_simple <- function(data, fun, dir, sym) {
  
  nrows <- terra::nrow(data)
  ncols <- terra::ncol(data)
  
  if (sym) {
    result <- numeric(nrows * ncols * dir / 2)
  } else {
    result <- numeric(nrows * ncols * dir)
  }
  index <- 0
  
  if (dir == 4) {
    dir <- c(2, 4, 6, 8)
  } else if (dir == 8) {
    dir <- c(1:4, 6:9)
  } else if (dir == 16) {
    # TODO 16 directions
    stop("16 directions not reimplemented yet")
  } else {
    stop("Bad directions", call. = FALSE)
  }
  
  if (sym) dir <- dir[1:(length(dir)/2)]
  
  for (r in 1:nrows) {
    vals <- terra::focalValues(data, 3, r, 1)
    
    for (c in 1:ncols) {
      v <- vals[c, 5]
      
      for (d in dir) {
        index <- index + 1
        
        if (is.finite(v) & is.finite(vals[c, d])) {
          result[index] <- fun(c(vals[c, d], v))
        }
      }
    }
  }
  
  result
}


# Modified from https://github.com/andrewmarx/samc/blob/1d9973882477180fa90ca7a570c3a0db8cadfbe2/R/internal-functions.R#L13
#modification of new function for slope
.TfromR_new_dem <- function(x, tr_fun, dir, sym, dem) {
  extent <- raster::extent(x)
  crs <- raster::projection(x, asText=FALSE)
  
  #Trim NA
  x <-terra::rast(x)
  dem_r <- terra::rast(dem)
  
  tr_vals <- .tr_vals_simple_dem(x, tr_fun, dir, sym,dem_r)
  
  nedges <- sum(tr_vals != 0)
  
  nrows <- terra::nrow(x)
  ncols <- terra::ncol(x)
  ncells <- terra::ncell(x)
  cell_nums <- terra::cells(x)
  
  rm(x)
  
  if (dir == 4) {
    dir_vec <- c(1:4)
    offsets <- c(-ncols, -1, 1, ncols)
  } else if (dir == 8) {
    dir_vec <- c(1:8)
    offsets <- c(-ncols - 1, -ncols, -ncols + 1, -1, 1, ncols - 1, ncols, ncols + 1)
  } else if (dir == 16) {
    # TODO 16 directions
    stop("16 directions not reimplemented yet")
  } else {
    stop("Bad directions", call. = FALSE)
  }
  
  dir_sym <- dir
  
  if (sym) {
    dir_sym <- dir/2
    dir_vec <- dir_vec[1:dir_sym]
    offsets <- offsets[1:dir_sym]
  }
  
  
  mat_p <- integer(ncells + 1)
  mat_x <- numeric(nedges)
  mat_i <- integer(nedges)
  
  index = 0
  
  for (i in 1:length(tr_vals)) {
    if (tr_vals[i] != 0) {
      index = index + 1
      
      # TODO modify for rasters that wrap across date line
      d <- ((i - 1) %% dir_sym) + 1
      p2 <- ((i - 1) %/% dir_sym) + 1
      p1 <- p2 + offsets[d]
      
      mat_p[p2 + 1] <- mat_p[p2 + 1] + 1
      mat_x[index] <- tr_vals[i]
      mat_i[index] <- p1
    }
  }
  
  for (i in 2:length(mat_p)) {
    mat_p[i] <- mat_p[i] + mat_p[i - 1]
  }
  
  mat_i <- mat_i - 1
  
  if (!all(mat_x >= 0)) {
    warning("transition function gives negative values")
  }
  
  if (sym) {
    mat <- new("dsCMatrix")
  } else {
    mat <- new("dgCMatrix")
  }
  
  mat@Dim <- c(as.integer(ncells), as.integer(ncells))
  
  mat@p <- as.integer(mat_p)
  mat@i <- as.integer(mat_i)
  mat@x <- mat_x
  
  new("TransitionLayer",
      nrows = as.integer(nrows),
      ncols = as.integer(ncols),
      extent = extent,
      crs = crs,
      transitionMatrix = mat,
      transitionCells = 1:ncells,
      matrixValues = "conductance")
}

.TfromR_new <- function(x, tr_fun, dir, sym) {
    extent <- raster::extent(x)
    crs <- raster::projection(x, asText=FALSE)
    
    x <- terra::rast(x)
    
    tr_vals <- .tr_vals_simple(x, tr_fun, dir, sym)
    
    nedges <- sum(tr_vals != 0)
    
    nrows <- terra::nrow(x)
    ncols <- terra::ncol(x)
    ncells <- terra::ncell(x)
    cell_nums <- terra::cells(x)
    
    rm(x)
    
    if (dir == 4) {
      dir_vec <- c(1:4)
      offsets <- c(-ncols, -1, 1, ncols)
    } else if (dir == 8) {
      dir_vec <- c(1:8)
      offsets <- c(-ncols - 1, -ncols, -ncols + 1, -1, 1, ncols - 1, ncols, ncols + 1)
    } else if (dir == 16) {
      # TODO 16 directions
      stop("16 directions not reimplemented yet")
    } else {
      stop("Bad directions", call. = FALSE)
    }
    
    dir_sym <- dir
    
    if (sym) {
      dir_sym <- dir/2
      dir_vec <- dir_vec[1:dir_sym]
      offsets <- offsets[1:dir_sym]
    }
    
    
    mat_p <- integer(ncells + 1)
    mat_x <- numeric(nedges)
    mat_i <- integer(nedges)
    
    index = 0
    
    for (i in 1:length(tr_vals)) {
      if (tr_vals[i] != 0) {
        index = index + 1
        
        # TODO modify for rasters that wrap across date line
        d <- ((i - 1) %% dir_sym) + 1
        p2 <- ((i - 1) %/% dir_sym) + 1
        p1 <- p2 + offsets[d]
        
        mat_p[p2 + 1] <- mat_p[p2 + 1] + 1
        mat_x[index] <- tr_vals[i]
        mat_i[index] <- p1
      }
    }
    
    for (i in 2:length(mat_p)) {
      mat_p[i] <- mat_p[i] + mat_p[i - 1]
    }
    
    mat_i <- mat_i - 1
    
    if (!all(mat_x >= 0)) {
      warning("transition function gives negative values")
    }
    
    if (sym) {
      mat <- new("dsCMatrix")
    } else {
      mat <- new("dgCMatrix")
    }
    
    mat@Dim <- c(as.integer(ncells), as.integer(ncells))
    
    mat@p <- as.integer(mat_p)
    mat@i <- as.integer(mat_i)
    mat@x <- mat_x
    
    new("TransitionLayer",
        nrows = as.integer(nrows),
        ncols = as.integer(ncols),
        extent = extent,
        crs = crs,
        transitionMatrix = mat,
        transitionCells = 1:ncells,
        matrixValues = "conductance")
  }
  

#Old function for lat and long
.TfromR_old <- function(x, transitionFunction, directions, symm) {
  tr <- new("TransitionLayer",
            nrows = as.integer(raster::nrow(x)),
            ncols = as.integer(raster::ncol(x)),
            extent = raster::extent(x),
            crs = raster::projection(x, asText = FALSE),
            transitionMatrix = Matrix(0, raster::ncell(x), raster::ncell(x)),
            transitionCells = 1:raster::ncell(x))
  
  transitionMatr <- transitionMatrix(tr)
  Cells <- which(!is.na(raster::getValues(x)))
  adj <- raster::adjacent(x, cells = Cells, pairs = TRUE,
                          target = Cells,
                          directions = directions)
  if(symm) { adj <- adj[adj[, 1] < adj[, 2], ] }
  dataVals <- cbind(raster::getValues(x)[adj[, 1]],
                    raster::getValues(x)[adj[, 2]])
  transition.values <- apply(dataVals, 1, transitionFunction)
  
  if(!all(transition.values >= 0)){
    warning("transition function gives negative values")
  }
  
  transitionMatr[adj] <- as.vector(transition.values)
  if(symm) {
    transitionMatr <- forceSymmetric(transitionMatr)
  }
  transitionMatrix(tr) <- transitionMatr
  matrixValues(tr) <- "conductance"
  
  return(tr)
}

#barriers function
.barriers <- function(x, directions, symm, intervalBreaks) {
  Xlayer <- new("TransitionLayer",
                nrows = as.integer(nrow(x)),
                ncols = as.integer(ncol(x)),
                extent = extent(x),
                crs = projection(x, asText = FALSE),
                transitionMatrix = Matrix(0, ncell(x), ncell(x)),
                transitionCells = 1:ncell(x))
  matrixValues(Xlayer) <- "resistance"
  Xstack <- as(Xlayer, "TransitionStack") * 0
  #Xstack@transition <- vector(list,...)
  
  if(x@data@isfactor) {
    
    vals <- unlist(x@data@attributes[[1]])
    n <- length(vals)
    
    if(symm) {
      maxn <- (n^2 - n)/2
      for(i in 1:maxn) {
        j <- .matrIndex(i, n)
        XlayerNew <- Xlayer
        cells1 <- which(getValues(x) == vals[j[1]])
        cells2 <- which(getValues(x) == vals[j[2]])
        adj1 <- adjacent(x, cells = cells1, pairs = TRUE,
                         target = cells2,
                         directions = directions)
        adj2 <- adjacent(x, cells = cells2,
                         pairs = TRUE,
                         target = cells1,
                         directions = directions)
        adj <- rbind(adj1, adj2)
        XlayerNew[adj] <- 1
        Xstack <- stack(Xstack, XlayerNew)
      }
    } else {
      maxn <- (n^2 - n)/2
      for(i in 1:maxn) {
        j <- .matrIndex(i,n)
        XlayerNew1 <- Xlayer
        XlayerNew2 <- Xlayer
        cells1 <- which(getValues(x) == vals[j[1]])
        cells2 <- which(getValues(x) == vals[j[2]])
        adj1 <- adjacent(x,
                         cells = cells1,
                         pairs = TRUE,
                         target = cells2,
                         directions = directions)
        adj2 <- adjacent(x,
                         cells = cells2,
                         pairs = TRUE,
                         target = cells1,
                         directions = directions)
        XlayerNew1[adj1] <- 1
        XlayerNew2[adj2] <- 1
        Xstack <- stack(Xstack, XlayerNew1, XlayerNew2)
      }
    }
  } else {
    Xmin <- transition(x, min, directions)
    Xmax <- transition(x, max, directions)
    index1 <- adjacent(x,
                       cells = 1:ncell(x),
                       pairs = TRUE,
                       target = 1:ncell(x),
                       directions = directions)
    XminVals <- Xmin[index1]
    XmaxVals <- Xmax[index1]
    
    if(symm == TRUE) {
      for(i in 1:length(intervalBreaks)) {
        index2 <- index1[XminVals < intervalBreaks[i] & XmaxVals > intervalBreaks[i], ]
        XlayerNew <- Xlayer
        XlayerNew[index2] <- 1
        Xstack <- stack(Xstack, XlayerNew)
      }
    }
    
    if(symm=="up" | symm=="down") {
      stop("not implemented yet")
    }
  }
  
  Xstack <- Xstack[[2:nlayers(Xstack)]]
  return(Xstack)
}

#areas function
.areas <- function(x, directions) {
  
  Xlayer <- new("TransitionLayer",
                nrows = as.integer(nrow(x)),
                ncols = as.integer(ncol(x)),
                extent = extent(x),
                crs = projection(x, asText = FALSE),
                transitionMatrix = Matrix(0, ncell(x), ncell(x)),
                transitionCells = 1:ncell(x))
  
  matrixValues(Xlayer) <- "resistance"
  Xstack <- as(Xlayer, "TransitionStack") * 0
  #Xstack@transition <- vector(list,...)
  
  if(x@data@isfactor) {
    vals <- unlist(x@data@attributes[[1]])
    n <- length(vals)
    
    for(i in 1:n) {
      transitionFunction <- function(v) { return(sum(v == i) / 2) }
      XlayerNew <- .TfromR_old(x, transitionFunction, directions, symm = TRUE) # TODO integrate with new optimizations
      Xstack <- stack(Xstack, XlayerNew)
    }
  } else {
    warning("not yet implemented for raster with non-factor",
            " variables. Contact author.")
  }
  
  Xstack <- Xstack[[2:nlayers(Xstack)]]
  
  return(Xstack)
}

#global long lat
.isGlobalLonLat <- function(x) {
  res <- FALSE
  tolerance <- 0.1
  scale <- xres(x)
  if (isTRUE(all.equal(xmin(x), -180, tolerance = tolerance, scale = scale)) &
      isTRUE(all.equal(xmax(x),  180, tolerance = tolerance, scale = scale))) {
    if (couldBeLonLat(x, warnings = FALSE)) {
      res <- TRUE
    }
  }
  res
}

#' @export
setMethod(
  "transition_mod",
  signature(x = "RasterBrick"),
  def = function(x, transitionFunction = "mahal", directions) {
    if (transitionFunction != "mahal") {
      stop("only Mahalanobis distance method implemented for RasterBrick \n")
    }
    
    xy <- cbind(1:ncell(x), getValues(x))
    xy <- na.omit(xy)
    
    dataCells <- xy[, 1]
    
    adj <- adjacent(x, cells = dataCells, pairs = TRUE,
                    target = dataCells, directions = directions)
    
    x.minus.y <- raster::getValues(x)[adj[, 1], ] - raster::getValues(x)[adj[, 2], ]
    
    cov.inv <- solve(cov(xy[, -1]))
    
    mahaldistance <- apply(x.minus.y, 1, function(x) { sqrt((x%*%cov.inv)%*%x) })
    mahaldistance <- mean(mahaldistance)/(mahaldistance + mean(mahaldistance))
    
    transitiondsC <- new("dsCMatrix",
                         p = as.integer(rep(0, ncell(x) + 1)),
                         Dim = as.integer(c(ncell(x), ncell(x))),
                         Dimnames = list(as.character(1:ncell(x)), as.character(1:ncell(x))))
    
    transitiondsC[adj] <- mahaldistance
    
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    
    tr <- new("TransitionLayer",
              transitionMatrix = transitiondsC,
              nrows = nr,
              ncols = nc,
              extent = extent(x),
              crs = projection(x, asText = FALSE),
              matrixValues = "conductance")
    
    return(tr)
  }
)
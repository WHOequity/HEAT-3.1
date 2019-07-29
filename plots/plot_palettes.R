

pal_ordered <- function (n, col, l = c(20, 100), power = 1,
                         fixup = TRUE, gamma = NULL, alpha = 1, ...)
{
  
  vals <- switch(col,
                 "green" = list(h = c(120, 0), c. = c(80, 100)),
                 "turquoise" = list(h = 180, c. = c(80, 40)),
                 "blue" = list(h = 260, c. = c(80, 100)),
                 "purple" = list(h = 280, c. = c(80, 100)),
                 "pink" = list(h = 320, c. = c(80, 100)),
                 "red" = list(h = 10, c. = c(80, 100)),
                 "orange" = list(h = 30, c. = c(80, 100)),
                 "yellow" = list(h = 60, c. = c(80, 100)),
                 "lightgreen" = list(h = 100, c. = c(80, 100)),
                 "lightblue" = list(h = 210, c. = c(80, 40)),
                 NULL)
  
  if(is.null(vals)) stop("Wrong choice of color palette")
  
  h <- vals$h
  c. <- vals$c.
  
  
  if (!is.null(gamma))
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L)
    return(character(0L))
  c <- rep(c., length.out = 2L)
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, 0, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L],
                       C = c[2L] - diff(c) * rval^power[1L], H = h[1L]), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}




#plot(1:10, 1:10, pch = 16, cex = 1.3, col = pal_nonordered(10, "blueorange"))

pal_nonordered <- function (n, col, c = 100, fixup = TRUE,
                            gamma = NULL, alpha = 1, ...)
{
  
  
  vals <- switch(col,
                 "redgreen" = list(l = 70, start = -360, end = -200),
                 "orangeblue" = list(l = 60, start = -310, end = -150),
                 "greenpurple" = list(l = 50, start = -260, end = -100),
                 "turquoisepink" = list(l = 70, start = -210, end = -50),
                 "bluered" = list(l = 60, start = -160, end = 0),
                 "blueorange" = list(l = 50, start = -110, end = 50),
                 "orangeblue" = list(l = 70, start = -60, end = -100),
                 NULL)
  
  if(is.null(vals)) stop("Wrong choice of color palette")
  
  l <- vals$l
  start <- vals$start
  end <- vals$end
  
  if (!is.null(gamma))
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L)
    return(character(0L))
  rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)),
              fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}



# taken from package colorspace
hex <- function (from, gamma = NULL, fixup = FALSE) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  coords <- as(from, "sRGB")@coords
  rval <- .Call("sRGB_to_RColor", coords, fixup, PACKAGE = "colorspace")
  if (!is.null(dnam <- attr(coords, "dimnames"))) 
    names(rval) <- dnam[[1L]]
  return(rval)
}

# taken from package colorspace
polarLUV <- function (L, C, H, names) 
{
  if (missing(L)) 
    return(new("polarLUV"))
  if (missing(names)) 
    names = dimnames(L)[[1]]
  coords = cbind(L, if (missing(C)) 
    NULL
    else C, if (missing(H)) 
      NULL
    else H)
  dimnames(coords) = list(names, c("L", "C", "H"))
  new("polarLUV", coords = coords)
}


.rdata[['pal_single']] <- c("green" = "#007300",
                             "purple" = "#7D00EC",
                             "yellow" = "#FFDA3D",
                             "pink" = "#D65EE9",
                             "blue" = "#003AC3",
                             "red" = "#CE4951",
                             "turquoise" = "#00B39C",
                             "orange" = "#DE7429"
                             
                             #"lightgreen" = "#B0D500",
                             #"lightblue" = "#00AECF"
)


# from smwrBase (with changes)

movingAve <- function(x, span=3, order=0, pos="center") {
  ## Coding history:
  ##    2009Aug17 DLLorenz Original Coding
  ##    2012May24 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##
  ## get the correct position
  pos <- match.arg(pos, c("center", "begin", "end", "leading", "trailing"))
  if(pos == "leading")
    pos <- "begin"
  else if(pos == "trailing")
    pos <- "end"
  if(order >= span)
    stop("the value for order must be less than the value for span")
  ## Construct the filter matrix
  ## Note that for order greater than 0, the construction of the matrix is
  ##  based on linear model theory
  if(order > 0) {
    X <- cbind(1, poly(seq(span), order))
    filMat <- X %*% solve(crossprod(X)) %*% t(X)
  }
  else
    filMat <- matrix(1/span, ncol=span, nrow=span)
  if(span > length(x)) # need to protect against failure in filter
    retval <- rep(NA_real_, length(x))
  else if(pos == "center")
    retval <- stats::filter(x, filMat[span + 1 - trunc((span + 1)/2),])
  else if(pos == "begin")
    retval <- rev(stats::filter(rev(x), filMat[1L,], sides=1))
  else # Must be end
    retval <- stats::filter(x, filMat[1L,], sides=1)
  return(as.vector(retval))
}
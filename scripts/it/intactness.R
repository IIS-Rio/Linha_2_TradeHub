
intactness = function(hfi,
                      gamma = -log(c(0.2))/4,
                      beta = -0.2,
                      z = 0.5,
                      n = 7,
                      dw = 1e-9) {

  # Create a matrix representing the focal window (with a n_pixels * res radius)
  res = xres(hfi) / 1e3 # raster´s resolution in km
  rad = n * res # radius considered in move window
  foc_size = 2*n + 1
  foc_index = trunc((foc_size + 1)/2)
  foc.w = matrix(NA, nrow = foc_size, ncol = foc_size)

  for (i in 1:foc_size){
    for (j in 1:foc_size){
      d = sqrt((res * (i - foc_index))^2 + (res * (j - foc_index))^2)
      if (d <= rad) foc.w[i,j] = exp(beta * d)
    }
  }

  # Convert that matrix to a vector for subsequent calculations:
  dvec <- as.vector(foc.w)

  # Habitat quality (must lie between 0 and 1)
  qm <- matrix(exp(-gamma * values(hfi)), nrow = nrow(hfi), ncol = ncol(hfi), byrow=T)

  # Q prime derivative
  qp <- matrix(NA, nrow = nrow(hfi), ncol = ncol(hfi))

  # Focal distance
  foc_dist = foc_index - 1

  # Derivative of Q' calculation -----------------------------------------------
  for (r in foc_index:(nrow(qm) - foc_index)){
    #if (r %% 100 == 0) cat(r, '\r')
    for (c in foc_index:(ncol(qm) - foc_index)){
      if (!is.na(qm[r,c])){
        x = as.vector(qm[(r-foc_dist):(r+foc_dist), (c-foc_dist):(c+foc_dist)])
        qp[r, c] <- (sum(((qm[r,c] + dw) * x)^z * dvec, na.rm=TRUE) - sum(((qm[r,c]) * x)^z * dvec, na.rm=TRUE)) / (dw * sum(dvec[which(!is.na(x))], na.rm=TRUE))
      }
    }
  }

  # Result layer
  it = hfi
  values(it) <- as.vector(t(qp))

  return(it)
}
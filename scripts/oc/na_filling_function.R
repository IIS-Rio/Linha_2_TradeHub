# function to fill Nas and calculate conservation oc



# gap.fill = function(r, max.it, tol = 1e-2, verbose=FALSE) {
#   gaps = which(is.na(r)[])
#   r.filled = r
#   #w = matrix(c(0,0.25,0,0.25,0,0.25,0,0.25,0), nc=3, nr=3)
#   w = matrix(rep(1/8, 9), ncol=3, nrow=3)
#   i = 0
#   while(i < max.it) {
#     i = i + 1
#     new.vals = focal(r.filled, w=w, na.rm=TRUE)[gaps]
#     max.residual = suppressWarnings(max(abs(r.filled[gaps] - new.vals), na.rm = TRUE))
#     if (verbose) print(paste('Iteration', i, ': residual = ', max.residual))
#     r.filled[gaps] = new.vals
#     if (is.finite(max.residual) & max.residual <= tol) break
#   }
#   return(r.filled)
# }


gap.fill = function(r, max.it=10^4, tol = 1e-2, min_threshold = NA, verbose=FALSE) {
  gaps = which(is.na(r)[])
  r.filled = r
  w = matrix(rep(1/8, 9), ncol=3, nrow=3)  # Updated window with equal weights
  i = 0
  while(i < max.it) {
    i = i + 1
    new.vals = focal(r.filled, w=w, na.rm=TRUE)[gaps]
    
    # Ensure the minimum filled value is above the specified threshold
    if (!is.na(min_threshold)) {
      min_non_na_value = min(r.filled[!is.na(r.filled)], na.rm = TRUE)
      new.vals[new.vals < min_threshold] = min_threshold
    }
    
    max.residual = suppressWarnings(max(abs(r.filled[gaps] - new.vals), na.rm = TRUE))
    if (verbose) print(paste('Iteration', i, ': residual = ', max.residual))
    r.filled[gaps] = new.vals
    if (is.finite(max.residual) & max.residual <= tol) break
  }
  return(r.filled)
}

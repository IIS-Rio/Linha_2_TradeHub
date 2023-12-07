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


# gap.fill = function(r, max.it=10^4, tol = 1e-2, min_threshold = NA, verbose=FALSE) {
#   gaps = which(is.na(r)[])
#   r.filled = r
#   w = 3  # window with equal weights
#   i = 0
#   while(i < max.it) {
#     i = i + 1
#     new.vals = focal(r.filled,na.policy="only", w=w, na.rm=TRUE,fun=mean)[gaps]
#     
#     # Ensure the minimum filled value is above the specified threshold
#     if (!is.na(min_threshold)) {
#       min_non_na_value = min(r.filled[!is.na(r.filled)], na.rm = TRUE)
#       new.vals[new.vals < min_threshold] = min_threshold
#     }
#     
#     max.residual = suppressWarnings(max(abs(r.filled[gaps] - new.vals), na.rm = TRUE))
#     if (verbose) print(paste('Iteration', i, ': residual = ', max.residual))
#     r.filled[gaps] = new.vals
#     if (is.finite(max.residual) & max.residual <= tol) break
#   }
#   return(r.filled)
# }


fill_nas = function(r, w=matrix(1, ncol=3, nrow=3), verbose=FALSE) {
  while (sum(is.na(r[]))) {
    new_vals = focal(r, w=w, na.rm=TRUE, fun=mean,na.policy="only",fillvalue=-1)
    r[is.na(r)] = new_vals[is.na(r)]
    if (verbose) print("Filling NAs...")
  }
  return(r)
}


# fill_nas <- terra::freq(r_fill, value = NA)
# 
# while(na_count$count != 0){
#   
#   r_fill <- terra::focal(r_fill, 5, mean, na.policy="only", na.rm=TRUE)  
#   na_count <- terra::freq(r_fill, value = NA)
# }
# Create and document the function
#' AUC calculation
#'
#' Computes the area under the concentration time curve
#' and above 'threshold'. In order to have the normal AUC threshold 
#' should be set to 0
#'
#' @param x          Time vector.
#' @param y          Observation vector matching with time vector
#' @param t0         Start time for AUC calculation
#' @param t1         End time for AUC calculation
#' @param threshold  Lower threshold. Any value below threshold will be set to threshold for AUC calculation
#' @param method     Method for AUC calculation: "linear" or "linear up/log down" 
#' @param return      
#' @param examples
#' trapz(x = time, y = concentration, t0 = 0, t1 = 100, threshold = 0.01,
#'                     method = "linear up/log down")
#'
#'@export
trapz <- function(x, y, t0, t1, threshold, method = "linear") {
  
  # Data must be sorted by x
  ind.order <- order(x)
  x     <- x[ind.order]
  y     <- y[ind.order]
  
  # give warning in case your time lower or upper bounds are out of bounds with the available data
  if(min(x) > t0 | max(x) < t1){
    if(min(x) > t0){
      warning(paste0("The inputted time lower bound for AUC calculation is below the",
                     " minimum time available in the given data frame.\n",
                     "Time lower bound was automatically set to ",min(x)))
    }
    if(max(x) < t1){
      warning(paste0("The inputted time upper bound for AUC calculation is above the",
                     " maximum time available in the given data frame.\n",
                     "Time upper bound was automatically set to ",max(x)))
    }
  }
  
  x.sel <- x[x >= t0 & x <= t1]
  y.sel <- y[x >= t0 & x <= t1]
  
  # Set data below threshold to threshold
  if(all(y.sel<threshold)) return(0)
  y.sel <- ifelse(y.sel<threshold, threshold, y.sel)
  
  x.s   <- x.sel[1]
  x.e   <- x.sel[length(x.sel)]
  
  if(length(x.sel) == 0){
    AUC   <- 0
  } else if (length(x.sel) == 1){
    AUC   <- 0
  } else {
    
    if (method == "linear") {
      idx   <- 2:length(x.sel)
      AUC   <- as.double( (x.sel[idx] - x.sel[idx-1]) %*% (y.sel[idx] + y.sel[idx-1])) / 2
      AUC   <- AUC - (x.e - x.s) * threshold
      
    } else if (method == "linear up/log down") {
      AUC   <- 0
      # Logarithmic if concentration is going down
      for (idx in seq(from = 2, to = length(x.sel))) {
        if (y.sel[idx] < y.sel[idx-1]) {
          AUC.temp <- as.double( (x.sel[idx] - x.sel[idx-1]) *
                                   (y.sel[idx-1] - y.sel[idx]) / (log(y.sel[idx-1]) - log(y.sel[idx])))
          AUC      <- AUC + AUC.temp
          # Linear otherwise
        } else {
          AUC.temp <- as.double( (x.sel[idx] - x.sel[idx-1]) * (y.sel[idx] + y.sel[idx-1])) / 2
          AUC      <- AUC + AUC.temp
        }
      }
      AUC   <- AUC - (x.e - x.s) * threshold
      
    } else {
      print("Error: specified AUC method is not defined")
      return(0)
    }
    
  }
  
  if(is.na(AUC)){
    print("Warning: AUC is NaN")
  }
  
  return (AUC)
}
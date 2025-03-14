#' Calculate half-life from 1-compartmental models
#'
#' This function will calculate the \eqn{\alpha} half-life for 1-cpt models. The time units for the parameters given will be used for the units of the half-life.
#' The user needs to provide the \code{V} with either \code{kel} or \code{Cl}. If both \code{kel} and \code{Cl} are provided, the kel will be used for half-life calculation.
#'
#' @param V Volume of distribution for the central compartment.
#' @param Cl Clearance from the central compartment.
#' @param kel Elimination rate constant from central compartment.
#'
#' @return The calculated \eqn{\alpha} half-life as a single value
#' @export
#' @examples
#' alpha.T12(V   = 3,    # in L
#'           Cl  = 16)   # in L/day
#'Output: [1] 0.1299651  # in days
#' alpha.T12(V   = 3     # in L
#'           kel = 5.33) # in 1/day
#'Output: [1] 0.1300464  # in days
#'
alpha.T12 <- function(V, Cl, kel){

  if(missing(kel) & (!missing(Cl)&&!missing(V))){
    kel <- Cl/V
  }

  T12_alpha <- log(2)/kel

  return(T12_alpha)
}





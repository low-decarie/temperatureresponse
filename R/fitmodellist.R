#' Fit model list
#'
#' @description Fits list of models (all models in package by default)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement (for example growth rate, but could also be abundance)
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical should the model object be returned
#' @param models list of strings of equations to be fit such as paste0("equ",4:15)
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, fitmodellist(temp=temp, rate=rate))

fitmodellist <- function(temp,rate, augment=F,return_fit=F, models=paste0("equ",4:15)){

  output <- do.call(dplyr::bind_rows,
                           lapply(models,
                                  do.call,
                                  list(temp=temp,
                                       rate=rate,
                                       augment=augment,
                                       return_fit=return_fit)))

  return(output)
}

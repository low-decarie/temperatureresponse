#' Equation 14
#'
#' @description Equation from Kamykowski (1985)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#'
equ14 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({
    fit = minpack.lm::nlsLM(rate ~ a*(1-exp(-b*(temp-Tmin)))*(1-exp(-c*(Tmax-temp))),
              control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=max(rate),
                           b=1,
                           c=1,
                           Tmax=max(temp),
                           Tmin=min(temp)))


    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    c <- output$estimate[output$term=="c"]
    Tmax <- output$estimate[output$term=="Tmax"]
    Tmin <- output$estimate[output$term=="Tmin"]
    f_equ= function(t){a*(1-exp(-b*(t-Tmin)))*(1-exp(-c*(Tmax-t)))}  #- for use with optim


  })


  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ14"
  print("equ14")
  return(output)
}

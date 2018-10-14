#' Equation 4
#'
#' @description Equation 4 is model H in  Li & Dickie (1987) citing Hinshelwood (1947)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ4(temp=temp, rate=rate))

equ4 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({
    R<-0.001987
    fit = minpack.lm::nlsLM(rate ~ a*exp(-b/(R*(temp+273.15)))-c*exp(-d/(R*(temp+273.15))),
              start = list(a=1.43e10, b=12.3,c=2.13e16,d=21),
              control = minpack.lm::nls.lm.control(maxiter = 10^20))


      output <- broom::tidy(fit)
      a <- output$estimate[output$term=="a"]
      b <- output$estimate[output$term=="b"]
      c <- output$estimate[output$term=="c"]
      d <- output$estimate[output$term=="d"]
      f_equ= function(t){a*exp(-b/(R*t))-c*exp(-d/(R*t))}  #- for use with optim

  })


  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ04"
  print("equ04")
  return(output)
}

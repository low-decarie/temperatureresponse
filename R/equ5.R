#' Equation 5
#'
#' @description Equation 5 is model J from Li & Dickie (1987) citing Johnson et al. (1942)  Does not currently work
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
#' @examples output <- with(Emiliania_huxleyi, equ5(temp=temp, rate=rate))
equ5 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({
    R<-0.001987
    b <- temp[rate==max(rate)]
    c <- b/100
    d <- 3*b
    a <- max(rate)/max((temp+273.15)*exp(-b/(R*(temp+273.15)))/(1+exp(c/R)*exp(-d/(R*(temp+273.15)))))

    fit = minpack.lm::nlsLM(rate ~ a*(temp+273.15)*exp(-b/(R*(temp+273.15)))/(1+exp(c/R)*exp(-d/(R*(temp+273.15)))),
              control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=a,
                           b=b,
                           c=c,
                           d=d))


    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    c <- output$estimate[output$term=="c"]
    d <- output$estimate[output$term=="d"]
    f_equ= function(t){a*(t+273.15)*exp(-b/(R*(t+273.15)))/(1+exp(c/R)*exp(-d/(R*(t+273.15))))}  #- for use with optim


  })



  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ05"
  print("equ05")
  return(output)
}

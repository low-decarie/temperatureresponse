#' Equation 6
#'
#' @description Equation 6
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ6(temp=temp, rate=rate))
#' 
equ6 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({
    R<-8.617343e-5
    b <- 1
    c <- 1.04
    d <- 273.15
    e <- 1.04
    f <- 273.15
    a <- max(rate)/max((((temp+273.15)/298.15)*exp(b/R*(1/298.15-1/(temp+273.15))))/(1+exp(c/R*(1/d-1/(temp+273.15)))+exp(e/R*(1/f-1/(temp+273.15)))))

    fit = minpack.lm::nlsLM(rate ~ (a*((temp+273.15)/298.15)*exp(b/R*(1/298.15-1/(temp+273.15))))/(1+exp(c/R*(1/d-1/(temp+273.15)))+exp(e/R*(1/f-1/(temp+273.15)))),
                            control = minpack.lm::nls.lm.control(maxiter = 10^20),
                            start = list(a=a,b=b,c=c,d=d,e=e,f=f))



      output <- broom::tidy(fit)
      a <- output$estimate[output$term=="a"]
      b <- output$estimate[output$term=="b"]
      c <- output$estimate[output$term=="c"]
      d <- output$estimate[output$term=="d"]
      f_equ= function(t){(a*((t+273.15)/298.15)*exp(b/R*(1/298.15-1/(t+273.15))))/(1+exp(c/R*(1/d-1/(t+273.15)))+exp(e/R*(1/f-1/(t+273.15))))}  #- for use with optim



  })

  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ06"
  print("equ06")
  return(output)
}

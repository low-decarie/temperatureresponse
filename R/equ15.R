#' Equation 15
#'
#' @description New equation (based on sine)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ15(temp=temp, rate=rate))
#'
equ15 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({
    fit = minpack.lm::nlsLM(rate ~ rmax*(sin(pi*((temp-tmin)/(tmax-tmin))^a))^b,
              control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=1, b=1,rmax=max(rate),tmin=min(temp), tmax=max(temp)))



      output <- broom::tidy(fit)
      a <- output$estimate[output$term=="a"]
      b <- output$estimate[output$term=="b"]
      tmin <- output$estimate[output$term=="tmin"]
      tmax <- output$estimate[output$term=="tmax"]
      rmax <- output$estimate[output$term=="rmax"]

      f_equ= function(t){rmax*(sin(pi*((t-tmin)/(tmax-tmin))^a))^b}  #- for use with optim


  })



  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ15"
  print("equ15")
  return(output)
}

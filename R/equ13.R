#' Equation 13
#'
#' @description Equation in Ratkowsky et al. (1983)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return depends on augment: if false, fitting parameters or fitted data
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ14(temp=temp, rate=rate))

equ13 <- function(temp,
                  rate,
                  augment = F,
                  return_fit=F){
  #Fit the function
  try_test <- try({
    fit = minpack.lm::nlsLM(rate ~ (a*(temp-tmin))^2 * (1-exp(b*(temp-tmax)))^2,start = list(a=1, b=0.1,tmin=min(temp),tmax=max(temp)))



    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    tmin <- output$estimate[output$term=="tmin"]
    tmax <- output$estimate[output$term=="tmax"]
    f_equ= function(t){(a*(t-tmin))^2 * (1-exp(b*(t-tmax)))^2}  #- for use with optim

  })

  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ13"
  print("equ13")
  return(output)
}

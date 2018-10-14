#' Equation 9
#'
#' @description Equation from Montagnes et al. 2008
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ9(temp=temp, rate=rate))

equ9 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({
    fit = minpack.lm::nlsLM(rate ~ a*exp(-0.5*(abs((temp-tref)/b))^c),
              control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=max(rate), b=5,c=2,tref=temp[rate==max(rate)]))

    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    c <- output$estimate[output$term=="c"]
    tref <- output$estimate[output$term=="tref"]
    f_equ= function(t){a*exp(-0.5*(abs((t-tref)/b))^c)}  #- for use with optim

  })

  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)

  output$model <- "equ09"
print("equ09")
  return(output)
}

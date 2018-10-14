#' Equation 10
#'
#' @description Equation from Thomas et al. (2014)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#' @examples #output <- with(Emiliania_huxleyi, equ10(temp=temp, rate=rate))
#'
equ10 <- function(temp,rate, augment=F, return_fit=F){
  #Fit the function
  try_test <- try({

    b=(max(temp)-min(temp))/2
    c=0
    tref=temp[rate==max(rate)]
    a=max(rate)/max(exp(c*temp)*(1-((temp-tref)/b)^2))

    fit = minpack.lm::nlsLM(rate ~ a*exp(c*temp)*(1-((temp-tref)/b)^2),
              control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=a,b=b,c=c,tref=tref))

    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    c <- output$estimate[output$term=="c"]
    tref <- output$estimate[output$term=="tref"]

    f_equ= function(t){a*exp(c*t)*(1-((t-tref)/b)^2)}  #- for use with optim

  })

  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ10"
  print("equ10")
  return(output)
}

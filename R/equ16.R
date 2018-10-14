#' Equation 16
#'
#' @description Equation from "A Key Marine Diazotroph in a Changing Ocean: The Interacting Effects of Temperature, CO2 and Light on the Growth of Trichodesmium erythraeum IMS101".  Challenging to fit to many datasets.  Does not fit to example dataset.
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ16(temp=temp, rate=rate))


equ16 <- function(temp,rate, augment = F, return_fit=F){
  #Fit the function
  try_test <- try({
    b=1
    c=0.05
    tnaught=temp[rate==max(rate)]
    d=2
    e=0
    a=max(rate)/max(((b^temp)*exp(-c*((temp-tnaught)^d))-e),na.rm = T)
    fit = minpack.lm::nlsLM(rate ~ a*((b^temp)*exp(-c*((temp-tnaught)^d))-e), start = list(a=a,
                           b=b,
                           c=c,
                           tnaught=tnaught,
                           d=d,
                           e=e))
                           # lower=c(0,0,0.009,0,1,0),
                           # upper=c(10,2,0.1,100,3,10))


    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    c <- output$estimate[output$term=="c"]
    tnaught <- output$estimate[output$term=="tnaught"]
    d <- output$estimate[output$term=="d"]
    e <- output$estimate[output$term=="e"]
  #- for use with optim


  })

  f_equ= function(t){a*((b^t)*exp(-c*((t-tnaught)^d)-e))}

  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)


  output$model <- "equ16"
  print("equ16")
  return(output)
}

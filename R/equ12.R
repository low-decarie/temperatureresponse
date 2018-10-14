#' Equation 12
#'
#' @description Equation in Montagnes et al (2008) citing Flinn (1991)
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#'
#' @return depends on augment: if false, fitting parameters or fitted data
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ12(temp=temp, rate=rate))

 equ12 <- function(temp,rate,augment=F, return_fit=F){
  #Fit the function

  try_test <- try({

    c=0.005
    b=(-2*c*temp[rate==max(rate)])[1]
    a= -min(b*temp +c*(temp^2))

    fit = minpack.lm::nlsLM(rate ~ 1/(1+a + b*temp +c*(temp^2)),
              control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=a, b=b,c=1))


    output <- broom::tidy(fit)
    a <- output$estimate[output$term=="a"]
    b <- output$estimate[output$term=="b"]
    c <- output$estimate[output$term=="c"]
    f_equ= function(t){1/(1+ a + b*t + c *t^2)}  #for use with optimise

      })


  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)



  output$model <- "equ12"
  print("equ12")
  return(output)

}

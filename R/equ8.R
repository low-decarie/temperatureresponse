#' Equation 8
#'
#' @description Equation in Li & Dickie (1987) citing Stoermer & Ladewski (1976): a*exp(-0.5*((temp-tref)/b)^2)
#'
#' @param temp temperature (in celsius or Kelvin)
#' @param rate rate measurement
#' @param augment logical wether the dataset with fits should be returned instead of the parameter values
#' @param return_fit logical wether the model fit object should be returned
#' @param plot_profile logical should the model fitting profile be plotted
#'
#' @return depends on augment: if false, fitting parameters or fitted data
#' @export
#'
#' @examples output <- with(Emiliania_huxleyi, equ8(temp=temp, rate=rate))
equ8 <- function(temp,
                 rate,
                 augment=F,
                 plot_profile=F,
                 return_fit=F){

  #Fit the function
  try_test <- try({
  fit = minpack.lm::nlsLM(rate ~ a*exp(-0.5*((temp-tref)/b)^2),
            control = minpack.lm::nls.lm.control(maxiter = 10^20),start = list(a=max(rate),
                         b=(max(temp)-min(temp))/2,
                         tref=temp[rate==max(rate)]))

  if(plot_profile==T){
    plot(profile(fit))
    }

  output <- broom::tidy(fit)
  a <- output$estimate[output$term=="a"]
  b <- output$estimate[output$term=="b"]
  tref <- output$estimate[output$term=="tref"]

    #For this model topt = tref, no need to find maxima

  f_equ= function(t){a*exp(-0.5*((t-tref)/b)^2)}

})

  output <- temperatureresponse::amend_output(output,
                                              fit,
                                              f_equ,
                                              temp,
                                              rate,
                                              try_test,
                                              augment,
                                              return_fit)

  output$model <- "equ08"
  print("equ08")
  return(output)

}

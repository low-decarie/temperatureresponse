#' amend_output
#'
#' @description Helper function that add terms to the broom output of fit
#'
#' @param f_equ equation with fitted parameters
#' @param output broom output of fit
#' @param temp temperature values of measurements
#' @param fit the model output of the fitting process
#' @param rate rate that changes with temperature
#' @param try_test did the model fitting succeed or produce an error?
#' @param augment add columns to the original dataset such as predictions, residuals and cluster assignments using package::broom (T/F)?
#' @param return_fit return the model object (T/F)?
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#' @import graphics
#' @import stats
#'
#' @export


#Terms to add: Tbr (temperature range at half growth), Tsm (difference between Topt and Tmax), inflection point

amend_output <- function(output,
                         fit,
                         f_equ,
                         temp,
                         rate,
                         try_test,
                         augment,
                         return_fit){

  try_test_2 <- try({


maxim= optimise(f_equ, interval = c(0,max(temp)), maximum = TRUE)
roots<-rootSolve::multiroot(f_equ,c(tmin=min(temp), tmax=max(temp)))
AIC<-AIC(fit)
BIC<-BIC(fit)
AICc <- AICcmodavg::AICc(fit)
pseudoR2 <- summary(lm(predict(fit)~rate))$adj.r.squared



topt <- maxim$maximum
rmax <- maxim$objective
CTmin <- roots$root[1]
CTmax <- roots$root[2]
tolerance_range <- roots$root[2]-roots$root[1]
thermal_safety_margin <- CTmax-topt

f_equ_50 <- function(t){f_equ(t)-0.5*rmax}

roots_50<-rootSolve::multiroot(f_equ_50,c(tmin=min(temp), tmax=max(temp)))

T50min <- roots_50$root[1]
T50max <- roots_50$root[2]
Tbr <- T50max-T50min


pred_fit <- function(x)predict(fit, newdata=data.frame(temp=x))
derivative <- numDeriv::grad(pred_fit, temp)

tryCatch({activation <- mean(derivative[derivative>0])},
         error=function(e){activation <- NA})
tryCatch({deactivation <- mean(derivative[derivative<0])},
         error=function(e){deactivation <- NA})
tryCatch({skewness <- abs(activation)-abs(deactivation)},
         error=function(e){skewness <- NA})




additional_terms <- data.frame(t(data.frame(topt,
                                 rmax,
                                 CTmin,
                                 CTmax,
                                 tolerance_range,
                                 thermal_safety_margin,
                                 AIC,
                                 BIC,
                                 AICc,
                                 pseudoR2,
                                 activation,
                                 deactivation,
                                 skewness,
                                 T50min,
                                 T50max,
                                 Tbr)))

names(additional_terms) <- "estimate"
additional_terms$term <- row.names(additional_terms)

output <- broom::tidy(fit)

output <- dplyr::bind_rows(output, additional_terms)
})

if(class(try_test)=="try-error"|class(try_test_2)=="try-error"){
  output <-data.frame(term=NA,
                      estimate=NA,
                      std.error=NA,
                      statistic=NA,
                      p.value=NA)
}

  if(augment==T){
    try_test2 <- try({
      output <- broom::augment(fit)
    pred_fit <- function(x)predict(fit, newdata=data.frame(temp=x))
    output$derivative <- numDeriv::grad(pred_fit, temp)
    # output <- merge(output,
    #                 predictNLS(fit, newdata = data.frame(temp=temp)))


    })
    if(class(try_test)=="try-error"|class(try_test2)=="try-error"){
      output <-data.frame(rate=NA,
                          temp=NA,
                          .fitted=NA,
                          # derivative=NA,
                          .resid=NA)
    }
  }

  if(return_fit){
    output <- fit
  }

return(output)
}

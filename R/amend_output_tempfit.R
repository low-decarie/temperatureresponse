#' amend_output_tempfit
#'
#' @description Helper function that add terms to the broom output of fit
#' @param temp temperature values of measurements
#' @param fit the model output of the fitting process
#' @param rate rate that changes with temperature
#' @param try_test did the model fitting succeed or produce an error?
#' @param output_type type of output to return
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#'
#'


#Terms to add: Tbr (temperature range at half growth), Tsm (difference between Topt and Tmax), inflection point

amend_output_tempfit <- function(fit,
                         temp,
                         rate,
                         try_test=NULL,
                         output_type){


  try_test_2 <- try({

    f_equ <- function(new_temp){predict(fit,newdata=list(temp=new_temp))}

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


predicted <- predict(fit)
rising <- data.frame(temp=temp[temp<topt],
                     predicted=predicted[temp<topt])
falling <- data.frame(temp=temp[temp>topt],
                      predicted=predicted[temp>topt])
if(nrow(rising)>2){
activation <- coef(lm(predicted~temp, data=rising))[2]}else{activation <- NA}
if(nrow(falling)>2){
deactivation <- coef(lm(predicted~temp, data=falling))[2]}else{deactivation <- NA}
skewness <- -deactivation/activation




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

  if(output_type=="augment"){
    try_test2 <- try({output <- broom::augment(fit)})
    if(class(try_test)=="try-error"|class(try_test2)=="try-error"){
      output <-data.frame(rate=NA,
                          temp=NA,
                          .fitted=NA,
                          .resid=NA)
    }
  }

  if(output_type=="model_object"){
    output <- fit
  }

return(output)
}

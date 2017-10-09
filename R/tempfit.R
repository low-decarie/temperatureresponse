#' tempfit
#'
#' @description Fits equations to temperature response data
#'
#' @param temp temperature (in Celsius)
#' @param rate rate measurement
#' @param output_type one of "parameters", "augment","model_object"
#' @param equation numeric value of equation
#' @param ... supplemental parameters for  minpack.lm::nlsLM (eg. control=minpack.lm::nls.lm.control(maxiter = 10^5)
#'
#' @return a data frame of, depending on augment argument, if FALSE, parameters, if TRUE, data with predicted values
#'
#'

tempfit <- function(temp,rate, equation=10,output_type="parameters",...){

  print(equation)


  # Open try ####
  try_test <- try({


#Equation 4 ####
#is model H in  Li & Dickie (1987) citing Hinshelwood (1947)
if(equation==4){
  R<-0.001987
  fit = minpack.lm::nlsLM(rate ~ a*exp(-b/(R*(temp+273.15)))-c*exp(-d/(R*(temp+273.15))),
                          start = list(a=1.43e10, b=12.3,c=2.13e16,d=21),...)
}

    #Equation 5 ####
    #is model J from Li & Dickie (1987) citing Johnson et al. (1942)
    if(equation==5){
      b=(max(temp)-min(temp))/2
      c=0
      tref=temp[rate==max(rate)]
      a=max(rate)/max(exp(c*temp)*(1-((temp-tref)/b)^2))

      fit = minpack.lm::nlsLM(rate ~ a*exp(c*temp)*(1-((temp-tref)/b)^2),start = list(a=a,b=b,c=c,tref=tref),...)
    }

    #Equation 6 ####
    #
    if(equation==6){

      R<-8.617343e-5
      b <- 1
      c <- 1.04
      d <- 273.15
      e <- 1.04
      f <- 273.15
      a <- max(rate)/max((((temp+273.15)/298.15)*exp(b/R*(1/298.15-1/(temp+273.15))))/(1+exp(c/R*(1/d-1/(temp+273.15)))+exp(e/R*(1/f-1/(temp+273.15)))))

      fit = minpack.lm::nlsLM(rate ~ (a*((temp+273.15)/298.15)*exp(b/R*(1/298.15-1/(temp+273.15))))/(1+exp(c/R*(1/d-1/(temp+273.15)))+exp(e/R*(1/f-1/(temp+273.15)))),start = list(a=a,b=b,c=c,d=d,e=e,f=f),...)
    }




  #Equation 7 ####
  #from Montagnes et al (2008) citing Schoolfield et al. (1981)
  if(equation==7){
    R<-8.617343e-5
    b <- 1
    c <- 1.05
    d <- 273.15
    a <- max(rate)/max((((temp+273.15)/293.15)*exp(b/R*(1/293.15-1/(temp+273.15))))/(1+exp(c/R*(1/d-1/(temp+273.15)))))

    fit = minpack.lm::nlsLM(rate ~ (a*((temp+273.15)/293.15)*exp(b/R*(1/293.15-1/(temp+273.15))))/(1+exp(c/R*(1/d-1/(temp+273.15)))),
                            start = list(a=a,                                                                               b=b,                                                                             c=c,                                                                              d=d),...)
  }

  #Equation 8 ####
  #Equation in Li & Dickie (1987) citing Stoermer & Ladewski (1976)
  if(equation==8){
    fit = minpack.lm::nlsLM(rate ~ a*exp(-0.5*((temp-tref)/b)^2),
                            start = list(a=max(rate),                                       b=(max(temp)-min(temp))/2,                                                       tref=temp[rate==max(rate)]),...)
  }


  #Equation 9 ####
  #Equation from Montagnes et al. 2008
  if(equation==9){
    fit = minpack.lm::nlsLM(rate ~ a*exp(-0.5*(abs((temp-tref)/b))^c),
                            start = list(a=max(rate), b=5,c=2,tref=temp[rate==max(rate)]),
                            ...)
  }

  #Equation 10 ####
  #Equation from Thomas et al. (2014)
  if(equation==10){b=(max(temp)-min(temp))/2
  c=0
  tref=temp[rate==max(rate)]
  a=max(rate)/max(exp(c*temp)*(1-((temp-tref)/b)^2))

  fit = minpack.lm::nlsLM(rate ~ a*exp(c*temp)*(1-((temp-tref)/b)^2),
                          start = list(a=a,b=b,c=c,tref=tref),
                          ...)
  }

  #Equation 11 ####
  #Equation in Montagnes et al. 2008
  if(equation==11){b=(max(temp)-min(temp))/2
  c=-0.005
  b=(-2*c*temp[rate==max(rate)])[1]
  a=max(rate)-max(b*temp +c*(temp^2))


  fit = minpack.lm::nlsLM(rate ~ a + b*temp +c*(temp^2),
                          start = list(a=a, b=b,c=-2),...)
  }


  #Equation 12 ####
  #Equation in Montagnes et al. 2008
  if(equation==12){b=(max(temp)-min(temp))/2
  c=0.005
  b=(-2*c*temp[rate==max(rate)])[1]
  a= -min(b*temp +c*(temp^2))

  fit = minpack.lm::nlsLM(rate ~ 1/(1+a + b*temp +c*(temp^2)),
                          start = list(a=a, b=b,c=1),
                          ...)
  }

  #Equation 13 ####
  #Equation in Ratkowsky et al. (1983)
  if(equation==13){
    b=(max(temp)-min(temp))/2
  fit = minpack.lm::nlsLM(rate ~ (a*(temp-tmin))^2 * (1-exp(b*(temp-tmax)))^2,start = list(a=1, b=0.1,tmin=min(temp),tmax=max(temp)),
                          ...)
  }

  #Equation 14 ####
  #Equation from Kamykowski (1985)
  if(equation==13){
    fit = minpack.lm::nlsLM(rate ~ a*(1-exp(-b*(temp-Tmin)))*(1-exp(-c*(Tmax-temp))),
                            start = list(a=max(rate),
                                         b=1,
                                         c=1,
                                         Tmax=max(temp),
                                         Tmin=min(temp)),
                            ...)
  }

  #Equation 15 ####
  #New equation (based on sine)
  if(equation==15){
    fit = minpack.lm::nlsLM(rate ~ rmax*(sin(pi*((temp-tmin)/(tmax-tmin))^a))^b,
                            start = list(a=1, b=1,rmax=max(rate),tmin=min(temp), tmax=max(temp)),
                            ...)
  }


  #Equation 16 ####
  #New equation (based on sine)
  if(equation==16){
    b=1
    c=0.05
    tnaught=temp[rate==max(rate)]
    d=2
    e=0
    a=max(rate)/max(((b^temp)*exp(-c*((temp-tnaught)^d))-e),na.rm = T)
    fit = minpack.lm::nlsLM(rate ~ a*((b^temp)*exp(-c*((temp-tnaught)^d))-e), start = list(a=a,b=b,c=c,tnaught=tnaught,d=d,e=e),...)
  }

# Close try ####
  })

  #amend_output_tempfit####

  output <- temperatureresponse::amend_output_tempfit(fit,
                                                      temp,
                                                      rate,
                                                      try_test,
                                                      output_type)

  output$model <- equation

  return(output)


}


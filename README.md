# temperatureresponse

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/temperatureresponse)](https://cran.r-project.org/package=temperatureresponse)

R package for fitting a suite equations for the unimodal response to temperature

Install from CRAN:

```R
install.packages("temperatureresponse")
```

Install from github:
```R
require(devtools)
install_github("low-decarie/temperatureresponse")
```

Companion to:
Low-Décarie E, Boatman TG, Bennett N, Passfield W, Gavalás-Olea A, Siegel P, Geider RJ (2017) Predictions of response to temperature are contingent on model choice and data quality. Ecology and Evolution, 1–15.
http://dx.doi.org/10.1002/ece3.3576

![Model fits](https://wol-prod-cdn.literatumonline.com/cms/attachment/62030c3e-4ac8-49da-9514-19aad2fb8c98/ece33576-fig-0003-m.jpg)

Caption from publication
> Figure 3 (a) Equation fit to an example dataset of phytoplankton growth rate as a function of temperature (Phaeodactylum tricornutum). The points are the measured growth rate (same values across panels), and the lines are the equation predicted growth rates. (b) Equation residuals as function of temperature. (c) Value of the first derivative (gradient) at each measured temperature. Numbers within the figure indicate the equation number. Equations are grouped as a function of their number of parameters (3–6). Equations with four parameters are further divided between empirical and mechanistic equations to minimize clutter within the plots. Lines for individual equations are labeled with color and the equation number. Similar patterns can be observed for other species

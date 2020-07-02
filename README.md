Catmaply - Demo app with shiny <img src="figures/logo.png" align="right" height="192 px"/>
======================

This app is used to test the integration of the package [catmaply](https://github.com/yvesmauron/catmaply) with shiny. Additionally, it is used the test interactive ploting possibilities by capturing plotly events with the `event_data()` function.

## Disclaimer

This simplistic shiny app is meant for quickly testing the funcitonality of the [catmaply](https://github.com/yvesmauron/catmaply) package and not for production scenarios.


## Installation

To run the shiny app, the following packages have to be installed:

```R
# make sure that you have the corrent R Tools installed.
# as you might need to build some packages from source

# if do not have RTools installed, you can install it with:
# install.packages('installr'); install.Rtools() # not tested on windows
# or download it from here:
# https://cran.r-project.org/bin/windows/Rtools/
# in any case, make sure that you select the correct version, 
# otherwise the installation will fail.

# Then you'll need devtools
if (!require('devtools'))
  install.packages('devtools')

# Finally install the package
devtools::install_github('yvesmauron/catmaply')


pkgs <- c(
   'shiny'
  ,'shinyjs'
  ,'catmaply'
  ,'plotly'
  ,'DT'
  ,'shinydashboard'
)

install.packages(pkgs)
```



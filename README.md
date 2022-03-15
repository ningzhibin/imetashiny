# imetashiny

* imetashiny apps to run locally
* This package is not intended to be published as installable package(though still can)
* The current imetashiny apps need to be adapted/optimized to run
* The main purpose is to run local shiny apps with dynamic data imput (pre-coppied to the defined folder) instead of "uploading manually". 
* The first stage goal is to organize all meta shiny apps to be able to run locally for training purpose
* The second stage goal is to modify these apps to take loacal input automatically, for interactive data analysis, directly after metalab search. 
* The ultimate goal is to make metalab as an interactive analysis hub from metalab result and for multi-omics data analysis. 



# Enviroment setup
It is going to share the same environment as the local static metalab report.

* R goes green, indeed. It does not need any registration on Windows to run. The whole R installation folder can be moved around as long as you use the right path. 
* The base/essential packages are in R.home()/library/. All the other package installation paths can be checked by .libPaths()
* Copy an R installation directory to a new folder. This folder's path can be checked by R.home()
* Copy all installed library (if you have installed any) under the installation folder, e.g. to a sub folder named site-library
* This file /etc/Rprofile.site  is the one to configure each r version/enviroment. It is an R script actually (without .r as extention though) and is sourced every time you start the R. Add this line to the file.    

```
.libPaths(c(paste0(R.home(), "/site-library"),paste0(R.home(), "/library")))
```

It will set the site-library as the default (first) for new packges to install. 

* In order to be able to use this "Green" R environment for R markdown, apart from installation of all rmarkdown related packages, rmarkdown needs to know where to find pandoc.exe. The easiest way is to set it like this in the script before the knit command: 
```
rmarkdown::find_pandoc(dir =  .libPaths())
```
For the above setting, pandoc.exe needs to be in site-library


# How to run apps in CMD in the target R enviroment


## The port number, 

The port number can be set like this:
```
options(shiny.port = 5010)
```

Either the port number can be set globally for all runs, by add the above line in the /etc/Rprofile.site, or can be set to a different number earch time if you want to run multiple instances.

The following examples assume that the port has been set to 5010 globally

## Batch script


* put these in a r script and run the script, then run using R CMD BATCH, then open this  127.0.0.1:5010 in your brower
* note that this can be implemented into java easily by the internet explore widget and run by cmd

In test_run_app.r, there is only one line:
```
shiny::runApp('d://shiny_apps/01_hello', launch.browser = TRUE)
```

then run this in CMD:

```
D:\R-4.0.4\bin\R.exe R CMD BATCH --no-save --no-restore D:\report_demo\Demo_shiny_CMD\test_run_app.r
```


## Direct run in CMD
```
D:\R-4.0.4\bin\R.exe -e "shiny::runApp('D:shiny_apps//01_hello')"
```
you will see a bit difference between the above two ways. 


# Save shiny apps as functions

see more from [here](https://shiny.rstudio.com/articles/function.html)

The point of run shiny apps as function is that you can pass parameters into shiny app
In a distribution point of view, functions need to be compiled into packages, but apps can be copied and pasted to run direcctly
There is a way of passing parameter to shiny apps by feeding the file with a fix name in a specified location 


* Define a Shiny app in a single script with shinyApp
* Save your app as a parameterized function
* Launch your app from the command line or inside an interactive document

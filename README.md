# imetashiny

* imetashiny apps to run locally
* This package is not intended to be published as installable package(thoguh still can)
* The main purpose is to run local shiny apps with dynamic data imput (pre-coppied to the defined folder) instead of "uploading manually". 
* The first stage goal is to organize all meta shiny apps to be able to run locally for training purpose
* The second stage goal is to modify these apps to take loacal input automatically, for interactive data analysis, directly after metalab search. 
* The ultimate goal is to make metalab as an interactive analysis hub from metalab result and for multi-omics data analysis. 



# running enviroment setup
It is going to share the same environment as the local static metalab report.

* R is green actually, it does not need any registration on window to run. The whole R installation folder can be moved as long as you use the right path. 
* the base/essential packages are in R.home()/libary. All the other package installation path can be checked by .libPaths()
* copy an R installation directory to a new folder. This folder's path can be checked by R.home()
* copy all installed library (if you have installed any) to the installation folder, e.g. this sub folder: site-library
* this file /etc/Rprofile.site  is the one to configure each r version. It is an R script and is sourced every time you start the R. Add this line:   .libPaths(c(paste0(R.home(), "/site-library"),paste0(R.home(), "/library")))

it will set the site-library as the default (first) to install

* in order to be able to use this "Green" R environment for R markdown, apart from installation of all rmarkdown related packages, rmarkdown needs to know where to find pandoc.exe. The easiest way is to set it like this in the script before knitting: 
rmarkdown::find_pandoc(dir =  .libPaths())

For the above setting, pandoc.exe needs to be in site-library



# how to run apps in commandline in the target r enviroment

* put these in a r script and run the script using R CMD BATCH, this will fix the port, then open this  127.0.0.1:5010 in your brower
* note that this can be implanted into java easily by the internet explore widget and run by cmd

options(shiny.port = 5010)
shiny::runApp('C://Users//Figeys Lab//Dropbox//Github//imetashiny//examples//01_hello', launch.browser = TRUE)




# or directly from command line, but this will not have a fix port 

D:\R-4.0.4\bin\R.exe -e "shiny::runApp('C://Users//Figeys Lab//Dropbox//Github//imetashiny//examples//01_hello')"


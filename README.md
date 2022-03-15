# imetashiny

* imetashiny apps to run locally
* This package is not intended to be published as installable package(thoguh still can)
* The main purpose is to run local shiny apps with dynamic data imput (pre-coppied to the defined folder) instead of "uploading manually". 
* The first stage goal is to organize all meta shiny apps to be able to run locally for training purpose
* The second stage goal is to modify these apps to take loacal input automatically, for interactive data analysis, directly after metalab search. 
* The ultimate goal is to make metalab as an interactive analysis hub from metalab result and for multi-omics data analysis. 



# running enviroment
It is going to share the same enviroment as the static metalab report.


# how to run apps in commandline

# put these in a r script and run the script, this will fix the port, then open this  127.0.0.1:5010 in your brower
# not that this can be implanted into java easilly

options(shiny.port = 5010)
shiny::runApp('C://Users//Figeys Lab//Dropbox//Github//imetashiny//examples//01_hello', launch.browser = FALSE)




# or directly from command line, but this will not have a fix port 

D:\R-4.0.4\bin\R.exe -e "shiny::runApp('C://Users//Figeys Lab//Dropbox//Github//imetashiny//examples//01_hello')"


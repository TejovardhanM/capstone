workingdirectory <- "/Users/tejo/UW450/Capstone"
setwd(workingdirectory)
cat("\014")
getwd()
if(!require("ggplot2")){install.packages("ggplot2");require("ggplot2")} 
if(!require("dplyr")){install.packages("dplyr");require("dplyr")} 
if(!require("tidyverse")){install.packages("tidyverse");require("tidyverse")} 
#if(!require("corrr")){install.packages("corrr");require("corrr")} 
if(!require("PerformanceAnalytics")){install.packages("PerformanceAnalytics");require("PerformanceAnalytics")} 

# script pour hortense pour le telechargement de ses photos

vecPackage=c("ade4","akima","data.table","date","devtools","dismo","doBy","dplyr","factoextra","gam","ggmap","gdata","ggplot2","gstat","lme4","lubridate","mapdata","mapproj","maps","maptools","MASS","mgcv","mgcViz","MuMIn","nlme","partykit","party","plyr","rattle","reshape","reshape2","rgdal","RMySQL","RODBC","rpart","rpart.plot","RPostgreSQL","siland","snow","sp","StreamMetabolism","stringr","survMisc","vegan","xlsx")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)



importXLS <- function() {
library(xlsx)



    d <- read.csv2(file="data/winterwings(3).csv")


}

# script pour hortense pour le telechargement de ses photos

vecPackage=c("ade4","akima","data.table","date","devtools","dismo","doBy","dplyr","factoextra","gam","ggmap","gdata","ggplot2","gstat","lme4","lubridate","mapdata","mapproj","maps","maptools","MASS","mgcv","mgcViz","MuMIn","nlme","partykit","party","plyr","rattle","reshape","reshape2","rgdal","RMySQL","RODBC","rpart","rpart.plot","RPostgreSQL","siland","snow","sp","StreamMetabolism","stringi","stringr","survMisc","vegan","xlsx")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)

require(reshape2)

read.data <- function() {

    library(reshape2)
library(stringi)
    tsp <- read.csv2("data/species.csv",stringsAsFactors=FALSE)
#  Sys.setlocale(category = "LC_ALL", locale="Korean")
    vecFile <- list.files("data_csv/")
    taball <- NULL
    for(f in vecFile) {
      ##  f <- vecFile[36]
     ##   f_encod<- rawToChar(readBin(file_name, "raw", 100000))
     ##   stri_enc_detect(f)
        file_name <- paste("data_csv/",f,sep="")
        df <- read.csv(file_name,stringsAsFactors=FALSE,encoding="EUC-KR")
     ##   head(df)
        df1 <- df[1:15,1:2]

        dfeed <- df[17:23,1:2]
        feed <- as.character(dfeed[which(!(dfeed[,2]%in% c(""," "))),1])
        if(length(feed)!=1) feed <- paste(feed,collapse="|")

        denv <- df[26:31,1:2]
        env <- as.character(denv[which(!(denv[,2]%in% c(""," "))),1])
        if(length(env)!=1) env <- paste(env,collapse="|")

        dtemp <- df[2:9,4:5]
        temp <- as.character(dtemp[which(!(dtemp[,2]%in% c(""," "))),1])
        if(length(temp)!=1) temp <- paste(temp,collapse="|")

        dprecType <- df[12:16,4:5]
        precType <- as.character(dprecType[which(!(dprecType[,2]%in% c(""," "))),1])
        if(length(precType)!=1) precType <- paste(precType,collapse="|")

        dprecWhen <- df[17:21,4:5]
        precWhen <- as.character(dprecWhen[which(!(dprecWhen[,2]%in% c(""," "))),1])
        if(length(precWhen)!=1) precWhen <- paste(precWhen,collapse="|")

        dprecHow <- df[23:24,4:5]
        precHow <- as.character(dprecHow[which(!(dprecHow[,2]%in% c(""," "))),1])
        if(length(precHow)!=1) precHow <- paste(precHow,collapse="|")

        dprecSnow <- df[27:32,4:5]
        precSnow <- as.character(dprecSnow[which(!(dprecSnow[,2]%in% c(""," "))),1])
        if(length(precSnow)!=1) precSnow <- paste(precSnow,collapse="|")


        sessionf <- data.frame(observator=df1[2,2],nomFile=f,date=df1[4,2],site=df1[6,2],
                               latitude_wgs = df1[10,2],longitude_wgs = df1[9,2],
                               time = df1[12,2],area = df1[15,2],
                               feeder=feed,environment=env,
                               temperature=temp,precipitation_type=precType,
                               precipitation_when=precWhen,precipitation_how=precHow,
                               snow=precSnow)


        df2 <- df[38:nrow(df),1:5]
        colnames(df2) <- c("sp","s1","s2","s3","s4")
        df2 <- subset(df2,sp!="")

        df2$sc_name <- substr(df2$sp,regexpr(",",df2$sp)+2,nchar(df2$sp))
        df2$eng_name <- substr(df2$sp,1,regexpr(",",df2$sp)-1)

        df2 <- merge(df2,tsp,by="sc_name",all.x=TRUE)

        df2$english_name <- ifelse(is.na(df2$english_name),df2$eng_name,df2$english_name)

        df2 <- df2[,c("sc_name","english_name","s1","s2","s3","s4")]

        obsf <- melt(df2,id.vars=c("sc_name","english_name"))
        colnames(obsf)[3:4] <- c("session","abundance")
        obsf$abundance <- as.numeric(obsf$abundance)
        obsf <- na.omit(obsf)
        obsf$session <- as.numeric(substr(obsf$session,2,2))


        tabf <- data.frame(observator=df1[2,2],nomFile=f,date=df1[4,2],site=df1[6,2],
                           latitude_wgs = df1[10,2],longitude_wgs = df1[9,2],
                           time = df1[12,2],area = df1[15,2],
                           feeder=feed,environment=env,
                           temperature=temp,precipitation_type=precType,
                           precipitation_when=precWhen,precipitation_how=precHow,
                           snow=precSnow,session = obsf$session,
                           sc_name = obsf$sc_name,english_name=obsf$english_name,
                           abundance = obsf$abundance)

        taball <- rbind(taball,tabf)

    }


    write.csv(taball,"data/database_winterwing.csv",encoding="UTF-8",row.names=FALSE)

}

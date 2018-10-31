# script pour hortense pour le telechargement de ses photos

vecPackage=c("ade4","akima","data.table","date","devtools","dismo","doBy","dplyr","factoextra","gam","ggmap","gdata","ggplot2","gstat","lme4","lubridate","mapdata","mapproj","maps","maptools","MASS","mgcv","mgcViz","MuMIn","nlme","partykit","party","plyr","rattle","reshape","reshape2","rgdal","RMySQL","RODBC","rpart","rpart.plot","RPostgreSQL","siland","snow","sp","StreamMetabolism","stringi","stringr","survMisc","vegan","xlsx")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)

require(reshape2)

read.data <- function(fileName="data/winterwings_data.csv",output=FALSE) {

    library(reshape2)
    library(stringi)
    tsp <- read.csv2("data/species.csv",stringsAsFactors=FALSE)
#  Sys.setlocale(category = "LC_ALL", locale="Korean")
    vecFile <- list.files("data_csv/")
    cat("\nImporting",length(vecFile),"files\n\n")
    taball <- NULL
    i <- 0
    for(f in vecFile) {
        i <- i + 1
     ##   f <- vecFile[1]
     ##   f_encod<- rawToChar(readBin(file_name, "raw", 100000))
        ##   stri_enc_detect(f)
        file_name <- paste("data_csv/",f,sep="")
        cat(i,"/",length(vecFile),": ",file_name,"\n",sep="")
        df <- read.csv(file_name,stringsAsFactors=FALSE,encoding="EUC-KR")
        if(ncol(df) == 1) df <- read.csv2(file_name,stringsAsFactors=FALSE,encoding="EUC-KR")
     ##   head(df)
        df1 <- df[1:15,1:2]

        dfeed <- df[17:23,1:2]
        vfeed_eng <- c("Type of feeder","","No feeder","","Feed on the ground","","High up feeder(s)")
        feed <- as.character(vfeed_eng[which(!(dfeed[,2]%in% c(""," ")))])
        if(length(feed)!=1) feed <- paste(feed,collapse="|")

        denv <- df[26:31,1:2]
        venv_eng <- c("The main environment around me","","Urban area","Agriculture area","Forest","Aquatic areas (wetlands, rivers, sea")
        env <- as.character(venv_eng[which(!(denv[,2]%in% c(""," ")))])
        if(length(env)!=1) env <- paste(env,collapse="|")

        dtemp <- df[2:9,4:5]
        vtemp_eng <- c("Environmental conditions","Temperatures","Under -18","-18 to -10","-9 to 0","1 to 10","11 to 20","Over 20")
        temp <- as.character(vtemp_eng[which(!(dtemp[,2]%in% c(""," ")))])
        if(length(temp)!=1) temp <- paste(temp,collapse="|")

        dprecType <- df[12:16,4:5]
        vprecType_eng <- c("Type","None","Rain","Rain/Snow","Snow")
        precType <- as.character(vprecType_eng[which(!(dprecType[,2]%in% c(""," ")))])
        if(length(precType)!=1) precType <- paste(precType,collapse="|")

        dprecWhen <- df[17:21,4:5]
        vprecWhen_eng <- c("When?","None","During 1st session","During 2nd session","During the next session(s)")
        precWhen <- as.character(vprecWhen_eng[which(!(dprecWhen[,2]%in% c(""," ")))])
        if(length(precWhen)!=1) precWhen <- paste(precWhen,collapse="|")

        dprecHow <- df[23:24,4:5]
        vprecHow_eng <- c("Partially","All the time")
        precHow <- as.character(vprecHow_eng[which(!(dprecHow[,2]%in% c(""," ")))])
        if(length(precHow)!=1) precHow <- paste(precHow,collapse="|")

        dprecSnow <- df[27:32,4:5]
        vprecSnow_eng <- c("None","under 5 cm ","5 cm to 15 cm","over 15 cm","Hard crust or ice covers snow","Snow cover is patchy (under 0.5 cover)")
        precSnow <- as.character(vprecSnow_eng[which(!(dprecSnow[,2]%in% c(""," ")))])
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


    write.csv(taball,fileName,fileEncoding="UTF-8",row.names=FALSE)
    cat("\n\n--> Data saved in :",fileName,"\n")

    if(output) return(taball)
}

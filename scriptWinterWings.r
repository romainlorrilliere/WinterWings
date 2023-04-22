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


analysisEffortEffect <- function(dataFile="data/winterwings_data.csv",output=FALSE) {

    dataFile="data/winterwings_data.csv";output=FALSE

    library(reshape2)
    library(ggplot2)
    library(lme4)

    d <- read.csv(dataFile,stringsAsFactor=FALSE)

    d$id <- as.numeric(as.factor(d$nomFile))
    d$obs <- as.numeric(as.factor(d$observator))
    d <- subset(d,select=c("id","obs","date","time","feeder","environment","temperature","session","sc_name","abundance"))

    dw <- dcast(id+obs+date+time+feeder+environment+temperature+sc_name~session,data=d, value.var="abundance")
    dw[is.na(dw)] <- 0


    d <- melt(dw,id.vars = c("id","obs","date","time","feeder","environment","temperature","sc_name"),variable.name = "session",  value.name = "abundance")

    d <- subset(d,feeder !="")
    d$session <- as.character(d$session)
    d1 <- subset(d,session==1)
    d1$session <- "s1"
    d2 <- subset(d,session==2)
    d2$session <- "s2"
    dall <- aggregate(abundance~id+obs+date+time+feeder+environment+temperature+sc_name,data=d,max)
    dall$session <- "all"
    dall <- dall[,colnames(d1)]
    dall$session <- as.character(dall$session)
    dd <- rbind(rbind(d1,d2),dall)
    dd$pres <- as.numeric(dd$abundance>0)

    ddiv <- aggregate(pres~id+obs+date+time+feeder+environment+temperature+session,dd,sum)
    ddiv.w <- dcast(id+obs+date+time+feeder+environment+temperature~session,data=ddiv,value.var="pres")

    ddiv.w$diffToAll <- ddiv.w$all-ddiv.w$s1
    ddiv.w$propToAll <- ddiv.w$s1/ddiv.w$all
    ddiv.w$diffTo2 <- ddiv.w$s2-ddiv.w$s1
    ddiv.w$propTo2 <- ddiv.w$s1/ddiv.w$s2
    ddiv.w$obs <- as.character(ddiv.w$obs)
    colnames(ddiv.w)[8:14] <- paste("div_",colnames(ddiv.w)[8:14],sep="")



    dab <- aggregate(abundance~id+obs+date+time+feeder+environment+temperature+session,dd,sum)
    dab.w <- dcast(id+obs+date+time+feeder+environment+temperature~session,data=dab,value.var="abundance")

    dab.w$diffToAll <- dab.w$all-dab.w$s1
    dab.w$propToAll <- dab.w$s1/dab.w$all
    dab.w$diffTo2 <- dab.w$s2-dab.w$s1
    dab.w$propTo2 <- dab.w$s1/dab.w$s2

    dab.w$obs <- as.character(dab.w$obs)
    colnames(dab.w)[8:14] <- paste("ab_",colnames(dab.w)[8:14],sep="")


    dd.w <- merge(ddiv.w,dab.w,by=c("id","obs","date","time","feeder","environment","temperature"),all=TRUE)


    dupli <- duplicated(dd.w[,-1])
    dd.w <- dd.w[!dupli,]

    dd.w <- subset(dd.w,ab_all < 1000)

    dd.w$inf50 <- ifelse(dd.w$ab_all<50,"abundance all < 50","abundance all >= 50")


    ddpropToAll.gg <- melt(dd.w[,c(1:7,12,19,22)],id.vars=c("id","obs","date","time","feeder","environment","temperature","inf50"))
    ddpropToAll.gg$variable <- as.character(ddpropToAll.gg$variable)
    ddpropToAll.gg$variable[grep("div", ddpropToAll.gg$variable)] <- "diversity"
    ddpropToAll.gg$variable[grep("ab", ddpropToAll.gg$variable)] <- "abundance"
    colnames(ddpropToAll.gg)[ncol(ddpropToAll.gg)] <- "propToAll"



    ddpropTo2.gg <- melt(dd.w[,c(1:7,14,21,22)],id.vars=c("id","obs","date","time","feeder","environment","temperature","inf50"))
    ddpropTo2.gg$variable <- as.character(ddpropTo2.gg$variable)
    ddpropTo2.gg$variable[grep("div", ddpropTo2.gg$variable)] <- "diversity"
    ddpropTo2.gg$variable[grep("ab", ddpropTo2.gg$variable)] <- "abundance"
    colnames(ddpropTo2.gg)[ncol(ddpropTo2.gg)] <- "propTo2"




    dds1.gg <- melt(dd.w[,c(1:7,9,16,22)],id.vars=c("id","obs","date","time","feeder","environment","temperature","inf50"))
    dds1.gg$variable <- as.character(dds1.gg$variable)
    dds1.gg$variable[grep("div", dds1.gg$variable)] <- "diversity"
    dds1.gg$variable[grep("ab", dds1.gg$variable)] <- "abundance"
    colnames(dds1.gg)[ncol(dds1.gg)] <- "s1"


    dds2.gg <- melt(dd.w[,c(1:7,10,17,22)],id.vars=c("id","obs","date","time","feeder","environment","temperature","inf50"))
    dds2.gg$variable <- as.character(dds2.gg$variable)
    dds2.gg$variable[grep("div", dds2.gg$variable)] <- "diversity"
    dds2.gg$variable[grep("ab", dds2.gg$variable)] <- "abundance"
    colnames(dds2.gg)[ncol(dds2.gg)] <- "s2"


    dd.gg <- merge(merge(merge(dds1.gg,dds2.gg,by=c("id","obs","date","time","feeder","environment","temperature","inf50","variable")),ddpropToAll.gg,by=c("id","obs","date","time","feeder","environment","temperature","inf50","variable")),ddpropTo2.gg,by=c("id","obs","date","time","feeder","environment","temperature","inf50","variable"))



     gg <- ggplot(dd.gg,aes(x=s1,y=s2,colour=environment))+ geom_abline(slope=1,intercept=0,size=1.2)+ geom_point(alpha=.5,size=2)+facet_wrap(inf50~variable,scales="free")
    gg <- gg + labs(x="first session",y="second session",title = "Diference between first and second session",colour="Environment",shape="Observator")
gg
    ggsave("output/diff_s1_s2.png",gg,width=14,height=9)




   gg <- ggplot(dd.gg,aes(y=propToAll,x=""))+facet_wrap(inf50~variable,scales="free")+geom_violin()+geom_boxplot(width=.05)
    gg <- gg + labs(x="",y="First session / (First + second session)",title = "Proportion of observation during first session amoung both sessions")
    ggsave("output/prop_s1_all.png",gg)



    gg <- ggplot(dd.gg,aes(y=propToAll,x=""))+facet_grid(inf50~variable,scales="free")+geom_hline(yintercept=1,colour="blue",size=1.1)
    gg <- gg + geom_violin(size=.7,draw_quantiles = c(0.025,0.975),alpha=.7)
    gg <- gg + geom_violin(size=.7,draw_quantiles = c(0.25,0.75),fill=NA)+geom_violin(size=1.1,draw_quantiles = c(0.5),fill=NA)
  #  gg <- gg + geom_boxplot(width=.05)
    gg <- gg + labs(x="",y="First session / (First + second session)",title = "Proportion of observation during first session amoung both sessions")
    gg
    ggsave("output/prop_s1_all.png",gg)






    gg <- ggplot(dd.gg,aes(y=propTo2,x=""))+facet_grid(inf50~variable,scales="free")+geom_hline(yintercept=1,colour="blue",size=1.1)
    gg <- gg + geom_violin(size=.7,draw_quantiles = c(0.025,0.975),alpha=.7)
    gg <- gg + geom_violin(size=.7,draw_quantiles = c(0.25,0.75),fill=NA)+geom_violin(size=1.1,draw_quantiles = c(0.5),fill=NA)
  #  gg <- gg + geom_boxplot(width=.05)
    gg <- gg + labs(x="",y="First session / Second session",title = "Proportion of observation during first session in the face of second session")
    gg
    ggsave("output/prop_s1_s2.png",gg)





    d$pres <- as.numeric(d$abundance > 0)
    dsample <- aggregate(abundance~id+sc_name,d,max)
    dsample$pres <- as.numeric(dsample$abundance>0)
    ddsp <- aggregate(pres~sc_name,dsample,sum)

    nbsample <- length(unique(d$id))

    ddsp$prop <- ddsp$pres/nbsample


    dsample1 <- aggregate(abundance~id+sc_name,subset(d,session==1),max)
    dsample1$pres <- as.numeric(dsample1$abundance>0)
    colnames(dsample1)[3:4] <- paste(colnames(dsample1)[3:4],"s1",sep="_")
    dsample <- merge(dsample,dsample1,by=c("id","sc_name"),all.x=TRUE)

    dsample2 <- aggregate(abundance~id+sc_name,subset(d,session==2),max)
    dsample2$pres <- as.numeric(dsample2$abundance>0)
    colnames(dsample2)[3:4] <- paste(colnames(dsample2)[3:4],"s2",sep="_")
    dsample <- merge(dsample,dsample2,by=c("id","sc_name"),all.x=TRUE)

    dsample$diff_pres <- dsample$pres_s1 - dsample$pres_s2

    ddsp_session <- aggregate(diff_pres ~ sc_name,dsample,mean)

    ddsp <- merge(ddsp,ddsp_session,by="sc_name")


    ddsp$prop_pres <- abs(ddsp$diff_pres)
    ddsp$session_pref <- ifelse(ddsp$diff_pres > 0,"More in session 1",ifelse(ddsp$diff_pres < 0,"More in session 2","equally"))

   library(ggrepel)

    gg <- ggplot(subset(ddsp,session_pref != "equally"),aes(x=prop,y=prop_pres,label=sc_name))+ facet_grid(session_pref~.)
        gg <- gg + geom_point()
    gg <- gg + geom_text_repel()
    gg
    ggsave("output/unbalanced_sp_in_session.png",gg)


table_pref <- table(subset(ddsp,prop_pres > .5)$session_pref)


    md <- glmer( ~ session * feeder + (1|nomFile), ddiv,family = "poisson")
    smd <- summary(md)
    print(smd)


      dab <- aggregate(abundance~nomFile+date+feeder+environment+temperature+session,dd,sum)


    gg <- ggplot(dab,aes(y=abundance,x=session,group=nomFile,colour=feeder))+geom_point()+geom_line() + geom_boxplot()
    gg

library(lme4)
    md <- glmer(abundance ~ session * feeder + (1|nomFile), ddiv,family = "poisson")
    smd <- summary(md)
    print(smd)





}

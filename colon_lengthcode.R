library(ggplot2)
library(plyr)

####read and assign csv file (broken down further between sex#### 
hfdstudy <- read.csv("~/Penn State/Spring 2020/Obesity Study/Obstudy.csv")
View(hfdstudy)

####readout of standard statistical values####
summary(hfdstudy)

####read csv for male file####
obstudymale <- read.csv(file.choose(), header=T)

####barplot####
g<- ggplot(data=obstudymale, aes(x=Treatment, y=Colon))
b <- geom_bar(stat="summary", fun.y="mean", position="dodge", 
              aes(color=Treatment, fill=Treatment))
g+b

####assign male sd####
colonmalehfdsd <- sd(obstudymale[1:12,4])
colonmalenonosd <- sd(obstudymale[13:25, 4])
colonmalelonosd <- sd(obstudymale[26:37, 4])
colonmalehinosd <- sd(obstudymale[38:49,4])
colonmalenolosd <- sd(obstudymale[50:61, 4])
colonmalenohisd <- sd(obstudymale[62:73, 4])
colonmalelolosd <- sd(obstudymale[74:85, 4])
colonmalehihisd <- sd(obstudymale[86:97, 4])
maletreatmentssd <- c(colonmalehfdsd, 
                    colonmalenonosd, colonmalelonosd, 
                    colonmalehinosd,colonmalenolosd, 
                    colonmalenohisd, colonmalelolosd, colonmalehihisd)
transmaletreatsd <- t(maletreatmentssd)

####Error Bars#### 
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
m <-geom_errorbar(aes(x=Treatment, ymin=Colon-transmaletreatsd, ymax=Colon+transmaletreatsd,
            width=0.4, color="black"))

g+b+m



####Harder create Barplot####
colonmalehfd <- mean(obstudymale[1:12,4])
colonmalenono <- mean(obstudymale[13:25, 4])
colonmalelono <- mean(obstudymale[26:37, 4])
colonmalehino <- mean(obstudymale[38:49,4])
colonmalenolo <- mean(obstudymale[50:61, 4])
colonmalenohi <- mean(obstudymale[62:73, 4])
colonmalelolo <- mean(obstudymale[74:85, 4])
colonmalehihi <- mean(obstudymale[86:97, 4])
maletreatments <- c(colonmalehfd, 
    colonmalenono, colonmalelono, 
    colonmalehino,colonmalenolo, 
    colonmalenohi, colonmalelolo, colonmalehihi)
transmaletreat <- t(maletreatments)
barplot(transmaletreat, main="Colon Length (cm) for Each Treatment in Males", 
        xlab = "Treatment", 
        ylab = "Average Colon Length (cm)",
        names.arg=c("HFD","NO/NO","LO/NO","HI/NO","NO/LO","NO/HI","LO/LO","HI/HI"))


####barplot for Female Colon Length####
colonfemhfd <- mean(obstudyfem[1:12,4])
colonfemnono <- mean(obstudyfem[13:24, 4])
colonfemlono <- mean(obstudyfem[25:35, 4])
colonfemhino <- mean(obstudyfem[36:47,4])
colonfemnolo <- mean(obstudyfem[48:59, 4])
colonfemnohi <- mean(obstudyfem[60:71, 4])
colonfemlolo <- mean(obstudyfem[72:83, 4])
colonfemhihi <- mean(obstudyfem[84:95, 4])
femtreatments <- c(colonfemhfd, 
                    colonfemnono, colonfemlono, 
                    colonfemhino,colonfemnolo, 
                    colonfemnohi, colonfemlolo, colonfemhihi)
transfemtreat <- t(femtreatments)
barplot(transfemtreat, main="Colon Length for Each Treatment in Females", 
        xlab = "Treatment", 
        ylab = "Average Colon Length (cm)",
        names.arg=c("HFD","NO/NO","LO/NO","HI/NO","NO/LO","NO/HI","LO/LO","HI/HI"))





###Create barplot###??
barplot(Tukeycolon, main = "Colon Length (cm) Average for each Treatment", 
        xlab = "Treatments", ylab = "Colon Length (cm)")

####stdev####
colonfemhfdsd <- sd(obstudyfem[1:12,4])
colonfemnonosd <- sd(obstudyfem[13:24, 4])
colonfemlonosd <- sd(obstudyfem[25:35, 4])
colonfemhinosd <- sd(obstudyfem[36:47,4])
colonfemnolosd <- sd(obstudyfem[48:59, 4])
colonfemnohisd <- sd(obstudyfem[60:71, 4])
colonfemlolosd <- sd(obstudyfem[72:83, 4])
colonfemhihisd <- sd(obstudyfem[84:95, 4])
sdcolmal <- c(sd(obstudyfem[1:12,4]),colonfemnonosd,colonfemlonosd,
              colonfemhinosd, colonfemnolosd, colonfemnohisd, 
              colonfemlolosd, colonfemhihisd) 

####barplot####
g<- ggplot(data=obstudymale, aes(x=Treatment, y=Colon))
b <- geom_bar(stat="summary", fun.y="mean", position="dodge", 
              aes(color=Treatment, fill=Treatment))
g+b
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
f <- geom_errorbar(aes(x=Treatment, ymin=Colon-sd(c(colonfemhfd,colonfemnonosd,colonfemlonosd,
                      colonfemhinosd, colonfemnolosd, colonfemnohisd, 
                      colonfemlolosd, colonfemhihisd), 
                      ymax=Colon+sd(c(colonfemhfd,colonfemnonosd,colonfemlonosd,
                      colonfemhinosd, colonfemnolosd, colonfemnohisd, 
                      colonfemlolosd, colonfemhihisd)),
                       width=0.4, color="black")))
e<- geom_errorbar(aes(x=Treatment, ymin=Colon-sd, ymax=Colon+sd, width=0.4,
                      color="black"))

g+b+e


###run 1-way ANOVA and Tukey's on colon length### 
colonlengthaov <- aov(Colon~Treatment, data=hfdstudy)
TukeyHSD(colonlengthaov)
Tukeycolon <- TukeyHSD(colonlengthaov)
####1-way ANOVA females colon length####
obstudyfem <- read.csv(file.choose(), header=T)
colonfemaov <-
  
###2-way ANOVA on sex + colon length, not necessary###
sexcolon <- aov(Colon~Treatment+Sex+Treatment:Sex, data=hfdstudy)
TukeyHSD(sexcolon)
Tukeysexcolon <- TukeyHSD(sexcolon)
####1-way ANOVA on colon length for males####
obstudymale <- read.csv(file.choose())
colonmaleaov <- aov(Colon~Treatment, data=obstudymale)
tukeycolonmale <- TukeyHSD(colonmaleaov)
summary(tukeycolonmale)
####install ggplot2#####
install.packages("ggplot2")
library(ggplot2)

####open and assign file for males, make sure files are saved as CSV####
obstudymale <- read.csv(file.choose(), header=T)

####assign variables for colon legnth based on treatment####
malcolhfd <- obstudymale[1:12,4]



####retrofat####
####Assign variables for Avg Retro Fat weight Based on Treatment####
maleretrohfd <- mean(obstudymale[1:12,5])
maleretronono <- mean(obstudymale[13:25, 5])
maleretrolono <- mean(obstudymale[26:37, 5])
maleretrohino <- mean(obstudymale[38:49, 5])
maleretronolo <- mean(obstudymale[50:61, 5])
maleretronohi <- mean(obstudymale[62:73, 5])
maleretrololo <- mean(obstudymale[74:85, 5])

malretroavg <- c(maleretrohfd,maleretronono, maleretrolono, maleretrohino, 
                 maleretronolo, maleretronohi, maleretrololo,maleretrohihi)
transmalretroavg <- t(malretroavg)
maleretrohihi <- mean(obstudymale[86:97, 5])
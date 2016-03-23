
## Source: The Butterfly Did It: The Aberrant Vote for Buchanan in
## Palm Beach County, Florida by JONATHAN N. WAND, KENNETH W. SHOTTS,
## JASJEET S. SEKHON, WALTER R. MEBANE, JR., and MICHAEL C. HERRON
## American Political Science Review Vol. 95, No. 4 December 2001

## We thank the authors for sharing data with us.
## The code is revised based on the original code provided by the authors


library(ggplot2)

d<-read.csv("abseday.FL2000.csv")
names(d)
head(d)

## [1] "X."                "County"            "Total.Pres.Votes" 
## [4] "Total.Buchanan"    "Total.Absentee"    "Buchanan.Absentee"
## [7] "Elecday.Buch.."    "Absentee.Buch.."   "Abs.Elec.Ratio"   
##[10] "Difference"       

d$totpres  <- d$Total.Pres.Votes
d$totabs <- d$Total.Absentee
d$toteday  <- d$Total.Pres.Votes - d$Total.Absentee

d$totbuch  <- d$Total.Buchanan
d$absbuch  <- d$Buchanan.Absentee
d$edaybuch <- d$Total.Buchanan - d$Buchanan.Absentee

d$share.both<-d$totbuch/d$totpres*100
d$share.eday<-d$edaybuch/d$toteday*100
d$share.abs<-d$absbuch/d$totabs*100
d$share.diff  <- d$share.eday-d$share.abs

## raw plot
qplot(totpres,totbuch, data=d, geom=c("point", "smooth"),
      xlab="Total Votes", ylab="Votes for Buchanan")+
    annotate(geom="text", x=d$totpres[50], y=d$totbuch[50]+200,
             label="Palm Beach",colour="red") +
    annotate(geom="point", x=d$totpres[50], y=d$totbuch[50],
             colour="red")


qplot(log(totpres),log(totbuch), data=d, geom=c("point", "smooth"),
       xlab="Log Total Votes", ylab="Log Votes for Buchanan") +
    annotate(geom="text", x=log(d$totpres[50]),
             y=log(d$totbuch[50])+0.2, label="Palm Beach",
             colour="red") +
    annotate(geom="point", x=log(d$totpres[50]),
             y=log(d$totbuch[50]), colour="red") 


## studentized t
library(lattice)
mod<-lm(totbuch~totpres + I(totpres^2),data=d)
d$studresid <- rstudent(mod)

ggplot(d, aes(x=studresid)) +
    geom_density(colour="gray20", fill="gray", alpha=0.5)+
    xlab("Studentized Residual") + ylab("Density") +
    annotate(geom="text", x=8, y=0.2, label="Palm\nBeach",
             colour="red") 


## two types of votes
qplot(log(totabs),log(absbuch+1), data=d,
          geom=c("point", "smooth"), 
          xlab="Log Absentee Votes",
          ylab="Log Absentee Votes for Buchanan") +
    annotate(geom="text", x=log(d$totabs[50])-0.2,
             y=log(d$absbuch[50]+1)+0.5, label="Palm Beach",
             colour="red") +
    annotate(geom="point", x=log(d$totabs[50]),
             y=log(d$absbuch[50]+1), colour="red") +
    annotate("text",  x=7, y =3, label = "Absentee",
             vjust=1, hjust=1,size=8,colour="gray50")


s<-d[-50,] ## take Palm Beach out

qplot(log(toteday),log(edaybuch+1), data=s,
          geom=c("point", "smooth"), 
          xlab="Log ElectionDay Votes",
          ylab="Log ElectionDay Votes for Buchanan") +
    annotate(geom="text", x=log(d$toteday[50]),
             y=log(d$edaybuch[50])+0.2, label="Palm Beach",
             colour="red") +
    annotate(geom="point", x=log(d$toteday[50]),
             y=log(d$totbuch[50]), colour="red") +
    annotate("text",  x=9.5, y = 6,
             label = "ElectionDay", vjust=1, hjust=1,size=8,colour="gray50")



## main plot (difference)
d$lower<--1/sqrt(d$totpres)*100
d$upper<-1/sqrt(d$totpres)*100

library(scales)
ggplot(d, aes(totpres,share.diff)) + geom_point() +
    geom_ribbon(aes(x=totpres,ymin=lower,ymax=upper),alpha=0.15)+
    geom_smooth(method="loess", se=FALSE, size=1) +
            xlab("Total Votes") + ylab("Buchanan %: ElectionDay - Absentee")+
    annotate(geom="text", x=d$totpres[50],
             y=d$share.diff[50]+0.2, label="Palm Beach",colour="red") +
    annotate(geom="point", x=d$totpres[50],
             y=d$share.diff[50],colour="red") +
                        scale_x_continuous(labels = comma)



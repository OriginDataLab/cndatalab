
## Data Source:

## Dali Yang, Calamity and Reform in China: State, Rural Society, and
## Institutional Change Since the Great Leap Famine, Stanford
## University Press, Stanford, California, 1996.

library(ggplot2)
d<-read.csv("famine.csv")
names(d)
head(d)

## Determinants of mess hall

## distance and party member
ggplot(d, aes(x=distance, y=party56)) + 
    geom_text(aes(label=prov)) +
    geom_smooth() +
    ylab("Party member % (1956)") +
    xlab("Distance to Beijing") +
    scale_x_log10(breaks=c(500,1000,2000,4000)) 

## party member and mess hall
ggplot(d, aes(x=party56, y=messhall)) + 
    geom_text(aes(label=prov))  +
    geom_smooth() +
    xlab("Party member % (1956)") +
    ylab("Mess hall / rural population % (1959)") +
    ylim(0,110)

## income and mess hall
ggplot(d, aes(x=income57, y=messhall)) + 
    geom_text(aes(label=prov))  +
    geom_smooth() +
    xlab("Per capita income (1957)") +
    ylab("Mess hall / rural population % (1959)")  +
    scale_x_log10(breaks=c(100,150,200,250)) +
    ylim(0,110)

summary(lm(messhall ~ log(distance) + party56 + income57, data=d))

## Determinants of death

## Rank of death rate

ggplot(d, aes(x=death, y=reorder(prov, death))) +
    geom_segment(aes(yend=prov), xend=0, colour="grey50", size=1.2) +
    geom_point(size=3, colour="red") +
    xlab("Abnormal death (1959-62)") +
    ylab("")  + theme(text = element_text(size=15)) 



## Messhall and death
ggplot(d, aes(x=messhall, y=death)) + 
    geom_text(aes(label=prov))  +
    geom_smooth() +
    xlab("Mess hall / rural population (1959) % ") +
    ylab("Abnormal death (1959-62)") +
    scale_y_log10(breaks=c(25,50,100,200,400))

## Income and death
ggplot(d, aes(x=income57, y=death)) + 
    geom_text(aes(label=prov))  +
    geom_smooth() +
    xlab("Per capita income (1957)") +
    ylab("Abnormal death (1959-62)") +
    scale_x_log10(breaks=c(100,150,200,250)) +
    scale_y_log10(breaks=c(25,50,100,200,400))

summary(lm(death ~ messhall + income57, data=d))

## Determinants of reform


ggplot(d, aes(x=death, y=brigade79)) + 
    geom_text(aes(label=prov))  +
    geom_smooth() +
    xlab("Abnormal death (1959-62)") +
    ylab("Percentage of brigade accounting (1979) ")+
    scale_x_log10(breaks=c(25,50,100,200,400))



summary(lm(brigade81 ~ log(death) + income57 + distance + party56, data=d))

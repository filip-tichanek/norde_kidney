---
title: "SURVIVAL IN KIDNEY AND BLADDER CANCERS IN THE NORDIC COUNTRIES THROUGH A HALF CENTURY"
subtitle: "Full R code"
format: 
  html:
    code-fold: true
    code-summary: "Show the code"
    embed-resources: true
    keep-md: true
    toc: true
    toc-depth: 4
    toc-location: left
    number-sections: true
    number-depth: 4
editor: visual
project:
  type: default
  output-dir: output
theme: darkly
fontsize: 13px
---



To see the code, click on the ***Show the code***

# Package upload


::: {.cell}

```{.r .cell-code}
rm(list = ls())
suppressWarnings(suppressMessages( {
  library(brms)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(sessioninfo)
  } ) )
```
:::


# Incidence and mortality

## Data upload and wrangling


::: {.cell}

```{.r .cell-code}
# setting parameters for smoothing
spar = 0.4
knot = 12

# colors ---------------------------------------------------------

cola <- c(
  rgb(1, 0.1, 0.1, alpha = 1),
  rgb(0.1, 0.1, 1, alpha = 1),
  rgb(0, 0.6, 0.3, alpha = 1),
  rgb(0.7, 0.7, 0.1, alpha = 1))

colb<-c(
  rgb(1, 0.1, 0.1, alpha = 0.2),
  rgb(0.1, 0.1, 1, alpha = 0.2),
  rgb(0, 0.6, 0.3, alpha = 0.2),
  rgb(0.7, 0.7, 0.1, alpha = 0.2))

colc<-c(
  rgb(1, 0.1, 0.1, alpha = 0.8),
  rgb(0.1, 0.1, 1, alpha = 0.8),
  rgb(0, 0.6, 0.3, alpha = 0.8),
  rgb(0.7, 0.7, 0.1, alpha = 0.8))


# kidney data
urlfile="https://raw.githubusercontent.com/filip-tichanek/nord_kidney/main/source/kidney_inc_mor.csv"
kidney_inc_mor <- read.csv(url(urlfile),sep=",")
colnam<-kidney_inc_mor[,1]
kidney_inc_mor<-data.frame(t(kidney_inc_mor))[-1, ]
colnames(kidney_inc_mor) <- colnam
kidney_inc_mor$year <- 1943:2020

### Subseting years 1961-2020
kidney_inc_mor <- kidney_inc_mor[kidney_inc_mor$year > 1960, ]
kidney_inc_mor[1:4, 1:4]
```

::: {.cell-output .cell-output-stdout}
```
      Denmark, incidence, males Finland, incidence, males
X1961                       6.1                       3.5
X1962                       5.3                       3.6
X1963                       6.1                       4.0
X1964                       6.2                       4.7
      Norway, incidence, males Sweden, incidence, males
X1961                      5.4                      7.1
X1962                      4.8                      7.9
X1963                      5.0                      6.7
X1964                      5.1                      7.7
```
:::

```{.r .cell-code}
### Removing space character and converting characters to numbers
for(x in 1:dim(kidney_inc_mor)[2]){
kidney_inc_mor[,x] <- str_trim(kidney_inc_mor[,x])
kidney_inc_mor[,x] <- as.numeric(kidney_inc_mor[,x]) }
summary(kidney_inc_mor) 
```

::: {.cell-output .cell-output-stdout}
```
 Denmark, incidence, males Finland, incidence, males Norway, incidence, males
 Min.   : 5.300            Min.   : 3.500            Min.   : 4.800          
 1st Qu.: 7.200            1st Qu.: 7.200            1st Qu.: 6.600          
 Median : 7.650            Median : 9.750            Median : 8.200          
 Mean   : 8.408            Mean   : 8.972            Mean   : 8.665          
 3rd Qu.: 8.925            3rd Qu.:10.925            3rd Qu.:10.025          
 Max.   :13.200            Max.   :11.900            Max.   :13.700          
                                                                             
 Sweden, incidence, males Denmark, incidence, females
 Min.   : 6.200           Min.   :3.400              
 1st Qu.: 7.475           1st Qu.:4.400              
 Median : 8.450           Median :4.800              
 Mean   : 8.453           Mean   :4.777              
 3rd Qu.: 9.300           3rd Qu.:5.200              
 Max.   :10.400           Max.   :6.200              
                                                     
 Finland, incidence, females Norway, incidence, females
 Min.   :3.00                Min.   :2.700             
 1st Qu.:4.20                1st Qu.:3.900             
 Median :5.65                Median :4.350             
 Mean   :5.28                Mean   :4.518             
 3rd Qu.:6.10                3rd Qu.:5.125             
 Max.   :6.80                Max.   :7.100             
                                                       
 Sweden, incidence, females Denmark, mortality, males Finland, mortality, males
 Min.   :3.400              Min.   :2.200             Min.   :2.500            
 1st Qu.:4.500              1st Qu.:3.650             1st Qu.:3.500            
 Median :5.150              Median :4.400             Median :4.150            
 Mean   :5.083              Mean   :4.169             Mean   :4.123            
 3rd Qu.:5.700              3rd Qu.:4.700             3rd Qu.:4.800            
 Max.   :6.600              Max.   :5.500             Max.   :5.500            
                            NA's   :1                                          
 Norway, mortality, males Sweden, mortality, males Denmark, mortality, females
 Min.   :2.600            Min.   :2.300            Min.   :0.930              
 1st Qu.:3.500            1st Qu.:3.700            1st Qu.:1.850              
 Median :3.800            Median :4.650            Median :2.800              
 Mean   :3.812            Mean   :4.448            Mean   :2.566              
 3rd Qu.:4.200            3rd Qu.:5.425            3rd Qu.:3.100              
 Max.   :5.200            Max.   :6.400            Max.   :4.200              
                                                   NA's   :1                  
 Finland, mortality, females Norway, mortality, females
 Min.   :1.300               Min.   :1.000             
 1st Qu.:1.900               1st Qu.:1.500             
 Median :2.200               Median :2.000             
 Mean   :2.157               Mean   :1.872             
 3rd Qu.:2.400               3rd Qu.:2.200             
 Max.   :2.900               Max.   :2.500             
                                                       
 Sweden, mortality, females      year     
 Min.   :1.100              Min.   :1961  
 1st Qu.:2.100              1st Qu.:1976  
 Median :2.800              Median :1990  
 Mean   :2.587              Mean   :1990  
 3rd Qu.:3.300              3rd Qu.:2005  
 Max.   :3.700              Max.   :2020  
                                          
```
:::

```{.r .cell-code}
# bladder data 
urlfile="https://raw.githubusercontent.com/filip-tichanek/nord_kidney/main/source/bladder_inc_mor.csv"
bladder_inc_mor <- read.csv(url(urlfile),sep=",")
colnam <- bladder_inc_mor[,1]
bladder_inc_mor <- data.frame(t(bladder_inc_mor))[-1,]
colnames(bladder_inc_mor) <- colnam
bladder_inc_mor$year <- 1943:2020

## Subseting years 1961-2020
bladder_inc_mor <- bladder_inc_mor[bladder_inc_mor$year>1960,]
bladder_inc_mor[1:4, 1:4]
```

::: {.cell-output .cell-output-stdout}
```
      Denmark, incidence, males Finland, incidence, males
X1961                     14.10                      7.00
X1962                     16.10                      6.60
X1963                      16.4                       8.5
X1964                      15.3                       9.1
      Norway, incidence, males Sweden, incidence, males
X1961                     9.10                     8.70
X1962                     9.90                     9.50
X1963                     10.6                     10.2
X1964                     12.1                      9.8
```
:::

```{.r .cell-code}
## Removing space character and converting characters to numbers
for (x in 1:dim(kidney_inc_mor)[2]){
  bladder_inc_mor[,x] <- str_trim(bladder_inc_mor[,x])
  bladder_inc_mor[,x] <- as.numeric(bladder_inc_mor[,x]) }
summary(bladder_inc_mor) 
```

::: {.cell-output .cell-output-stdout}
```
 Denmark, incidence, males Finland, incidence, males Norway, incidence, males
 Min.   :14.10             Min.   : 6.60             Min.   : 9.10           
 1st Qu.:24.50             1st Qu.:13.78             1st Qu.:17.15           
 Median :27.70             Median :15.15             Median :20.20           
 Mean   :26.37             Mean   :14.37             Mean   :18.68           
 3rd Qu.:29.00             3rd Qu.:15.85             3rd Qu.:21.40           
 Max.   :31.90             Max.   :18.30             Max.   :23.60           
                                                                             
 Sweden, incidence, males Denmark, incidence, females
 Min.   : 8.70            Min.   :4.600              
 1st Qu.:15.65            1st Qu.:6.975              
 Median :18.20            Median :8.500              
 Mean   :16.78            Mean   :7.943              
 3rd Qu.:18.60            3rd Qu.:9.000              
 Max.   :20.00            Max.   :9.800              
                                                     
 Finland, incidence, females Norway, incidence, females
 Min.   :1.300               Min.   :2.900             
 1st Qu.:2.975               1st Qu.:4.950             
 Median :3.450               Median :5.600             
 Mean   :3.237               Mean   :5.458             
 3rd Qu.:3.600               3rd Qu.:6.100             
 Max.   :4.100               Max.   :6.900             
                                                       
 Sweden, incidence, females Denmark, mortality, males Finland, mortality, males
 Min.   :3.500              Min.   : 4.400            Min.   :2.900            
 1st Qu.:4.700              1st Qu.: 6.900            1st Qu.:3.600            
 Median :5.400              Median : 8.200            Median :4.500            
 Mean   :5.212              Mean   : 8.198            Mean   :4.445            
 3rd Qu.:5.725              3rd Qu.: 9.650            3rd Qu.:5.200            
 Max.   :6.800              Max.   :11.400            Max.   :6.900            
                            NA's   :1                                          
 Norway, mortality, males Sweden, mortality, males Denmark, mortality, females
 Min.   :3.400            Min.   :3.600            Min.   :1.500              
 1st Qu.:4.700            1st Qu.:4.300            1st Qu.:2.400              
 Median :5.450            Median :4.600            Median :2.600              
 Mean   :5.383            Mean   :4.587            Mean   :2.658              
 3rd Qu.:6.100            3rd Qu.:4.800            3rd Qu.:3.000              
 Max.   :7.000            Max.   :5.800            Max.   :3.600              
                                                   NA's   :1                  
 Finland, mortality, females Norway, mortality, females
 Min.   :0.730               Min.   :1.200             
 1st Qu.:0.920               1st Qu.:1.575             
 Median :1.000               Median :1.900             
 Mean   :1.073               Mean   :1.830             
 3rd Qu.:1.200               3rd Qu.:2.000             
 Max.   :1.900               Max.   :2.500             
                                                       
 Sweden, mortality, females      year     
 Min.   :1.400              Min.   :1961  
 1st Qu.:1.500              1st Qu.:1976  
 Median :1.600              Median :1990  
 Mean   :1.602              Mean   :1990  
 3rd Qu.:1.700              3rd Qu.:2005  
 Max.   :2.100              Max.   :2020  
                                          
```
:::
:::


## Plotting


::: {.cell}

```{.r .cell-code}
## General setting and titles of plots

m <- matrix(c(9,1,2
             ,3,5,6
             ,4,7,8), nrow = 3, ncol =3 ,byrow = TRUE)
layout(mat = m,heights = c(0.04,0.96/2,0.96/2),widths = c(0.04,0.96/2,0.96/2))
par(mgp=c(1.6,0.8,0))
par(mar=c(0,0,0,0))

plot(NULL, axes = FALSE,xlab = "", ylab = "", xlim=c(-1,1), ylim = c(-0.85, 0.85))
text(0, -0.2, "Kidney cancer", cex = 1.8, font = 3, xpd=TRUE)
plot(NULL, axes=FALSE, xlab = "", ylab="", xlim = c(-1,1),ylim=c(-0.85,0.85))
text(0, -0.2, "Bladder cancer", cex = 1.8, font = 3, xpd = TRUE)

par(mar=c(2, 0, 0, 0))
plot(NULL, axes = FALSE, xlab = "", ylab = "", xlim = c(-1,1), ylim=c(0,1))
text(-0.2, 0.5, "Incidence per 100,000 (ASR - World)", cex = 1.5, srt=90)

par(mar = c(2, 0, 0, 0))
plot(NULL, axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(0, 1))
text(-0.2, 0.5, "Mortality per 100,000 (ASR - World)", cex = 1.5, srt = 90)

# Kidney incidence plot

data = kidney_inc_mor
par(mgp = c(1.4, 0.6, 0))
par(mar = c(2, 0.5, 0, 0))
tckk = -0.017

range <- c(0, 14); scal <- range[2]-range[1]
xrange <- c(1961, 2020)
plot(NULL, xlim = xrange, ylim = c(range[1], range[2]), xlab="", ylab="" ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="white", border=NA)
x <- range[1]
repeat{
  lines(c(xrange[1], xrange[2]), c(x, x),col = "grey90", lwd = 0.7)
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x), c(range[1],  range[2]),col= "grey96", lwd = 0.7)
  x=x+10; if(x>2020){break}}

data = kidney_inc_mor

for (xx in 1:4){
smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
lines(smoothingSpline, col= colc[xx], lty = 1, lwd = 2, lend = 1)}

for (xx in 5:8){
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline, col = colc[xx-4], lty=4, lwd=2, lend=1) }

axis(2, las = 2, cex.axis = 1.4, at = seq(range[1], range[2], by=2),
     labels = c(rep("", length(seq(range[1], range[2], by = 2)))),
     pos = xrange[1], tck = tckk)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     pos=xrange[1],tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     labels=c(rep("",length(c(seq(round(xrange[1]+5,-1),2020,by=10))))),tck=tckk)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=20)),
     pos=range[1],tck=tckk)
title(xlab="Year", line=1, cex.lab=1.4,xpd=TRUE)
text(1965,range[2]-0.05*scal,"a",cex=2.5)

xx=1;yy=range[1]+scal*0.22
rect(1987,yy+0.035*scal,2015,yy-0.18*scal,col="white",border="grey50",lwd=0.8)
repeat{
  lines(c(1999.5,2001.7),c(yy,yy),lwd=10,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.05);if(xx>4){break}}

text(1993,0.21*scal,"Males",cex=1.25)
lines(c(1988.3,1997.1),c(0.16*scal,0.16*scal),lwd=2)

text(1993,0.12*scal,"Females",cex=1.25)
lines(c(1988.3,1997.1),c(0.07*scal,0.07*scal),lwd=2,lty=4)

xx=1;yy=range[1]+scal*0.22
text(2008.5,yy,"Denmark",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.05)
text(2008.5,yy,"Finland",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.05)
text(2008.5,yy,"Norway",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.05)
text(2008.5,yy,"Sweden",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.05)

# Bladder incidence plot

data=bladder_inc_mor

range<-c(0,32);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="white",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="grey96",lwd=0.7)
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

data=bladder_inc_mor
for (xx in 1:4){
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx],lty=1,lwd=2,lend=1) }

for (xx in 5:8){
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx-4],lty=4,lwd=2,lend=1) }

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck=tckk)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=4),
     pos=xrange[1],tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     labels=c(rep("",length(c(seq(round(xrange[1]+5,-1),2020,by=10))))),tck=tckk)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=20)),
     pos=range[1],tck=tckk)
title(xlab="Year", line=1, cex.lab=1.4,xpd=TRUE)

text(1965,range[2]-0.05*scal,"b",cex=2.5)


# kidney mortality

### WARNING! Due to a bug in NORDCAN database (Denmark mortality data in 1969),
### some data were removed from table (these evidently incorrect).
### The bug was reported to NORDCAN secretary on 7th of November, 2022

data=kidney_inc_mor
  
  range<-c(0,6);scal<-range[2]-range[1]
  xrange<-c(1961,2020)
  
  plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
       ,las=1, axes=FALSE)
  rect(xrange[1],range[2],xrange[2],range[1],col="white",border=NA)
  x<-range[1]
  repeat{
    lines(c(xrange[1],xrange[2]),c(x,x),col="grey96",lwd=0.7)
    x=x+2;if(x>range[2]){break}}
  
  x<- round(xrange[1]+5,-1)
  repeat{
    lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
    x=x+10;if(x>2020){break}}
  
for (xx in 9:12){
    data = na.omit(kidney_inc_mor[,c(xx,17)] )
    smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
    lines(smoothingSpline,col=colc[xx-8],lty=1,lwd=2,lend=1) }
  
for (xx in 13:16){
    data = na.omit(kidney_inc_mor[,c(xx,17)] )
    smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
      lines(smoothingSpline,col=colc[xx-12],lty=4,lwd=2,lend=1) }
  
  
  axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
       labels=c(rep("",length(seq(range[1],range[2],by=2)))),
       pos=xrange[1],tck=tckk)
  axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
       pos=xrange[1],tck=tckk)
  axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
       labels=c(rep("",length(c(seq(round(xrange[1]+5,-1),2020,by=10))))),tck=tckk)
  lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
  axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=20)),
       pos=range[1],tck=tckk)
  title(xlab="Year", line=1, cex.lab=1.4,xpd=TRUE)
  text(1965,range[2]-0.05*scal,"c",cex=2.5)

  
  
# bladder mortality
  
### WARNING! Due to a bug in NORDCAN database (Denmark mortality data in 1969),
### some data were removed from table (these evidently incorrect).
### The bug was reported to NORDCAN secretary on 7th of November, 2022
  
  data=bladder_inc_mor
  range<-c(0,12);scal<-range[2]-range[1]
  xrange<-c(1961,2020)
  
  plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
       ,las=1, axes=FALSE)
  rect(xrange[1],range[2],xrange[2],range[1],col="white",border=NA)
  x<-range[1]
  repeat{
    lines(c(xrange[1],xrange[2]),c(x,x),col="grey96",lwd=0.7)
    x=x+2;if(x>range[2]){break}}
  
x<- round(xrange[1]+5,-1)
  repeat{
    lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
    x=x+10;if(x>2020){break}}
  
 for (xx in 9:12){
    data= na.omit(bladder_inc_mor[,c(xx,17)] )
    smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
    lines(smoothingSpline, col=colc[xx-8],lty=1,lwd=2,lend=1) }
  
 for (xx in 13:16){
    data = na.omit(bladder_inc_mor[,c(xx,17)] )
    smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
    lines(smoothingSpline, col=colc[xx-12],lty=4,lwd=2,lend=1)}
  
  axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
       labels=c(rep("",length(seq(range[1],range[2],by=2)))),
       pos=xrange[1],tck=tckk)
  
  axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
       pos=xrange[1],tck=tckk)
  
  axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
       labels=c(rep("",length(c(seq(round(xrange[1]+5,-1),2020,by=10))))),tck=tckk)
  lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
  axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=20)),
       pos=range[1],tck=tckk)
  title(xlab="Year", line=1, cex.lab=1.4,xpd=TRUE)
  
  text(1965,range[2]-0.05*scal,"d",cex=2.5)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-3-1.png){width=604.8}
:::
:::


# Survival trends

This code aim to time trends in the survival of kidney and bladder cancers, using Bayesian Generalized Additive Models. Data were downloaded from [NORDCAN](https://nordcan.iarc.fr/en/dataviz/survival?sexes=2&cancers=180_230_190_200_220&populations=752&mode=cancer&multiple_populations=0&multiple_cancers=1&survival=1) website.


::: {.cell}

```{.r .cell-code}
cola<-c(
  rgb(1, 0, 0, alpha=1),
  rgb(0.2,0.2,1,alpha=1),
  rgb(0, 0.7, 0,alpha=1),
  rgb(0.7,0.7,0.1,alpha=1))

alp=0.18;colb<-c(
  rgb(1, 0, 0, alpha=alp),
  rgb(0.2, 0.2, 1, alpha=alp),
  rgb(0, 0.9, 0,alpha=alp),
  rgb(0.7,0.7,0.1,alpha=alp))

colc<-c(
  rgb(1,0,0,alpha=0.8),
  rgb(0.2,0.2,1,alpha=0.8),
  rgb(0, 0.9, 0,alpha=0.8),
  rgb(0.7,0.7,0.1,alpha=0.8))
```
:::


### Data upload


::: {.cell}

```{.r .cell-code}
# Kidney data upload
urlfile <- "https://raw.githubusercontent.com/filip-tichanek/nord_kidney/main/source/kidney_1y.csv"
kidney_1y <- read.csv(url(urlfile),sep=";")

urlfile <- "https://raw.githubusercontent.com/filip-tichanek/nord_kidney/main/source/kidney_5y.csv"
kidney_5y <- read.csv(url(urlfile),sep=";")

# Bladder data upload
urlfile <- "https://raw.githubusercontent.com/filip-tichanek/nord_kidney/main/source/bladder_1y.csv"
bladder_1y <- read.csv(url(urlfile),sep=";")

urlfile <- "https://raw.githubusercontent.com/filip-tichanek/nord_kidney/main/source/bladder_5y.csv"
bladder_5y <- read.csv(url(urlfile),sep=";")
```
:::


### Kidney data wrangling


::: {.cell}

```{.r .cell-code}
kidney_1y_est<-data.frame(kidney_1y[, 1])
for (x in 2:9){
  kidney_1y_est[,x] <- str_sub(kidney_1y[, x], 1, 4) }

for (x in 10:17){
  kidney_1y_est[,x]<-str_sub(kidney_1y[, x-8], 6, 9) }

kidney_5y_est<-data.frame(kidney_5y[,1])
for (x in 2:9){kidney_5y_est[,x] <- str_sub(kidney_5y[,x], 1, 4) }
for (x in 10:17){kidney_5y_est[,x] <- str_sub(kidney_5y[,x-8], 6, 9) }

kidney<-(data.frame(unlist(kidney_1y_est[,2:9])));colnames(kidney)<-"surv_1y"
kidney$cil_1y<-as.numeric(unlist(kidney_1y_est[,10:17]))
kidney$surv_5y<-as.numeric(unlist(kidney_5y_est[,2:9]))
kidney$cil_5y<-as.numeric(unlist(kidney_5y_est[,10:17]))
kidney$year<-rep(seq(1973,2018,by=5),8)
kidney$sex<-c(rep("Males",40),rep("Females",40))
kidney$country<-c(rep(c(rep("Denmark",10), rep("Finland",10), rep("Norway",10),
                        rep("Sweden",10)),2))
kidney$shou<-c(rep(c(rep("den_mal_",10), rep("fin_mal_",10), rep("nor_mal_",10),
                     rep("swe_mal_",10), rep("den_fem_",10), rep("fin_fem_",10),
                     rep("nor_fem_",10), rep("swe_fem_",10)),1))
kidney$group<-interaction(kidney$country,kidney$sex)
kidney$years10cen<-(kidney$year-1995.5)/10
kidney$surv_1y<-as.numeric(kidney$surv_1y)
kidney$surv_5y<-as.numeric(kidney$surv_5y)
kidney$cil_1y<-as.numeric(kidney$cil_1y)
kidney$cil_5y<-as.numeric(kidney$cil_5y)
kidney$se_1y<-(kidney$surv_1y-kidney$cil_1y)/1.96
kidney$se_5y<-(kidney$surv_5y-kidney$cil_5y)/1.96
kidney$surv_cond<-(kidney$surv_5y/kidney$surv_1y)*100
summary(kidney)
```

::: {.cell-output .cell-output-stdout}
```
    surv_1y          cil_1y         surv_5y          cil_5y           year     
 Min.   :47.60   Min.   :44.30   Min.   :27.00   Min.   :23.50   Min.   :1973  
 1st Qu.:59.10   1st Qu.:55.27   1st Qu.:38.42   1st Qu.:35.25   1st Qu.:1983  
 Median :70.50   Median :67.60   Median :50.80   Median :47.80   Median :1996  
 Mean   :69.72   Mean   :67.18   Mean   :52.05   Mean   :48.99   Mean   :1996  
 3rd Qu.:80.00   3rd Qu.:78.17   3rd Qu.:64.20   3rd Qu.:61.58   3rd Qu.:2008  
 Max.   :91.30   Max.   :90.20   Max.   :80.00   Max.   :77.10   Max.   :2018  
                                                                               
     sex              country              shou                       group   
 Length:80          Length:80          Length:80          Denmark.Females:10  
 Class :character   Class :character   Class :character   Finland.Females:10  
 Mode  :character   Mode  :character   Mode  :character   Norway.Females :10  
                                                          Sweden.Females :10  
                                                          Denmark.Males  :10  
                                                          Finland.Males  :10  
                                                          (Other)        :20  
   years10cen        se_1y            se_5y          surv_cond    
 Min.   :-2.25   Min.   :0.5612   Min.   :0.9694   Min.   :50.90  
 1st Qu.:-1.25   1st Qu.:1.0077   1st Qu.:1.3138   1st Qu.:66.07  
 Median : 0.00   Median :1.1990   Median :1.5306   Median :73.06  
 Mean   : 0.00   Mean   :1.2959   Mean   :1.5593   Mean   :73.07  
 3rd Qu.: 1.25   3rd Qu.:1.5816   3rd Qu.:1.7347   3rd Qu.:81.28  
 Max.   : 2.25   Max.   :2.4490   Max.   :2.5000   Max.   :89.15  
                                                                  
```
:::
:::


### Bladder data wrangling


::: {.cell}

```{.r .cell-code}
x<-2;bladder_1y_est<-data.frame(bladder_1y[,1])
for(x in 2:9){bladder_1y_est[,x] <- str_sub(bladder_1y[,x], 1, 4) }
for(x in 10:17){bladder_1y_est[,x] <- str_sub(bladder_1y[,x-8], 6, 9) }
bladder_5y_est<-data.frame(bladder_5y[,1])
for(x in 2:9){bladder_5y_est[,x] <- str_sub(bladder_5y[,x], 1, 4) }
for(x in 10:17){bladder_5y_est[,x] <- str_sub(bladder_5y[,x-8], 6, 9) }

bladder<-(data.frame(unlist(bladder_1y_est[,2:9])));colnames(bladder)<-"surv_1y"
bladder$cil_1y<-as.numeric(unlist(bladder_1y_est[,10:17]))
bladder$surv_5y<-as.numeric(unlist(bladder_5y_est[,2:9]))
bladder$cil_5y<-as.numeric(unlist(bladder_5y_est[,10:17]))
bladder$year<-rep(seq(1973,2018,by=5),8)
bladder$sex<-c(rep("Males",40),rep("Females",40))
bladder$country<-c(rep(c(rep("Denmark",10),rep("Finland",10),rep("Norway",10),
                         rep("Sweden",10)),2))
bladder$shou<-c(rep(c(rep("den_mal_",10),rep("fin_mal_",10),rep("nor_mal_",10),
                      rep("swe_mal_",10),rep("den_fem_",10),rep("fin_fem_",10),
                      rep("nor_fem_",10),rep("swe_fem_",10)),1))
bladder$group<-interaction(bladder$country,bladder$sex)
bladder$years10cen<-(bladder$year-1995.5)/10
bladder$surv_1y<-as.numeric(bladder$surv_1y)
bladder$surv_5y<-as.numeric(bladder$surv_5y)
bladder$cil_1y<-as.numeric(bladder$cil_1y)
bladder$cil_5y<-as.numeric(bladder$cil_5y)
bladder$se_1y<-(bladder$surv_1y-bladder$cil_1y)/1.96
bladder$se_5y<-(bladder$surv_5y-bladder$cil_5y)/1.96
bladder$surv_cond<-(bladder$surv_5y/bladder$surv_1y)*100
summary(bladder)
```

::: {.cell-output .cell-output-stdout}
```
    surv_1y          cil_1y         surv_5y          cil_5y           year     
 Min.   :71.30   Min.   :68.70   Min.   :48.60   Min.   :43.60   Min.   :1973  
 1st Qu.:80.45   1st Qu.:78.53   1st Qu.:63.92   1st Qu.:61.02   1st Qu.:1983  
 Median :84.75   Median :83.10   Median :69.90   Median :67.80   Median :1996  
 Mean   :83.75   Mean   :82.14   Mean   :68.45   Mean   :66.11   Mean   :1996  
 3rd Qu.:87.90   3rd Qu.:86.65   3rd Qu.:74.58   3rd Qu.:72.33   3rd Qu.:2008  
 Max.   :92.40   Max.   :91.80   Max.   :81.60   Max.   :80.60   Max.   :2018  
                                                                               
     sex              country              shou                       group   
 Length:80          Length:80          Length:80          Denmark.Females:10  
 Class :character   Class :character   Class :character   Finland.Females:10  
 Mode  :character   Mode  :character   Mode  :character   Norway.Females :10  
                                                          Sweden.Females :10  
                                                          Denmark.Males  :10  
                                                          Finland.Males  :10  
                                                          (Other)        :20  
   years10cen        se_1y            se_5y          surv_cond    
 Min.   :-2.25   Min.   :0.3061   Min.   :0.5102   Min.   :64.86  
 1st Qu.:-1.25   1st Qu.:0.5612   1st Qu.:0.8673   1st Qu.:78.75  
 Median : 0.00   Median :0.7653   Median :1.1224   Median :82.82  
 Mean   : 0.00   Mean   :0.8202   Mean   :1.1926   Mean   :81.48  
 3rd Qu.: 1.25   3rd Qu.:1.0204   3rd Qu.:1.4286   3rd Qu.:85.23  
 Max.   : 2.25   Max.   :2.0408   Max.   :2.5510   Max.   :88.91  
                                                                  
```
:::
:::


## Modelling of survival trends

### Prior probabilities specification


::: {.cell}

```{.r .cell-code}
prior_group <- c(
set_prior("normal(0,30)", class = "b", coef = "groupFinland.Males"),
set_prior("normal(0,30)", class = "b", coef = "groupNorway.Males"),
set_prior("normal(0,30)", class = "b", coef = "groupSweden.Males"),
set_prior("normal(0,30)", class = "b", coef = "groupDenmark.Males"),
set_prior("normal(0,30)", class = "b", coef = "groupNorway.Females"),
set_prior("normal(0,30)", class = "b", coef = "groupFinland.Females"),
set_prior("normal(0,30)", class = "b", coef = "groupSweden.Females"))
```
:::


### Fitting models of kidney survival


::: {.cell}

```{.r .cell-code}
set.seed(17)
kidney_1y_model <- brm(surv_1y|se(se_1y) ~ group + s(years10cen, by=group, k=5),
                       family="Gaussian", 
                       prior = prior_group, 
                       data = kidney, seed = 17,
                       iter = 7000, warmup = 2000, chains = 2, cores = 1,
                       control = list(adapt_delta = 0.98),
                       save_pars = save_pars(all = TRUE))

kidney_5y_model <- brm(surv_5y|se(se_5y) ~ group + s(years10cen, by=group, k=5),
                       family="Gaussian", 
                       prior = prior_group, 
                       data = kidney, seed = 17,
                       iter = 7000, warmup = 2000, chains = 2, cores = 1,
                       control = list(adapt_delta = 0.98),
                       save_pars = save_pars(all = TRUE))
```
:::


### Fitting models of bladder cancer


::: {.cell}

```{.r .cell-code}
set.seed(17)
bladder_1y_model <- brm(surv_1y|se(se_1y) ~ group + s(years10cen, by=group,k=5),
                       family="Gaussian", 
                       prior = prior_group, 
                       data = bladder, seed = 17,
                       iter = 7000, warmup = 2000, chains = 2, cores = 1,
                       control = list(adapt_delta = 0.98),
                       save_pars = save_pars(all = TRUE))

bladder_5y_model <- brm(surv_5y|se(se_5y) ~ group + s(years10cen, by=group,k=5),
                       family="Gaussian", 
                       prior = prior_group, 
                       data = bladder, seed = 17,
                       iter = 7000, warmup = 2000, chains = 2, cores = 1,
                       control = list(adapt_delta = 0.98),
                       save_pars = save_pars(all = TRUE))
```
:::


### Diagnostics

Checking effective sample size of posterior samples, convergence of the models and posterior predictive check (PPC)

In general, diagnostics uses:

\(i\) *summary(model)*: the columns of intereste are ***Rhat*** (when = 1, model converged well), and columns ***Tail_ESS*** and ***Bulk_ESS*** which should never go under 1,000 and should be mostly above 2,000.

\[2\] ***pp_check*** : graphical tools to explore how well the model predicts real data, as thoroughly described by Gelman [here](http://www.stat.columbia.edu/~gelman/book/). Ideally, there should not be strong discrepancy between the model prediction and the distribution of the real dataset.

You can use also *plot(model)* to see convergence of chains graphically and to explore posterior distribution of parameters, estimated via the models

Finally, we will also use a function *prior_summary* to explore the prior probabilities set

#### Kidney 1y model


::: {.cell}

```{.r .cell-code}
summary(kidney_1y_model)
```

::: {.cell-output .cell-output-stdout}
```
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: surv_1y | se(se_1y) ~ group + s(years10cen, by = group, k = 5) 
   Data: kidney (Number of observations: 80) 
  Draws: 2 chains, each with iter = 7000; warmup = 2000; thin = 1;
         total post-warmup draws = 10000

Smooth Terms: 
                                       Estimate Est.Error l-95% CI u-95% CI
sds(syears10cengroupDenmark.Females_1)    23.71     10.95    10.23    50.26
sds(syears10cengroupFinland.Females_1)    10.96      6.53     2.99    28.12
sds(syears10cengroupNorway.Females_1)     11.11      7.32     1.58    29.40
sds(syears10cengroupSweden.Females_1)      8.03      7.95     0.20    28.21
sds(syears10cengroupDenmark.Males_1)      18.94      8.99     8.14    41.57
sds(syears10cengroupFinland.Males_1)      32.99     15.28    12.36    69.80
sds(syears10cengroupNorway.Males_1)       12.19      7.25     3.42    30.45
sds(syears10cengroupSweden.Males_1)       25.99     13.20     9.12    56.59
                                       Rhat Bulk_ESS Tail_ESS
sds(syears10cengroupDenmark.Females_1) 1.00     6190     7588
sds(syears10cengroupFinland.Females_1) 1.00     6205     5677
sds(syears10cengroupNorway.Females_1)  1.00     4691     4504
sds(syears10cengroupSweden.Females_1)  1.00     2479     4234
sds(syears10cengroupDenmark.Males_1)   1.00     6506     6806
sds(syears10cengroupFinland.Males_1)   1.00     4000     4125
sds(syears10cengroupNorway.Males_1)    1.00     5482     5823
sds(syears10cengroupSweden.Males_1)    1.00     3653     3810

Population-Level Effects: 
                                   Estimate Est.Error l-95% CI u-95% CI Rhat
Intercept                             62.92      0.47    62.00    63.84 1.00
groupFinland.Females                   9.77      0.65     8.49    11.04 1.00
groupNorway.Females                   10.02      0.70     8.65    11.39 1.00
groupSweden.Females                   11.01      0.57     9.91    12.13 1.00
groupDenmark.Males                     0.94      0.63    -0.30     2.19 1.00
groupFinland.Males                     6.66      0.66     5.37     7.95 1.00
groupNorway.Males                      6.82      0.64     5.56     8.10 1.00
groupSweden.Males                      9.52      0.55     8.44    10.58 1.00
syears10cen:groupDenmark.Females_1    86.35     12.51    61.59   111.15 1.00
syears10cen:groupFinland.Females_1    56.82      9.98    37.17    76.46 1.00
syears10cen:groupNorway.Females_1     61.14     11.18    37.13    80.48 1.00
syears10cen:groupSweden.Females_1     71.55      6.53    57.02    84.19 1.00
syears10cen:groupDenmark.Males_1      87.13     10.87    65.93   108.82 1.00
syears10cen:groupFinland.Males_1      67.92     13.95    40.19    94.03 1.00
syears10cen:groupNorway.Males_1       69.79     10.29    48.28    88.59 1.00
syears10cen:groupSweden.Males_1       97.81      8.46    81.19   114.58 1.00
                                   Bulk_ESS Tail_ESS
Intercept                              3660     5742
groupFinland.Females                   5196     7053
groupNorway.Females                    5418     6950
groupSweden.Females                    4435     6510
groupDenmark.Males                     5045     6959
groupFinland.Males                     5210     7098
groupNorway.Males                      4906     7071
groupSweden.Males                      4319     6412
syears10cen:groupDenmark.Females_1     8157     7893
syears10cen:groupFinland.Females_1     7987     7156
syears10cen:groupNorway.Females_1      6651     7230
syears10cen:groupSweden.Females_1      6624     6403
syears10cen:groupDenmark.Males_1       9074     8022
syears10cen:groupFinland.Males_1       8211     7452
syears10cen:groupNorway.Males_1        8188     8038
syears10cen:groupSweden.Males_1        7497     4940

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma     0.00      0.00     0.00     0.00   NA       NA       NA

Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
```
:::

```{.r .cell-code}
pp_check(kidney_1y_model, type='dens_overlay', ndraws = 100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-11-1.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_1y_model, type='scatter_avg') 
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-11-2.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_1y_model, type = "stat", stat = "mean")
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-11-3.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_1y_model, type="stat_2d", stat = c("max", "min"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-11-4.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_1y_model, type="stat_2d", stat = c("mean", "sd"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-11-5.png){width=672}
:::

```{.r .cell-code}
prior_summary(kidney_1y_model)
```

::: {.cell-output .cell-output-stdout}
```
                    prior     class                               coef group
                   (flat)         b                                         
             normal(0,30)         b                 groupDenmark.Males      
             normal(0,30)         b               groupFinland.Females      
             normal(0,30)         b                 groupFinland.Males      
             normal(0,30)         b                groupNorway.Females      
             normal(0,30)         b                  groupNorway.Males      
             normal(0,30)         b                groupSweden.Females      
             normal(0,30)         b                  groupSweden.Males      
                   (flat)         b syears10cen:groupDenmark.Females_1      
                   (flat)         b   syears10cen:groupDenmark.Males_1      
                   (flat)         b syears10cen:groupFinland.Females_1      
                   (flat)         b   syears10cen:groupFinland.Males_1      
                   (flat)         b  syears10cen:groupNorway.Females_1      
                   (flat)         b    syears10cen:groupNorway.Males_1      
                   (flat)         b  syears10cen:groupSweden.Females_1      
                   (flat)         b    syears10cen:groupSweden.Males_1      
 student_t(3, 70.5, 16.6) Intercept                                         
    student_t(3, 0, 16.6)       sds                                         
    student_t(3, 0, 16.6)       sds   s(years10cen, by = group, k = 5)      
 resp dpar nlpar lb ub       source
                            default
                               user
                               user
                               user
                               user
                               user
                               user
                               user
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                            default
                  0         default
                  0    (vectorized)
```
:::
:::


\>\> Everything seems to be fine: For all parameters, *Rhat* = 1, *ESS* is above 2000 and PPC show relatively good correspondence between the predicted and the real data.

#### Kidney 5y model


::: {.cell}

```{.r .cell-code}
summary(kidney_5y_model)
```

::: {.cell-output .cell-output-stdout}
```
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: surv_5y | se(se_5y) ~ group + s(years10cen, by = group, k = 5) 
   Data: kidney (Number of observations: 80) 
  Draws: 2 chains, each with iter = 7000; warmup = 2000; thin = 1;
         total post-warmup draws = 10000

Smooth Terms: 
                                       Estimate Est.Error l-95% CI u-95% CI
sds(syears10cengroupDenmark.Females_1)    29.24     13.13    13.09    61.98
sds(syears10cengroupFinland.Females_1)    17.20     10.08     5.17    42.63
sds(syears10cengroupNorway.Females_1)     14.35      9.83     2.22    39.38
sds(syears10cengroupSweden.Females_1)     14.81      8.61     4.15    37.32
sds(syears10cengroupDenmark.Males_1)      26.80     11.94    12.02    56.52
sds(syears10cengroupFinland.Males_1)      38.04     18.09    12.94    81.89
sds(syears10cengroupNorway.Males_1)       19.74     10.11     7.53    46.15
sds(syears10cengroupSweden.Males_1)       21.30     12.10     5.73    51.29
                                       Rhat Bulk_ESS Tail_ESS
sds(syears10cengroupDenmark.Females_1) 1.00     6047     7160
sds(syears10cengroupFinland.Females_1) 1.00     5862     5866
sds(syears10cengroupNorway.Females_1)  1.00     4103     3002
sds(syears10cengroupSweden.Females_1)  1.00     6327     6650
sds(syears10cengroupDenmark.Males_1)   1.00     6253     6465
sds(syears10cengroupFinland.Males_1)   1.00     4669     4404
sds(syears10cengroupNorway.Males_1)    1.00     6874     6804
sds(syears10cengroupSweden.Males_1)    1.00     4437     4944

Population-Level Effects: 
                                   Estimate Est.Error l-95% CI u-95% CI Rhat
Intercept                             45.41      0.52    44.37    46.42 1.00
groupFinland.Females                  10.41      0.73     8.98    11.87 1.00
groupNorway.Females                   11.40      0.79     9.86    12.95 1.00
groupSweden.Females                   11.40      0.66    10.08    12.71 1.00
groupDenmark.Males                    -0.63      0.72    -2.00     0.78 1.00
groupFinland.Males                     5.02      0.75     3.57     6.48 1.00
groupNorway.Males                      6.91      0.74     5.46     8.37 1.00
groupSweden.Males                      8.54      0.64     7.27     9.79 1.00
syears10cen:groupDenmark.Females_1   109.42     14.47    81.65   138.68 1.00
syears10cen:groupFinland.Females_1    86.53     13.01    60.98   112.31 1.00
syears10cen:groupNorway.Females_1     86.25     13.28    58.12   111.00 1.00
syears10cen:groupSweden.Females_1     71.87     11.09    49.05    91.77 1.00
syears10cen:groupDenmark.Males_1     107.44     13.24    81.56   133.64 1.00
syears10cen:groupFinland.Males_1      88.62     15.57    57.62   118.52 1.00
syears10cen:groupNorway.Males_1       93.55     13.06    67.99   119.57 1.00
syears10cen:groupSweden.Males_1       99.14      9.86    79.47   118.92 1.00
                                   Bulk_ESS Tail_ESS
Intercept                              4462     6267
groupFinland.Females                   6215     7474
groupNorway.Females                    6609     7219
groupSweden.Females                    5626     7527
groupDenmark.Males                     5986     7647
groupFinland.Males                     6387     7591
groupNorway.Males                      6317     7215
groupSweden.Males                      5241     7014
syears10cen:groupDenmark.Females_1     8827     7507
syears10cen:groupFinland.Females_1     9008     7055
syears10cen:groupNorway.Females_1      8225     6538
syears10cen:groupSweden.Females_1      8626     7279
syears10cen:groupDenmark.Males_1      10949     8077
syears10cen:groupFinland.Males_1       8238     7788
syears10cen:groupNorway.Males_1        9912     8219
syears10cen:groupSweden.Males_1       10233     7809

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma     0.00      0.00     0.00     0.00   NA       NA       NA

Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
```
:::

```{.r .cell-code}
pp_check(kidney_5y_model, type='dens_overlay', ndraws = 100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_5y_model, type='scatter_avg') 
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-12-2.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_5y_model, type = "stat", stat = "mean")
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-12-3.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_5y_model, type="stat_2d", stat = c("max", "min"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-12-4.png){width=672}
:::

```{.r .cell-code}
pp_check(kidney_5y_model, type="stat_2d", stat = c("mean", "sd"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-12-5.png){width=672}
:::

```{.r .cell-code}
prior_summary(kidney_5y_model)
```

::: {.cell-output .cell-output-stdout}
```
                    prior     class                               coef group
                   (flat)         b                                         
             normal(0,30)         b                 groupDenmark.Males      
             normal(0,30)         b               groupFinland.Females      
             normal(0,30)         b                 groupFinland.Males      
             normal(0,30)         b                groupNorway.Females      
             normal(0,30)         b                  groupNorway.Males      
             normal(0,30)         b                groupSweden.Females      
             normal(0,30)         b                  groupSweden.Males      
                   (flat)         b syears10cen:groupDenmark.Females_1      
                   (flat)         b   syears10cen:groupDenmark.Males_1      
                   (flat)         b syears10cen:groupFinland.Females_1      
                   (flat)         b   syears10cen:groupFinland.Males_1      
                   (flat)         b  syears10cen:groupNorway.Females_1      
                   (flat)         b    syears10cen:groupNorway.Males_1      
                   (flat)         b  syears10cen:groupSweden.Females_1      
                   (flat)         b    syears10cen:groupSweden.Males_1      
 student_t(3, 50.8, 18.8) Intercept                                         
    student_t(3, 0, 18.8)       sds                                         
    student_t(3, 0, 18.8)       sds   s(years10cen, by = group, k = 5)      
 resp dpar nlpar lb ub       source
                            default
                               user
                               user
                               user
                               user
                               user
                               user
                               user
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                            default
                  0         default
                  0    (vectorized)
```
:::
:::


\>\> Everything seems fine.

#### Bladder 1y model


::: {.cell}

```{.r .cell-code}
summary(bladder_1y_model)
```

::: {.cell-output .cell-output-stdout}
```
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: surv_1y | se(se_1y) ~ group + s(years10cen, by = group, k = 5) 
   Data: bladder (Number of observations: 80) 
  Draws: 2 chains, each with iter = 7000; warmup = 2000; thin = 1;
         total post-warmup draws = 10000

Smooth Terms: 
                                       Estimate Est.Error l-95% CI u-95% CI
sds(syears10cengroupDenmark.Females_1)     4.36      3.40     0.22    13.02
sds(syears10cengroupFinland.Females_1)     7.01      3.90     1.63    16.68
sds(syears10cengroupNorway.Females_1)      6.29      5.02     0.27    18.71
sds(syears10cengroupSweden.Females_1)      5.37      3.50     0.65    14.03
sds(syears10cengroupDenmark.Males_1)       4.95      3.01     0.98    12.48
sds(syears10cengroupFinland.Males_1)       8.63      4.13     3.55    19.23
sds(syears10cengroupNorway.Males_1)        8.47      4.13     3.17    18.78
sds(syears10cengroupSweden.Males_1)       12.22      4.73     6.23    24.01
                                       Rhat Bulk_ESS Tail_ESS
sds(syears10cengroupDenmark.Females_1) 1.00     4353     4032
sds(syears10cengroupFinland.Females_1) 1.00     6208     4173
sds(syears10cengroupNorway.Females_1)  1.00     3296     4742
sds(syears10cengroupSweden.Females_1)  1.00     4158     3418
sds(syears10cengroupDenmark.Males_1)   1.00     4452     4172
sds(syears10cengroupFinland.Males_1)   1.00     6703     7236
sds(syears10cengroupNorway.Males_1)    1.00     5812     6295
sds(syears10cengroupSweden.Males_1)    1.00     5614     6506

Population-Level Effects: 
                                   Estimate Est.Error l-95% CI u-95% CI Rhat
Intercept                             78.07      0.30    77.48    78.66 1.00
groupFinland.Females                   4.37      0.52     3.33     5.40 1.00
groupNorway.Females                    2.62      0.47     1.70     3.54 1.00
groupSweden.Females                    4.95      0.39     4.19     5.72 1.00
groupDenmark.Males                     6.87      0.35     6.19     7.54 1.00
groupFinland.Males                     8.68      0.39     7.92     9.43 1.00
groupNorway.Males                      9.21      0.36     8.49     9.91 1.00
groupSweden.Males                      9.19      0.33     8.53     9.85 1.00
syears10cen:groupDenmark.Females_1    35.57      6.23    25.92    50.34 1.00
syears10cen:groupFinland.Females_1    20.36      8.39     4.24    37.93 1.00
syears10cen:groupNorway.Females_1     37.37     10.92    22.44    61.39 1.00
syears10cen:groupSweden.Females_1     22.64      6.26    12.75    36.51 1.00
syears10cen:groupDenmark.Males_1      33.67      4.91    25.30    43.89 1.00
syears10cen:groupFinland.Males_1      32.63      7.16    19.55    47.41 1.00
syears10cen:groupNorway.Males_1       35.84      6.41    23.59    48.63 1.00
syears10cen:groupSweden.Males_1       50.86      4.55    41.92    59.74 1.00
                                   Bulk_ESS Tail_ESS
Intercept                              3826     5122
groupFinland.Females                   6754     7136
groupNorway.Females                    6540     7097
groupSweden.Females                    5044     6400
groupDenmark.Males                     4197     5841
groupFinland.Males                     4908     6831
groupNorway.Males                      4676     6243
groupSweden.Males                      4089     5777
syears10cen:groupDenmark.Females_1     6758     7220
syears10cen:groupFinland.Females_1     8751     6784
syears10cen:groupNorway.Females_1      3752     6918
syears10cen:groupSweden.Females_1      5904     7196
syears10cen:groupDenmark.Males_1       5843     6771
syears10cen:groupFinland.Males_1       8587     7083
syears10cen:groupNorway.Males_1        7837     6145
syears10cen:groupSweden.Males_1       11911     7381

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma     0.00      0.00     0.00     0.00   NA       NA       NA

Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
```
:::

```{.r .cell-code}
pp_check(bladder_1y_model, type='dens_overlay', ndraws = 100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-13-1.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_1y_model, type='scatter_avg') 
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-13-2.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_1y_model, type = "stat", stat = "mean")
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-13-3.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_1y_model, type="stat_2d", stat = c("max", "min"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-13-4.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_1y_model, type="stat_2d", stat = c("mean", "sd"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-13-5.png){width=672}
:::

```{.r .cell-code}
prior_summary(bladder_1y_model)
```

::: {.cell-output .cell-output-stdout}
```
                   prior     class                               coef group
                  (flat)         b                                         
            normal(0,30)         b                 groupDenmark.Males      
            normal(0,30)         b               groupFinland.Females      
            normal(0,30)         b                 groupFinland.Males      
            normal(0,30)         b                groupNorway.Females      
            normal(0,30)         b                  groupNorway.Males      
            normal(0,30)         b                groupSweden.Females      
            normal(0,30)         b                  groupSweden.Males      
                  (flat)         b syears10cen:groupDenmark.Females_1      
                  (flat)         b   syears10cen:groupDenmark.Males_1      
                  (flat)         b syears10cen:groupFinland.Females_1      
                  (flat)         b   syears10cen:groupFinland.Males_1      
                  (flat)         b  syears10cen:groupNorway.Females_1      
                  (flat)         b    syears10cen:groupNorway.Males_1      
                  (flat)         b  syears10cen:groupSweden.Females_1      
                  (flat)         b    syears10cen:groupSweden.Males_1      
 student_t(3, 84.8, 5.4) Intercept                                         
    student_t(3, 0, 5.4)       sds                                         
    student_t(3, 0, 5.4)       sds   s(years10cen, by = group, k = 5)      
 resp dpar nlpar lb ub       source
                            default
                               user
                               user
                               user
                               user
                               user
                               user
                               user
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                            default
                  0         default
                  0    (vectorized)
```
:::
:::


\>\> Everything seems fine.

#### Bladder 5y model


::: {.cell}

```{.r .cell-code}
summary(bladder_5y_model)
```

::: {.cell-output .cell-output-stdout}
```
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: surv_5y | se(se_5y) ~ group + s(years10cen, by = group, k = 5) 
   Data: bladder (Number of observations: 80) 
  Draws: 2 chains, each with iter = 7000; warmup = 2000; thin = 1;
         total post-warmup draws = 10000

Smooth Terms: 
                                       Estimate Est.Error l-95% CI u-95% CI
sds(syears10cengroupDenmark.Females_1)     5.92      5.05     0.17    18.46
sds(syears10cengroupFinland.Females_1)    12.85      6.35     4.77    28.96
sds(syears10cengroupNorway.Females_1)      8.69      6.30     0.54    23.68
sds(syears10cengroupSweden.Females_1)      4.81      4.56     0.14    16.25
sds(syears10cengroupDenmark.Males_1)       4.39      3.60     0.24    13.22
sds(syears10cengroupFinland.Males_1)      19.15      7.84     9.31    38.26
sds(syears10cengroupNorway.Males_1)        9.06      5.48     1.67    22.46
sds(syears10cengroupSweden.Males_1)       13.11      5.66     5.64    27.45
                                       Rhat Bulk_ESS Tail_ESS
sds(syears10cengroupDenmark.Females_1) 1.00     3319     4080
sds(syears10cengroupFinland.Females_1) 1.00     6654     6815
sds(syears10cengroupNorway.Females_1)  1.00     3698     3708
sds(syears10cengroupSweden.Females_1)  1.00     3827     5218
sds(syears10cengroupDenmark.Males_1)   1.00     4132     4563
sds(syears10cengroupFinland.Males_1)   1.00     5173     6298
sds(syears10cengroupNorway.Males_1)    1.00     4031     3777
sds(syears10cengroupSweden.Males_1)    1.00     5189     6103

Population-Level Effects: 
                                   Estimate Est.Error l-95% CI u-95% CI Rhat
Intercept                             62.00      0.40    61.21    62.77 1.00
groupFinland.Females                   4.96      0.70     3.59     6.34 1.00
groupNorway.Females                    3.31      0.64     2.08     4.57 1.00
groupSweden.Females                    6.88      0.53     5.86     7.92 1.00
groupDenmark.Males                     7.07      0.48     6.14     8.03 1.00
groupFinland.Males                     8.25      0.56     7.13     9.34 1.00
groupNorway.Males                     10.00      0.52     8.99    11.02 1.00
groupSweden.Males                     11.42      0.46    10.53    12.34 1.00
syears10cen:groupDenmark.Females_1    54.49      9.72    41.12    77.42 1.00
syears10cen:groupFinland.Females_1    47.85     12.98    23.60    75.74 1.00
syears10cen:groupNorway.Females_1     53.94     12.94    34.92    83.27 1.00
syears10cen:groupSweden.Females_1     30.08      7.21    19.94    47.93 1.00
syears10cen:groupDenmark.Males_1      53.04      5.41    44.19    65.77 1.00
syears10cen:groupFinland.Males_1      82.04     11.65    59.66   105.50 1.00
syears10cen:groupNorway.Males_1       53.46      9.67    37.01    74.16 1.00
syears10cen:groupSweden.Males_1       59.62      7.35    44.92    73.86 1.00
                                   Bulk_ESS Tail_ESS
Intercept                              3949     5809
groupFinland.Females                   6707     7196
groupNorway.Females                    6546     7012
groupSweden.Females                    5502     7160
groupDenmark.Males                     4737     6794
groupFinland.Males                     5894     7108
groupNorway.Males                      5272     6916
groupSweden.Males                      4582     6336
syears10cen:groupDenmark.Females_1     4199     6555
syears10cen:groupFinland.Females_1     6609     6339
syears10cen:groupNorway.Females_1      4228     7133
syears10cen:groupSweden.Females_1      4257     6052
syears10cen:groupDenmark.Males_1       5506     5981
syears10cen:groupFinland.Males_1       8109     6790
syears10cen:groupNorway.Males_1        4466     5798
syears10cen:groupSweden.Males_1        9431     7209

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma     0.00      0.00     0.00     0.00   NA       NA       NA

Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
```
:::

```{.r .cell-code}
pp_check(bladder_5y_model, type='dens_overlay', ndraws = 100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-14-1.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_5y_model, type='scatter_avg') 
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-14-2.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_5y_model, type = "stat", stat = "mean")
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-14-3.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_5y_model, type="stat_2d", stat = c("max", "min"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-14-4.png){width=672}
:::

```{.r .cell-code}
pp_check(bladder_5y_model, type="stat_2d", stat = c("mean", "sd"), ndraws=100)
```

::: {.cell-output-display}
![](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-14-5.png){width=672}
:::

```{.r .cell-code}
prior_summary(bladder_5y_model)
```

::: {.cell-output .cell-output-stdout}
```
                   prior     class                               coef group
                  (flat)         b                                         
            normal(0,30)         b                 groupDenmark.Males      
            normal(0,30)         b               groupFinland.Females      
            normal(0,30)         b                 groupFinland.Males      
            normal(0,30)         b                groupNorway.Females      
            normal(0,30)         b                  groupNorway.Males      
            normal(0,30)         b                groupSweden.Females      
            normal(0,30)         b                  groupSweden.Males      
                  (flat)         b syears10cen:groupDenmark.Females_1      
                  (flat)         b   syears10cen:groupDenmark.Males_1      
                  (flat)         b syears10cen:groupFinland.Females_1      
                  (flat)         b   syears10cen:groupFinland.Males_1      
                  (flat)         b  syears10cen:groupNorway.Females_1      
                  (flat)         b    syears10cen:groupNorway.Males_1      
                  (flat)         b  syears10cen:groupSweden.Females_1      
                  (flat)         b    syears10cen:groupSweden.Males_1      
 student_t(3, 69.9, 8.1) Intercept                                         
    student_t(3, 0, 8.1)       sds                                         
    student_t(3, 0, 8.1)       sds   s(years10cen, by = group, k = 5)      
 resp dpar nlpar lb ub       source
                            default
                               user
                               user
                               user
                               user
                               user
                               user
                               user
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                       (vectorized)
                            default
                  0         default
                  0    (vectorized)
```
:::
:::


\>\> Everything seems fine.

## Custom functions

All the functions below serve for plotting of nonlinear trends in survival and related uncertainty. All the functions below use extracted posterior samples as an input **data**. The samples represent estimation of survival over the 50 years (1971 to 2020) for specific country, cancer, and type of survival (1-year, 5-years, conditional \[5y/1y\]. The time period of 50 years must be divided to sequence of 500 numbers (1/10 of a year).

### *breakpo*

This function serves to identify ***breaking points***, i.e. times when the annual change of survival changed with at least 95% plausibility. This was assessed by calculation of the 2nd derivation of the given survival measure and its 95% credible interval (CI); the '*breaking point*' was defined as a peak value within at least a 3-year interval where 95% CI for the 2nd derivation did not cross zero. If the 2nd derivation is plausibly non-zero for at least 3 years, the function takes the peak in the 2nd derivation (within the identified time interval) as the *breaking point* (must be between the years 1976 and 2016).

Function also returns table of potential *brekaing points*, if they were identified.

There is one additional argument *arb* which should be zero except for the situation when the multiple breaking points overlap. The argument only move the breaking points by given value to avoid overlapping.


::: {.cell}

```{.r .cell-code}
breakpo<-function(data,arb){ 
 data<-data.frame((data[,-1] - data[,-ncol(data)])*10)
  data<-data.frame(data[,-1] - data[,-ncol(data)])
  data=sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cbinl<-c()
  x=1
  repeat{
    cbinl[x]<-
      if(data[1,x]>0|data[2,x]<0){cbinl[x]=1}else{cbinl[x]=0}
    x<-x+1
    if(x>length(data[1,])){break}}
  cbin=1;x<-1
  repeat{
    cbin[x+1]<-abs(cbinl[x]-cbinl[x+1])
    x=x+1
    if(x>(length(cbinl)-1)){break}}
  data<-data.frame(rbind(data,cbin));data[5,]<-yreal[c(2:499)]-0.049
  data[6,]<-1:498;data[4,51]<-1;data[4,449]<-1
  row.names(data)<-c("cil","ciu","est","stat_change","year","timepoint")
  data2=t(data[,data[4,]==1])
  data2<-data.frame(data2)
  tr<-subset(data2,data2$timepoint>50&data2$timepoint<450)
  y=1
  bp<-c()
  bx<-1
  repeat{
    if( (tr[y,1]<0) & (tr[y,2]<0) & (  (tr$year[y+1]-tr$year[y])>3   )  ) {
      tr2<-data[,tr[y,6]:(tr[y+1,6]-2)]
      bp[bx]<- tr2[,order(tr2[3,],decreasing=F)[1]][5]
      bx=bx+1
    }
    if( (tr[y,1]>0) & (tr[y,2]>0) & (  (tr$year[y+1]-tr$year[y])>3   )) {
      tr2<-data[,tr[y,6]:(tr[y+1,6]-2)]
      bp[bx]<- tr2[,order(tr2[3,],decreasing=T)[1]][5]
      bx=bx+1
    }
    y=y+1;if(y>(dim(tr)[1]-1)){break}}
  y<-1
  try(repeat{
    lines(c(bp[y]+arb,bp[y]+arb),c(range[1],range[1]+0.025*scal),col=cola[xx],lwd=3.5,lend=1)
    lines(c(bp[y]+arb,bp[y]+arb),c(range[2],range[2]-0.025*scal),col=cola[xx],lwd=3.5,lend=1)
    lines(c(bp[y]+arb,bp[y]+arb),c(range[1],range[1]+0.999*scal),col=colc[xx],lwd=1,lend=1,lty=2)
    y=y+1;if(y>length(bp)){break}},silent=TRUE)
 print(bp)
 print(tr)
  }
```
:::


### *polyg_surv*

for drawing 95% credible interval for survival indicators


::: {.cell}

```{.r .cell-code}
polyg_surv<-function(data){ 
  data<-data.frame(data)
  data<-sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cis<-c(data[1,],rev(data[2,]))
  x<-c(yreal[1:500],yreal[500:1])
  cis[cis<range[1]]<-range[1]
  cis[cis>range[2]]<-range[2]
  polygon(x,cis,border=NA,col=colb[xx],xpd=F)}
```
:::


### *surv_fit*

Fit curve of survival trend over the 50 years. **Solid lines** imply that that the curve is increasing or decreasing with at least 95% plausibility (95% credible interval for 1st derivation of estimated survival does not cross zero) for at least 5 years. Dashed lines show otherwise (no detectable change in the survival)


::: {.cell}

```{.r .cell-code}
surv_fit<-function(dat2){
  dat2=data.frame(dat2)
  cis= data.frame((dat2[,-1] - dat2[,-ncol(dat2)])*10)
  cis=sapply(cis, function(p) quantile(p, probs = c(0.025,0.975)))
  est=sapply(dat2, function(p) quantile(p, probs = c(0.5)))
  data=cis
  cbinl<-c()
  x=1
  repeat{
    cbinl[x]<-
      if(data[1,x]>0|data[2,x]<0){cbinl[x]=1}else{cbinl[x]=0}
    x<-x+1
    if(x>length(data[1,])){break}}
  cbin=1;x<-1
  repeat{
    cbin[x+1]<-abs(cbinl[x]-cbinl[x+1])
    x=x+1
    if(x>(length(cbinl)-1)){break}}
  data<-data.frame(rbind(data,cbin));data[4,]<-yreal[c(2:500)]-0.049;data[5,]<-1:499
  row.names(data)<-c("cil","ciu","stat_change","year","timepoint");data[3,499]<-1
  data2=t(data[,data[3,]==1])
  data2<-data.frame(data2)
  tr<-data2;y=1
  yreal=seq(1971,2020,length=500)
  repeat{
    if((((tr[y,1]<0)&(tr[y,2])<0))|(((tr[y,1]>0)&(tr[y,2])>0))&(tr[y+1,4]-tr[y,4])>5)
    {lines(yreal[tr[y,5]:tr[y+1,5]],
           est[tr[y,5]:tr[y+1,5]],lwd=1.9,col=cola[xx],lend=1)}
    y<-y+1;if(y>dim(tr)[1]-1){break}}
  lines(yreal[1:500],est,lwd=1,col=cola[xx],lty=2,lend=1)}
```
:::


### *polyg_slope*

Draws 95% credible interval for slope of the survival trend (1st derivation of the estimated survival trend)


::: {.cell}

```{.r .cell-code}
polyg_slope<-function(data){ 
  x<-c(yreal[1:499],yreal[499:1])
  data<-data.frame((data[,-1] - data[,-ncol(data)]))*10
  data=sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cis<-c(data[1,],rev(data[2,]))
  cis[cis<range[1]]<-range[1]
  cis[cis>range[2]]<-range[2]
  polygon(x,cis,border=NA,col=colb[xx])}
```
:::


### *slope_fit*

Fit curve of **slope** (i.e., the **magnitude of the change**) of the survival trend over the 50 years. Solid lines imply that that the curve is increasing or decreasing with at least 95% plausibility (95% credible interval for 2st derivation of estimated survival does not cross zero) for at least 3 years. Dashed lines show otherwise (no detectable change in the slope of the survival trend)


::: {.cell}

```{.r .cell-code}
slope_fit<-function(data){
  data = data.frame((data[,-1] - data[,-ncol(data)])*10)
  dar2<-sapply(data, function(p) quantile(p, probs = c(0.5)))
  data<-data.frame(data[,-1] - data[,-ncol(data)])
  data=sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cbinl<-c()
  x=1
  repeat{
    cbinl[x]<-
      if(data[1,x]>0|data[2,x]<0){cbinl[x]=1}else{cbinl[x]=0}
    x<-x+1
    if(x>length(data[1,])){break}}
  cbin=1;x<-1
  repeat{
    cbin[x+1]<-abs(cbinl[x]-cbinl[x+1])
    x=x+1
    if(x>(length(cbinl)-1)){break}}
  data<-data.frame(rbind(data,cbin));data[5,]<-yreal[c(2:499)]-0.049;data[6,]<-1:498
  row.names(data)<-c("cil","ciu","est","stat_change","year","timepoint")
  data[4,450]<-1
  data[4,50]<-1
  data=data[,50:450]
  lines(yreal[1:499],dar2,lwd=1,col=cola[xx],lty=2,lend=1)
  data2=(t(data[,data[4,]==1]))
  data2<-data.frame(data2)
  tr<-data2;y=1
  yreal=seq(1971,2020,length=498)
  repeat{
    if(((((tr[y,1]<0)&(tr[y,2])<0))|(((tr[y,1]>0)&(tr[y,2])>0)))&(tr[y+1,5]-tr[y,5])>3)
    {lines(yreal[tr[y,6]:tr[y+1,6]],
           dar2[tr[y,6]:tr[y+1,6]]
           ,lwd=1.9,col=cola[xx],lend=1)}
    y<-y+1;if(y>dim(tr)[1]-1){break}}
  }
```
:::


## Extraction of posterior samples

It will be used to visualize the predictions of the models jointly with showing uncertainty of model estimations


::: {.cell}

```{.r .cell-code}
## kidney posterior extraction ---------------------------------------------------
yreal <- seq(1971,2020,length=500)
first <- expand.grid(years10cen = ((yreal-1995.5)/10),
                     group = c('Denmark.Females','Finland.Females',
                               'Norway.Females','Sweden.Females',
                               'Denmark.Males','Finland.Males',
                               'Norway.Males','Sweden.Males'), y = 0)

ms_1y<-posterior_smooths(kidney_1y_model,smooth="s(years10cen,by=group,k=5)",
                         newdata = first)
post_fix<-as.data.frame(kidney_1y_model, 
                        variable = c("b_Intercept","b_groupFinland.Females",
                                     "b_groupNorway.Females","b_groupSweden.Females",
                                     "b_groupDenmark.Males","b_groupFinland.Males",
                                     "b_groupNorway.Males","b_groupSweden.Males"))
fixef(kidney_1y_model)
```

::: {.cell-output .cell-output-stdout}
```
                                     Estimate  Est.Error       Q2.5      Q97.5
Intercept                          62.9166996  0.4709803 61.9954644  63.839548
groupFinland.Females                9.7655183  0.6530530  8.4916694  11.040989
groupNorway.Females                10.0188568  0.6964579  8.6469979  11.394085
groupSweden.Females                11.0094471  0.5701680  9.9074180  12.134812
groupDenmark.Males                  0.9435266  0.6344587 -0.2988091   2.190200
groupFinland.Males                  6.6635108  0.6649952  5.3692967   7.954739
groupNorway.Males                   6.8247517  0.6442763  5.5550339   8.104949
groupSweden.Males                   9.5161475  0.5526470  8.4447934  10.584108
syears10cen:groupDenmark.Females_1 86.3473688 12.5076618 61.5859579 111.151206
syears10cen:groupFinland.Females_1 56.8184276  9.9847611 37.1651317  76.455252
syears10cen:groupNorway.Females_1  61.1418284 11.1802440 37.1255036  80.479086
syears10cen:groupSweden.Females_1  71.5460160  6.5272287 57.0198197  84.185931
syears10cen:groupDenmark.Males_1   87.1326393 10.8652189 65.9255187 108.822570
syears10cen:groupFinland.Males_1   67.9242131 13.9451916 40.1899647  94.032154
syears10cen:groupNorway.Males_1    69.7867667 10.2921134 48.2788787  88.589281
syears10cen:groupSweden.Males_1    97.8064227  8.4635548 81.1901711 114.576654
```
:::

```{.r .cell-code}
post_kidney_den_fem_1y<-ms_1y[,1:500]     +post_fix[,1]
post_kidney_fin_fem_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_kidney_nor_fem_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_kidney_swe_fem_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]
post_kidney_den_mal_1y<-ms_1y[,2001:2500] +post_fix[,1]+post_fix[,5]
post_kidney_fin_mal_1y<-ms_1y[,2501:3000] +post_fix[,1]+post_fix[,6]
post_kidney_nor_mal_1y<-ms_1y[,3001:3500] +post_fix[,1]+post_fix[,7]
post_kidney_swe_mal_1y<-ms_1y[,3501:4000] +post_fix[,1]+post_fix[,8]

post_fix<-as.data.frame(kidney_5y_model,
                        variable = c("b_Intercept","b_groupFinland.Females",
                                     "b_groupNorway.Females","b_groupSweden.Females",
                                     "b_groupDenmark.Males","b_groupFinland.Males",
                                     "b_groupNorway.Males","b_groupSweden.Males"))

ms_5y<-posterior_smooths(kidney_5y_model,smooth="s(years10cen,by=group,k=5)",
                         newdata = first)
post_kidney_den_fem_5y<-ms_5y[,1:500]     +post_fix[,1]
post_kidney_fin_fem_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_kidney_nor_fem_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_kidney_swe_fem_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]
post_kidney_den_mal_5y<-ms_5y[,2001:2500] +post_fix[,1]+post_fix[,5]
post_kidney_fin_mal_5y<-ms_5y[,2501:3000] +post_fix[,1]+post_fix[,6]
post_kidney_nor_mal_5y<-ms_5y[,3001:3500] +post_fix[,1]+post_fix[,7]
post_kidney_swe_mal_5y<-ms_5y[,3501:4000] +post_fix[,1]+post_fix[,8]

post_kidney_den_fem_cond<-(post_kidney_den_fem_5y/post_kidney_den_fem_1y)*100
post_kidney_den_mal_cond<-(post_kidney_den_mal_5y/post_kidney_den_mal_1y)*100
post_kidney_fin_fem_cond<-(post_kidney_fin_fem_5y/post_kidney_fin_fem_1y)*100
post_kidney_fin_mal_cond<-(post_kidney_fin_mal_5y/post_kidney_fin_mal_1y)*100
post_kidney_nor_fem_cond<-(post_kidney_nor_fem_5y/post_kidney_nor_fem_1y)*100
post_kidney_nor_mal_cond<-(post_kidney_nor_mal_5y/post_kidney_nor_mal_1y)*100
post_kidney_swe_fem_cond<-(post_kidney_swe_fem_5y/post_kidney_swe_fem_1y)*100
post_kidney_swe_mal_cond<-(post_kidney_swe_mal_5y/post_kidney_swe_mal_1y)*100


## bladder posterior extraction ---------------------------------------------------
ms_1y<-posterior_smooths(bladder_1y_model,smooth="s(years10cen,by=group,k=5)",
                         newdata = first)
post_fix<-as.data.frame(bladder_1y_model,
                        variable = c("b_Intercept","b_groupFinland.Females",
                                     "b_groupNorway.Females","b_groupSweden.Females",
                                     "b_groupDenmark.Males","b_groupFinland.Males",
                                     "b_groupNorway.Males","b_groupSweden.Males"))

post_bladder_den_fem_1y<-ms_1y[,1:500]     +post_fix[,1]
post_bladder_fin_fem_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_bladder_nor_fem_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_bladder_swe_fem_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]
post_bladder_den_mal_1y<-ms_1y[,2001:2500] +post_fix[,1]+post_fix[,5]
post_bladder_fin_mal_1y<-ms_1y[,2501:3000] +post_fix[,1]+post_fix[,6]
post_bladder_nor_mal_1y<-ms_1y[,3001:3500] +post_fix[,1]+post_fix[,7]
post_bladder_swe_mal_1y<-ms_1y[,3501:4000] +post_fix[,1]+post_fix[,8]

post_fix<-as.data.frame(bladder_5y_model,
                        variable = c("b_Intercept","b_groupFinland.Females",
                                     "b_groupNorway.Females","b_groupSweden.Females",
                                     "b_groupDenmark.Males","b_groupFinland.Males",
                                     "b_groupNorway.Males","b_groupSweden.Males"))
ms_5y<-posterior_smooths(bladder_5y_model,smooth="s(years10cen,by=group,k=5)",
                         newdata = first)

post_bladder_den_fem_5y<-ms_5y[,1:500]     +post_fix[,1]
post_bladder_fin_fem_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_bladder_nor_fem_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_bladder_swe_fem_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]
post_bladder_den_mal_5y<-ms_5y[,2001:2500] +post_fix[,1]+post_fix[,5]
post_bladder_fin_mal_5y<-ms_5y[,2501:3000] +post_fix[,1]+post_fix[,6]
post_bladder_nor_mal_5y<-ms_5y[,3001:3500] +post_fix[,1]+post_fix[,7]
post_bladder_swe_mal_5y<-ms_5y[,3501:4000] +post_fix[,1]+post_fix[,8]

post_bladder_den_fem_cond<-(post_bladder_den_fem_5y/post_bladder_den_fem_1y)*100
post_bladder_den_mal_cond<-(post_bladder_den_mal_5y/post_bladder_den_mal_1y)*100
post_bladder_fin_fem_cond<-(post_bladder_fin_fem_5y/post_bladder_fin_fem_1y)*100
post_bladder_fin_mal_cond<-(post_bladder_fin_mal_5y/post_bladder_fin_mal_1y)*100
post_bladder_nor_fem_cond<-(post_bladder_nor_fem_5y/post_bladder_nor_fem_1y)*100
post_bladder_nor_mal_cond<-(post_bladder_nor_mal_5y/post_bladder_nor_mal_1y)*100
post_bladder_swe_fem_cond<-(post_bladder_swe_fem_5y/post_bladder_swe_fem_1y)*100
post_bladder_swe_mal_cond<-(post_bladder_swe_mal_5y/post_bladder_swe_mal_1y)*100
```
:::


## Visualisations

### Denmark


::: {.cell}

```{.r .cell-code}
## DEN figures 
m <- matrix(c(15,1 ,2
              ,3,7 ,11
              ,4,8 ,12
              ,5,9 ,13
              ,6,10,14), nrow = 5, ncol =3 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6/2,0.37/2,0.6/2,0.37/2),
       widths = c(0.04,rep(0.96/2,2)))
par(mgp=c(1.6,0.62,0))
par(mar=c(0,0,0,0))


plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Kidney cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Bladder cancer",cex=1.6,font=3,xpd=TRUE)


range_b<-c(20,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,2);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in males", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)

range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in females", cex=1.4,srt=90)


range<-range_c;scal=scal_c
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)


### kidney  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification

  xx=1
  try(breakpo(post_kidney_den_mal_1y,-0.6), silent = TRUE )
```

::: {.cell-output .cell-output-stdout}
```
[1] 1997.955
               cil         ciu           est stat_change     year timepoint
X52  -3.873317e-03 0.001863205 -0.0007229729           1 1975.959        51
X179  4.178942e-05 0.005739009  0.0029330679           1 1988.430       178
X349 -1.489082e-05 0.005330153  0.0026822322           1 2005.123       348
X450 -2.727270e-03 0.001887971 -0.0002670158           1 2015.041       449
```
:::

```{.r .cell-code}
  xx=xx+1
  try(breakpo(post_kidney_den_mal_cond,0), silent = TRUE)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil          ciu          est stat_change     year timepoint
X52  -0.013819178 0.0059914644 -0.003699161           1 1975.959        51
X450 -0.006431086 0.0002440346 -0.003051680           1 2015.041       449
```
:::

```{.r .cell-code}
  xx=xx+1
  try(breakpo(post_kidney_den_mal_5y,0.6),silent = TRUE)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1997.857
               cil         ciu           est stat_change     year timepoint
X52  -5.815408e-03 0.001523405 -0.0017542159           1 1975.959        51
X175  4.849922e-05 0.006937825  0.0035418924           1 1988.037       174
X348 -4.583173e-05 0.007551952  0.0038595643           1 2005.025       347
X450 -4.318863e-03 0.002081212 -0.0008290497           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_den_mal_1y);xx=xx+1
polyg_surv(post_kidney_den_mal_cond);xx=xx+1
polyg_surv(post_kidney_den_mal_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="den_mal_",]$surv_1y~kidney[kidney$shou=="den_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="den_mal_",]$surv_cond~kidney[kidney$shou=="den_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="den_mal_",]$surv_5y~kidney[kidney$shou=="den_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_den_mal_1y);xx=xx+1
surv_fit(post_kidney_den_mal_cond);xx=xx+1
surv_fit(post_kidney_den_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(2004.3+xpo,yy+0.035*scal,2019+xpo,yy-0.17*scal,col="grey97",border="grey50",lwd=0.8)
repeat{
  points(2007+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(2005+xpo,2009+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.063);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2014+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(1973,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_den_mal_1y);xx=xx+1
polyg_slope(post_kidney_den_mal_cond);xx=xx+1
polyg_slope(post_kidney_den_mal_5y)

xx=1
slope_fit(post_kidney_den_mal_1y);xx=xx+1
slope_fit(post_kidney_den_mal_cond);xx=xx+1
slope_fit(post_kidney_den_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_den_fem_1y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1997.857
               cil         ciu          est stat_change     year timepoint
X52  -0.0052789864 0.001654776 -0.001480843           1 1975.959        51
X174  0.0001153345 0.006995794  0.003552194           1 1987.939       173
X343 -0.0001235785 0.006009009  0.003004209           1 2004.534       342
X450 -0.0042130922 0.001543517 -0.001110055           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_den_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu          est stat_change     year timepoint
X52  -0.015198464 0.006421943 -0.004384122           1 1975.959        51
X450 -0.006029555 0.001469692 -0.002343034           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_den_fem_5y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1997.955
               cil          ciu          est stat_change     year timepoint
X52  -6.837474e-03 0.0009558243 -0.002632280           1 1975.959        51
X180  9.610381e-05 0.0068582026  0.003478303           1 1988.528       179
X349 -1.918705e-04 0.0085122869  0.004287182           1 2005.123       348
X450 -4.888168e-03 0.0022119400 -0.001040177           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_den_fem_1y);xx=xx+1
polyg_surv(post_kidney_den_fem_cond);xx=xx+1
polyg_surv(post_kidney_den_fem_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="den_fem_",]$surv_1y~kidney[kidney$shou=="den_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="den_fem_",]$surv_cond~kidney[kidney$shou=="den_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="den_fem_",]$surv_5y~kidney[kidney$shou=="den_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_den_fem_1y);xx=xx+1
surv_fit(post_kidney_den_fem_cond);xx=xx+1
surv_fit(post_kidney_den_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"c",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_den_fem_1y);xx=xx+1
polyg_slope(post_kidney_den_fem_cond);xx=xx+1
polyg_slope(post_kidney_den_fem_5y)

xx=1
slope_fit(post_kidney_den_fem_1y);xx=xx+1
slope_fit(post_kidney_den_fem_cond);xx=xx+1
slope_fit(post_kidney_den_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### bladder
#### males 
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_den_mal_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1987.841
               cil           ciu           est stat_change     year timepoint
X52  -0.0016325052  3.241988e-04 -4.121825e-04           1 1975.959        51
X154 -0.0031127664 -3.997000e-06 -1.268754e-03           1 1985.975       153
X207 -0.0021125300  1.300559e-06 -9.225611e-04           1 1991.179       206
X450 -0.0007277584  8.557115e-04  4.909323e-05           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_den_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.002248261 0.001484830 -0.0001819162           1 1975.959        51
X450 -0.001373956 0.001030056 -0.0002153001           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_den_mal_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
               cil          ciu           est stat_change     year timepoint
X52  -0.0014444910 0.0006698539 -1.129205e-04           1 1975.959        51
X450 -0.0008980794 0.0009752176  2.604385e-06           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_den_mal_1y);xx=xx+1
polyg_surv(post_bladder_den_mal_cond);xx=xx+1
polyg_surv(post_bladder_den_mal_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="den_mal_",]$surv_1y~bladder[bladder$shou=="den_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="den_mal_",]$surv_cond~bladder[bladder$shou=="den_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="den_mal_",]$surv_5y~bladder[bladder$shou=="den_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_den_mal_1y);xx=xx+1
surv_fit(post_bladder_den_mal_cond);xx=xx+1
surv_fit(post_bladder_den_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"b",cex=2.2)

ypo=-55
xfl=50
rect(1958+xfl,range[1]+scal*0.965+ypo,1965+xfl,range[1]+scal*0.84+ypo,col="red2",border="grey50")
rect(1958+xfl,range[1]+scal*0.9135+ypo,1965+xfl,range[1]+scal*0.8865+ypo,col="grey96",border=NA)
rect(1960.5+xfl,range[1]+scal*0.965+ypo,1961.5+xfl,range[1]+scal*0.84+ypo,col="grey96",border=NA)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_den_mal_1y);xx=xx+1
polyg_slope(post_bladder_den_mal_cond);xx=xx+1
polyg_slope(post_bladder_den_mal_5y)

xx=1
slope_fit(post_bladder_den_mal_1y);xx=xx+1
slope_fit(post_bladder_den_mal_cond);xx=xx+1
slope_fit(post_bladder_den_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_den_fem_1y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
NULL
               cil         ciu           est stat_change     year timepoint
X52  -0.0011452316 0.001071765 -3.366792e-06           1 1975.959        51
X450 -0.0004188062 0.001736193  2.400756e-04           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_den_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.003381149 0.001845997 -0.0004804360           1 1975.959        51
X450 -0.001671954 0.002135210 -0.0001551614           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_den_fem_5y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
NULL
               cil         ciu           est stat_change     year timepoint
X52  -0.0018305228 0.001232044 -5.583193e-05           1 1975.959        51
X450 -0.0005866847 0.002486484  2.787560e-04           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_den_fem_1y);xx=xx+1
polyg_surv(post_bladder_den_fem_cond);xx=xx+1
polyg_surv(post_bladder_den_fem_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="den_fem_",]$surv_1y~bladder[bladder$shou=="den_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="den_fem_",]$surv_cond~bladder[bladder$shou=="den_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="den_fem_",]$surv_5y~bladder[bladder$shou=="den_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_den_fem_1y);xx=xx+1
surv_fit(post_bladder_den_fem_cond);xx=xx+1
surv_fit(post_bladder_den_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_den_fem_1y);xx=xx+1
polyg_slope(post_bladder_den_fem_cond);xx=xx+1
polyg_slope(post_bladder_den_fem_5y)

xx=1
slope_fit(post_bladder_den_fem_1y);xx=xx+1
slope_fit(post_bladder_den_fem_cond);xx=xx+1
slope_fit(post_bladder_den_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)
```

::: {.cell-output-display}
![Relative 1- , 5/1- and 5-year survival in Danish men (a, b) and women (c, d) in kidney (a, c) and bladder (b, d) cancers. The vertical lines show significant breakpoints in survival trends and bottom curves show estimated annual changes in survival. The solid line in survival curves and annual change curve indicate a plausible trend (see methods) whereas dotted lines suggest otherwise. Shaded regions indicate 95% Bayesian credible intervals. All curves are color coded (see the insert).](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-21-1.png){width=648}
:::
:::


### Finland


::: {.cell}

```{.r .cell-code}
## FIN figures --------------------------- -----------------
m <- matrix(c(15,1 ,2
              ,3,7 ,11
              ,4,8 ,12
              ,5,9 ,13
              ,6,10,14), nrow = 5, ncol =3 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6/2,0.37/2,0.6/2,0.37/2),
       widths = c(0.04,rep(0.96/2,2)))
par(mgp=c(1.6,0.62,0))
par(mar=c(0,0,0,0))


plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Kidney cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Bladder cancer",cex=1.6,font=3,xpd=TRUE)


range_b<-c(20,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,2);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in males", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)

range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in females", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0.2,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)


### kidney  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_fin_mal_1y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1982.735 1993.045 2008.069
               cil           ciu          est stat_change     year timepoint
X52   1.251906e-03  9.753600e-03  0.005448313           1 1975.959        51
X134 -6.107284e-05  1.464958e-02  0.007025578           1 1984.011       133
X187 -6.201703e-03 -5.875698e-05 -0.003164985           1 1989.216       186
X313 -5.704464e-03  9.231200e-05 -0.002703840           1 2001.588       312
X341  3.714935e-05  5.464883e-03  0.002764781           1 2004.338       340
X450  2.197299e-03  8.660425e-03  0.005550592           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_fin_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil           ciu           est stat_change     year timepoint
X52  -0.006060207  1.725669e-02  0.0050589766           1 1975.959        51
X181 -0.013433300 -1.344481e-04 -0.0065741729           1 1988.626       180
X208 -0.017291350  1.056906e-05 -0.0087015625           1 1991.278       207
X450 -0.004027360  5.847445e-03  0.0007981049           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_fin_mal_5y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1978.218 1997.759 2012.390
               cil           ciu          est stat_change     year timepoint
X52   1.091948e-03  1.146353e-02  0.006303498           1 1975.959        51
X128 -7.365122e-05  1.883627e-02  0.009302199           1 1983.422       127
X185 -7.314035e-03 -1.237461e-04 -0.003730906           1 1989.019       184
X320 -6.379535e-03  9.238486e-06 -0.003122838           1 2002.276       319
X357  4.222572e-05  1.058321e-02  0.005329692           1 2005.909       356
X450  1.593456e-03  1.016297e-02  0.005972762           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_fin_mal_1y);xx=xx+1
polyg_surv(post_kidney_fin_mal_cond);xx=xx+1
polyg_surv(post_kidney_fin_mal_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="fin_mal_",]$surv_1y~kidney[kidney$shou=="fin_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="fin_mal_",]$surv_cond~kidney[kidney$shou=="fin_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="fin_mal_",]$surv_5y~kidney[kidney$shou=="fin_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_fin_mal_1y);xx=xx+1
surv_fit(post_kidney_fin_mal_cond);xx=xx+1
surv_fit(post_kidney_fin_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(2004.3+xpo,yy+0.035*scal,2019+xpo,yy-0.17*scal,col="grey97",border="grey50",lwd=0.8)
repeat{
  points(2007+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(2005+xpo,2009+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.063);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2014+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(1973,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_fin_mal_1y);xx=xx+1
polyg_slope(post_kidney_fin_mal_cond);xx=xx+1
polyg_slope(post_kidney_fin_mal_5y)

xx=1
slope_fit(post_kidney_fin_mal_1y);xx=xx+1
slope_fit(post_kidney_fin_mal_cond);xx=xx+1
slope_fit(post_kidney_fin_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_fin_fem_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.002285341 0.002193369 -0.0001186196           1 1975.959        51
X450 -0.001987692 0.001627751 -0.0002071178           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_fin_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.008069535 0.007711410 -0.0015922772           1 1975.959        51
X450 -0.002096686 0.004933333  0.0009450956           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_fin_fem_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1993.045
              cil           ciu           est stat_change     year timepoint
X52  -0.001727993  4.698612e-03  0.0007819920           1 1975.959        51
X188 -0.006123200 -1.693800e-05 -0.0028342725           1 1989.314       187
X315 -0.005415751  4.254021e-05 -0.0024907598           1 2001.785       314
X450 -0.001243796  4.191357e-03  0.0009054231           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_fin_fem_1y);xx=xx+1
polyg_surv(post_kidney_fin_fem_cond);xx=xx+1
polyg_surv(post_kidney_fin_fem_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="fin_fem_",]$surv_1y~kidney[kidney$shou=="fin_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="fin_fem_",]$surv_cond~kidney[kidney$shou=="fin_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="fin_fem_",]$surv_5y~kidney[kidney$shou=="fin_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_fin_fem_1y);xx=xx+1
surv_fit(post_kidney_fin_fem_cond);xx=xx+1
surv_fit(post_kidney_fin_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"c",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_fin_fem_1y);xx=xx+1
polyg_slope(post_kidney_fin_fem_cond);xx=xx+1
polyg_slope(post_kidney_fin_fem_5y)

xx=1
slope_fit(post_kidney_fin_fem_1y);xx=xx+1
slope_fit(post_kidney_fin_fem_cond);xx=xx+1
slope_fit(post_kidney_fin_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### bladder  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_fin_mal_1y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1987.939
               cil           ciu           est stat_change     year timepoint
X52  -0.0022184592  8.334726e-04 -0.0005744193           1 1975.959        51
X143 -0.0052829927 -8.433043e-06 -0.0023406571           1 1984.895       142
X260 -0.0038975046  2.630815e-05 -0.0018591531           1 1996.384       259
X450 -0.0008248561  1.457973e-03  0.0002440824           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_fin_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1982.931
              cil           ciu           est stat_change     year timepoint
X52  -0.009137727 -4.176241e-04 -0.0044953247           1 1975.959        51
X63  -0.011179563  1.129470e-05 -0.0052178164           1 1977.039        62
X95  -0.014262925 -1.118466e-05 -0.0066761268           1 1980.181        94
X221 -0.008391148  7.861122e-05 -0.0041919768           1 1992.554       220
X450 -0.002896776  2.308572e-03 -0.0002677898           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_fin_mal_5y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1987.939
              cil           ciu           est stat_change     year timepoint
X52  -0.005405731  2.284214e-04 -2.384552e-03           1 1975.959        51
X82  -0.009838031 -9.428047e-06 -4.552786e-03           1 1978.905        81
X258 -0.007394420  4.352689e-05 -3.762503e-03           1 1996.187       257
X450 -0.002170042  2.166471e-03 -9.017668e-06           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_fin_mal_1y);xx=xx+1
polyg_surv(post_bladder_fin_mal_cond);xx=xx+1
polyg_surv(post_bladder_fin_mal_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="fin_mal_",]$surv_1y~bladder[bladder$shou=="fin_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="fin_mal_",]$surv_cond~bladder[bladder$shou=="fin_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="fin_mal_",]$surv_5y~bladder[bladder$shou=="fin_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_fin_mal_1y);xx=xx+1
surv_fit(post_bladder_fin_mal_cond);xx=xx+1
surv_fit(post_bladder_fin_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"b",cex=2.2)

ypo=-55
xfl=50
rect(1958+xfl,range[1]+scal*0.965+ypo,1965+xfl,range[1]+scal*0.84+ypo,col="grey96",border="grey50")
rect(1958+xfl,range[1]+scal*0.9135+ypo,1965+xfl,range[1]+scal*0.8865+ypo,col="dodgerblue3",border=NA)
rect(1960.5+xfl,range[1]+scal*0.965+ypo,1961.5+xfl,range[1]+scal*0.84+ypo,col="dodgerblue3",border=NA)


##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_fin_mal_1y);xx=xx+1
polyg_slope(post_bladder_fin_mal_cond);xx=xx+1
polyg_slope(post_bladder_fin_mal_5y)

xx=1
slope_fit(post_bladder_fin_mal_1y);xx=xx+1
slope_fit(post_bladder_fin_mal_cond);xx=xx+1
slope_fit(post_bladder_fin_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_fin_fem_1y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil          ciu           est stat_change     year timepoint
X52  -0.002183736 0.0011593996 -0.0002700772           1 1975.959        51
X450 -0.001854413 0.0009605772 -0.0002847451           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_fin_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu          est stat_change     year timepoint
X52  -0.007180312 0.002104588 -0.001868214           1 1975.959        51
X450 -0.003987681 0.002338563 -0.000628803           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_fin_fem_5y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.004447125 0.001342582 -0.0010097857           1 1975.959        51
X450 -0.003544778 0.001269272 -0.0007881489           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_fin_fem_1y);xx=xx+1
polyg_surv(post_bladder_fin_fem_cond);xx=xx+1
polyg_surv(post_bladder_fin_fem_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="fin_fem_",]$surv_1y~bladder[bladder$shou=="fin_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="fin_fem_",]$surv_cond~bladder[bladder$shou=="fin_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="fin_fem_",]$surv_5y~bladder[bladder$shou=="fin_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_fin_fem_1y);xx=xx+1
surv_fit(post_bladder_fin_fem_cond);xx=xx+1
surv_fit(post_bladder_fin_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_fin_fem_1y);xx=xx+1
polyg_slope(post_bladder_fin_fem_cond);xx=xx+1
polyg_slope(post_bladder_fin_fem_5y)

xx=1
slope_fit(post_bladder_fin_fem_1y);xx=xx+1
slope_fit(post_bladder_fin_fem_cond);xx=xx+1
slope_fit(post_bladder_fin_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)
```

::: {.cell-output-display}
![Relative 1- , 5/1- and 5-year survival in Finnish men (a, b) and women (c, d) in kidney (a, c) and bladder (b, d) cancers. The vertical lines show significant breakpoints in survival trends and bottom curves show estimated annual changes in survival. The solid line in survival curves and annual change curve indicate a plausible trend whereas dotted lines suggest otherwise. Shaded regions indicate 95% Bayesian credible intervals. All curves are color coded (see the insert).](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-22-1.png){width=648}
:::
:::


### Norway


::: {.cell}

```{.r .cell-code}
## NOR figures --------------------------- -----------------
m <- matrix(c(15,1 ,2
              ,3,7 ,11
              ,4,8 ,12
              ,5,9 ,13
              ,6,10,14), nrow = 5, ncol =3 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6/2,0.37/2,0.6/2,0.37/2),
       widths = c(0.04,rep(0.96/2,2)))
par(mgp=c(1.6,0.62,0))
par(mar=c(0,0,0,0))


plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Kidney cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Bladder cancer",cex=1.6,font=3,xpd=TRUE)


range_b<-c(20,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,2);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in males", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)

range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in females", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0.2,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)


### kidney  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_nor_mal_1y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1992.456
               cil         ciu           est stat_change     year timepoint
X52  -2.473597e-03 0.002347702  9.249719e-05           1 1975.959        51
X185  7.822615e-07 0.005261147  2.390624e-03           1 1989.019       184
X222 -7.426611e-07 0.006726897  2.854314e-03           1 1992.652       221
X450 -2.629401e-03 0.001281505 -3.554086e-04           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_nor_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu          est stat_change     year timepoint
X52  -0.010394403 0.005458141 -0.002028596           1 1975.959        51
X450 -0.005127154 0.001302059 -0.001809260           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_nor_mal_5y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1993.045
               cil         ciu           est stat_change     year timepoint
X52  -4.300883e-03 0.002282348 -0.0005614128           1 1975.959        51
X178  5.637905e-05 0.006744308  0.0033192207           1 1988.332       177
X327 -4.662473e-05 0.005784457  0.0028257014           1 2002.963       326
X450 -3.980805e-03 0.001806214 -0.0006698603           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_nor_mal_1y);xx=xx+1
polyg_surv(post_kidney_nor_mal_cond);xx=xx+1
polyg_surv(post_kidney_nor_mal_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="nor_mal_",]$surv_1y~kidney[kidney$shou=="nor_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="nor_mal_",]$surv_cond~kidney[kidney$shou=="nor_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="nor_mal_",]$surv_5y~kidney[kidney$shou=="nor_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_nor_mal_1y);xx=xx+1
surv_fit(post_kidney_nor_mal_cond);xx=xx+1
surv_fit(post_kidney_nor_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(2004.3+xpo,yy+0.035*scal,2019+xpo,yy-0.17*scal,col="grey97",border="grey50",lwd=0.8)
repeat{
  points(2007+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(2005+xpo,2009+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.063);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2014+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(1973,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_nor_mal_1y);xx=xx+1
polyg_slope(post_kidney_nor_mal_cond);xx=xx+1
polyg_slope(post_kidney_nor_mal_5y)

xx=1
slope_fit(post_kidney_nor_mal_1y);xx=xx+1
slope_fit(post_kidney_nor_mal_cond);xx=xx+1
slope_fit(post_kidney_nor_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_nor_fem_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.002322896 0.002427798  0.0001123652           1 1975.959        51
X450 -0.002750548 0.001260199 -0.0002845514           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_nor_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu          est stat_change     year timepoint
X52  -0.008887916 0.004507055 -0.001717225           1 1975.959        51
X450 -0.004633979 0.001769529 -0.001186828           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_nor_fem_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.003844969 0.002219585 -0.0001963866           1 1975.959        51
X450 -0.004125286 0.001427719 -0.0005969941           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_nor_fem_1y);xx=xx+1
polyg_surv(post_kidney_nor_fem_cond);xx=xx+1
polyg_surv(post_kidney_nor_fem_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="nor_fem_",]$surv_1y~kidney[kidney$shou=="nor_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="nor_fem_",]$surv_cond~kidney[kidney$shou=="nor_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="nor_fem_",]$surv_5y~kidney[kidney$shou=="nor_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_nor_fem_1y);xx=xx+1
surv_fit(post_kidney_nor_fem_cond);xx=xx+1
surv_fit(post_kidney_nor_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"c",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_nor_fem_1y);xx=xx+1
polyg_slope(post_kidney_nor_fem_cond);xx=xx+1
polyg_slope(post_kidney_nor_fem_5y)

xx=1
slope_fit(post_kidney_nor_fem_1y);xx=xx+1
slope_fit(post_kidney_nor_fem_cond);xx=xx+1
slope_fit(post_kidney_nor_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### bladder  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_nor_mal_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1984.208
               cil           ciu           est stat_change     year timepoint
X52  -0.0025203234  2.182326e-04 -0.0009816881           1 1975.959        51
X98  -0.0051450231 -1.077229e-05 -0.0022697502           1 1980.476        97
X223 -0.0034707513  5.137920e-06 -0.0016713700           1 1992.751       222
X450 -0.0008177666  1.414083e-03  0.0002940655           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_nor_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.003637900 0.002369310 -3.416648e-04           1 1975.959        51
X450 -0.001765079 0.001989374 -1.690151e-05           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_nor_mal_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1987.939
              cil           ciu           est stat_change     year timepoint
X52  -0.002808277  8.227313e-04 -0.0006594371           1 1975.959        51
X157 -0.005813839 -8.767410e-06 -0.0024195201           1 1986.270       156
X212 -0.004345260  4.068751e-06 -0.0019170167           1 1991.670       211
X450 -0.001012390  1.980636e-03  0.0003157261           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_nor_mal_1y);xx=xx+1
polyg_surv(post_bladder_nor_mal_cond);xx=xx+1
polyg_surv(post_bladder_nor_mal_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="nor_mal_",]$surv_1y~bladder[bladder$shou=="nor_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="nor_mal_",]$surv_cond~bladder[bladder$shou=="nor_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="nor_mal_",]$surv_5y~bladder[bladder$shou=="nor_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_nor_mal_1y);xx=xx+1
surv_fit(post_bladder_nor_mal_cond);xx=xx+1
surv_fit(post_bladder_nor_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"b",cex=2.2)

ypo=-55
xfl=50

rect(1958+xfl,range[1]+scal*0.965+ypo,1965+xfl,range[1]+scal*0.84+ypo,col="red2",border="grey50")
rect(1958+xfl,range[1]+scal*0.9285+ypo,1965+xfl,range[1]+scal*0.8735+ypo,col="grey96",border=NA)
rect(1959.5+xfl,range[1]+scal*0.965+ypo,1962.2+xfl,range[1]+scal*0.84+ypo,col="grey96",border=NA)
rect(1958+xfl,range[1]+scal*0.9135+ypo,1965+xfl,range[1]+scal*0.8865+ypo,col="darkblue",border=NA)
rect(1960.3+xfl,range[1]+scal*0.965+ypo,1961.5+xfl,range[1]+scal*0.84+ypo,col="darkblue",border=NA)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_nor_mal_1y);xx=xx+1
polyg_slope(post_bladder_nor_mal_cond);xx=xx+1
polyg_slope(post_bladder_nor_mal_5y)

xx=1
slope_fit(post_bladder_nor_mal_1y);xx=xx+1
slope_fit(post_bladder_nor_mal_cond);xx=xx+1
slope_fit(post_bladder_nor_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_nor_fem_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
               cil          ciu           est stat_change     year timepoint
X52  -0.0028909056 0.0005921696 -0.0003758536           1 1975.959        51
X450 -0.0008683348 0.0018210386  0.0001728280           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_nor_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.004838467 0.003173042 -4.968787e-04           1 1975.959        51
X450 -0.001968442 0.002736371  1.633368e-05           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_nor_fem_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.003107642 0.001124959 -0.0003985393           1 1975.959        51
X450 -0.001112607 0.002715953  0.0003019687           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_nor_fem_1y);xx=xx+1
polyg_surv(post_bladder_nor_fem_cond);xx=xx+1
polyg_surv(post_bladder_nor_fem_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="nor_fem_",]$surv_1y~bladder[bladder$shou=="nor_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="nor_fem_",]$surv_cond~bladder[bladder$shou=="nor_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="nor_fem_",]$surv_5y~bladder[bladder$shou=="nor_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_nor_fem_1y);xx=xx+1
surv_fit(post_bladder_nor_fem_cond);xx=xx+1
surv_fit(post_bladder_nor_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_nor_fem_1y);xx=xx+1
polyg_slope(post_bladder_nor_fem_cond);xx=xx+1
polyg_slope(post_bladder_nor_fem_5y)

xx=1
slope_fit(post_bladder_nor_fem_1y);xx=xx+1
slope_fit(post_bladder_nor_fem_cond);xx=xx+1
slope_fit(post_bladder_nor_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)
```

::: {.cell-output-display}
![Relative 1- , 5/1- and 5-year survival in Norwish men (a, b) and women (c, d) in kidney (a, c) and bladder (b, d) cancers. The vertical lines show significant breakpoints in survival trends and bottom curves show estimated annual changes in survival. The solid line in survival curves and annual change curve indicate a plausible trend whereas dotted lines suggest otherwise. Shaded regions indicate 95% Bayesian credible intervals. All curves are color coded (see the insert).](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-23-1.png){width=648}
:::
:::


### Sweden


::: {.cell}

```{.r .cell-code}
#  SWE figures --------------------------- -----------------
m <- matrix(c(15,1 ,2
              ,3,7 ,11
              ,4,8 ,12
              ,5,9 ,13
              ,6,10,14), nrow = 5, ncol =3 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6/2,0.37/2,0.6/2,0.37/2),
       widths = c(0.04,rep(0.96/2,2)))
par(mgp=c(1.6,0.62,0))
par(mar=c(0,0,0,0))


plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Kidney cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Bladder cancer",cex=1.6,font=3,xpd=TRUE)


range_b<-c(20,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,2);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in males", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)

range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(0,range_b[1]+scal_b*0.5,"Relative survival (%) in females", cex=1.4,srt=90)


range<-range_c;scal=scal_c
par(mar=c(2.5,0,0.2,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(0,range_c[1]+scal_c*0.5,"Annual change",
     cex=1.4,srt=90)


### kidney  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_swe_mal_1y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1982.931 1997.955 2012.881
               cil           ciu          est stat_change     year timepoint
X52  -7.691618e-03 -1.879896e-03 -0.004902038           1 1975.959        51
X180 -4.090013e-03  6.823574e-05 -0.001978172           1 1988.528       179
X210  4.243717e-05  5.306159e-03  0.002693678           1 1991.474       209
X319 -5.495146e-05  3.832330e-03  0.001886716           1 2002.177       318
X356 -5.382778e-03 -4.311208e-05 -0.002678642           1 2005.811       355
X450 -5.599548e-03 -1.019863e-03 -0.003372732           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_swe_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.010988190 0.010506769 -5.221504e-05           1 1975.959        51
X450 -0.004090049 0.003302294 -2.875884e-04           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_swe_mal_5y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1997.857
               cil          ciu          est stat_change     year timepoint
X52  -6.240272e-03 0.0002460628 -0.002636705           1 1975.959        51
X186  5.290399e-08 0.0047064782  0.002268881           1 1989.117       185
X321 -4.247222e-05 0.0047846429  0.002296484           1 2002.374       320
X450 -5.635747e-03 0.0003660722 -0.002299369           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_swe_mal_1y);xx=xx+1
polyg_surv(post_kidney_swe_mal_cond);xx=xx+1
polyg_surv(post_kidney_swe_mal_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="swe_mal_",]$surv_1y~kidney[kidney$shou=="swe_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="swe_mal_",]$surv_cond~kidney[kidney$shou=="swe_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="swe_mal_",]$surv_5y~kidney[kidney$shou=="swe_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_swe_mal_1y);xx=xx+1
surv_fit(post_kidney_swe_mal_cond);xx=xx+1
surv_fit(post_kidney_swe_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(2004.3+xpo,yy+0.035*scal,2019+xpo,yy-0.17*scal,col="grey97",border="grey50",lwd=0.8)
repeat{
  points(2007+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(2005+xpo,2009+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.063);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2014+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(2014+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.063)
text(1973,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_swe_mal_1y);xx=xx+1
polyg_slope(post_kidney_swe_mal_cond);xx=xx+1
polyg_slope(post_kidney_swe_mal_5y)

xx=1
slope_fit(post_kidney_swe_mal_1y);xx=xx+1
slope_fit(post_kidney_swe_mal_cond);xx=xx+1
slope_fit(post_kidney_swe_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_kidney_swe_fem_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil          ciu           est stat_change     year timepoint
X52  -0.003501163 0.0005762856 -0.0003333275           1 1975.959        51
X450 -0.003371968 0.0002900232 -0.0005676406           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_swe_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu          est stat_change     year timepoint
X52  -0.004660325 0.008292492  0.001087049           1 1975.959        51
X450 -0.003738983 0.002045565 -0.001015776           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_kidney_swe_fem_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1990.296
               cil          ciu           est stat_change     year timepoint
X52  -2.127315e-03 0.0025669932  0.0003221797           1 1975.959        51
X157  5.142918e-05 0.0061483956  0.0028713820           1 1986.270       156
X238 -8.497513e-06 0.0073549486  0.0030634780           1 1994.224       237
X450 -3.724267e-03 0.0009851772 -0.0009796037           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_kidney_swe_fem_1y);xx=xx+1
polyg_surv(post_kidney_swe_fem_cond);xx=xx+1
polyg_surv(post_kidney_swe_fem_5y)

# data points of estimated survival
xx=1
points(kidney[kidney$shou=="swe_fem_",]$surv_1y~kidney[kidney$shou=="swe_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="swe_fem_",]$surv_cond~kidney[kidney$shou=="swe_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(kidney[kidney$shou=="swe_fem_",]$surv_5y~kidney[kidney$shou=="swe_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_kidney_swe_fem_1y);xx=xx+1
surv_fit(post_kidney_swe_fem_cond);xx=xx+1
surv_fit(post_kidney_swe_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"c",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_kidney_swe_fem_1y);xx=xx+1
polyg_slope(post_kidney_swe_fem_cond);xx=xx+1
polyg_slope(post_kidney_swe_fem_5y)

xx=1
slope_fit(post_kidney_swe_fem_1y);xx=xx+1
slope_fit(post_kidney_swe_fem_cond);xx=xx+1
slope_fit(post_kidney_swe_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### bladder  % ------------------------------------------------------
#### males --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_swe_mal_1y,0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1987.939 2007.971
               cil           ciu          est stat_change     year timepoint
X52  -2.542748e-03 -1.944485e-04 -0.001331817           1 1975.959        51
X248 -3.040292e-03  5.413202e-05 -0.001509809           1 1995.206       247
X297  2.844387e-05  2.327749e-03  0.001147965           1 2000.017       296
X450  2.321616e-04  2.153761e-03  0.001190479           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_swe_mal_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.003266787 0.002459252 -3.898014e-04           1 1975.959        51
X450 -0.001570648 0.001744477  2.192753e-05           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_swe_mal_5y,-0.6)
```

::: {.cell-output .cell-output-stdout}
```
[1] 1987.939 2007.971
               cil           ciu          est stat_change     year timepoint
X52  -2.891437e-03  4.303120e-04 -0.001129128           1 1975.959        51
X104 -6.380849e-03 -6.625114e-06 -0.002998925           1 1981.065       103
X230 -4.511355e-03  4.781303e-05 -0.002164559           1 1993.438       229
X302  3.690939e-05  3.293560e-03  0.001562433           1 2000.508       301
X421 -1.231157e-05  4.835520e-03  0.002306816           1 2012.193       420
X450 -1.021707e-04  2.664318e-03  0.001225287           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_swe_mal_1y);xx=xx+1
polyg_surv(post_bladder_swe_mal_cond);xx=xx+1
polyg_surv(post_bladder_swe_mal_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="swe_mal_",]$surv_1y~bladder[bladder$shou=="swe_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="swe_mal_",]$surv_cond~bladder[bladder$shou=="swe_mal_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="swe_mal_",]$surv_5y~bladder[bladder$shou=="swe_mal_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_swe_mal_1y);xx=xx+1
surv_fit(post_bladder_swe_mal_cond);xx=xx+1
surv_fit(post_bladder_swe_mal_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"b",cex=2.2)

ypo=-55
xfl=50
rect(1958+xfl,range[1]+scal*0.965+ypo,1965+xfl,range[1]+scal*0.84+ypo,col="blue",border="grey50")
rect(1958+xfl,range[1]+scal*0.9135+ypo,1965+xfl,range[1]+scal*0.8865+ypo,col="yellow",border=NA)
rect(1960.5+xfl,range[1]+scal*0.965+ypo,1961.5+xfl,range[1]+scal*0.84+ypo,col="yellow",border=NA)


##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_swe_mal_1y);xx=xx+1
polyg_slope(post_bladder_swe_mal_cond);xx=xx+1
polyg_slope(post_bladder_swe_mal_5y)

xx=1
slope_fit(post_bladder_swe_mal_1y);xx=xx+1
slope_fit(post_bladder_swe_mal_cond);xx=xx+1
slope_fit(post_bladder_swe_mal_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



#### females --------------
range<-range_b;scal=scal_b
par(mgp=c(1.6,0.4,0))
par(mar=c(0,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="grey96",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_bladder_swe_fem_1y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
               cil         ciu           est stat_change     year timepoint
X52  -0.0012935058 0.001003008 -0.0001166470           1 1975.959        51
X450 -0.0005014256 0.001741421  0.0002797157           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_swe_fem_cond,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
              cil         ciu           est stat_change     year timepoint
X52  -0.001925537 0.002388258  7.294547e-05           1 1975.959        51
X450 -0.001667953 0.002207338 -6.597862e-05           1 2015.041       449
```
:::

```{.r .cell-code}
xx=xx+1
breakpo(post_bladder_swe_fem_5y,0)
```

::: {.cell-output .cell-output-stdout}
```
NULL
               cil         ciu           est stat_change     year timepoint
X52  -0.0012636677 0.001416280 -2.569434e-06           1 1975.959        51
X450 -0.0004659446 0.002212743  1.794117e-04           1 2015.041       449
```
:::

```{.r .cell-code}
# 95% credible interval for survival
xx=1
polyg_surv(post_bladder_swe_fem_1y);xx=xx+1
polyg_surv(post_bladder_swe_fem_cond);xx=xx+1
polyg_surv(post_bladder_swe_fem_5y)

# data points of estimated survival
xx=1
points(bladder[bladder$shou=="swe_fem_",]$surv_1y~bladder[bladder$shou=="swe_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="swe_fem_",]$surv_cond~bladder[bladder$shou=="swe_fem_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(bladder[bladder$shou=="swe_fem_",]$surv_5y~bladder[bladder$shou=="swe_fem_",]$year
       ,pch=17,col=colc[xx],cex=1)

tckk=-0.016
# fitted lines for survival
xx=1
surv_fit(post_bladder_swe_fem_1y);xx=xx+1
surv_fit(post_bladder_swe_fem_cond);xx=xx+1
surv_fit(post_bladder_swe_fem_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971,tck=tckk)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=20)),pos=1971,tck=tckk)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)),tck=tckk)
lines(c(1971,2020),c(range[1],range[1]))
text(1973,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,1.4,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
#rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="grey96",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="grey96",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_bladder_swe_fem_1y);xx=xx+1
polyg_slope(post_bladder_swe_fem_cond);xx=xx+1
polyg_slope(post_bladder_swe_fem_5y)

xx=1
slope_fit(post_bladder_swe_fem_1y);xx=xx+1
slope_fit(post_bladder_swe_fem_cond);xx=xx+1
slope_fit(post_bladder_swe_fem_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971,tck=tckk*2)
axis(2,las=2,cex.axis=1.4,at=c(seq(-1,range[2],by=1)),pos=1971,tck=tckk*2)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)),tck=tckk*2)
text(c(1971, seq(1979,2020,by=10)),c(rep(range[1]-0.1*scal,6)),c(1971, seq(1980,2020,by=10)),xpd=TRUE,cex=1.4)
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)
```

::: {.cell-output-display}
![Relative 1- , 5/1- and 5-year survival in Swedish men (a, b) and women (c, d) in kidney (a, c) and bladder (b, d) cancers. The vertical lines show significant breakpoints in survival trends and bottom curves show estimated annual changes in survival. The solid line in survival curves and annual change curve indicate a plausible trend whereas dotted lines suggest otherwise. Shaded regions indicate 95% Bayesian credible intervals. All curves are color coded (see the insert).](Kidney_bladder_RCode_files/figure-html/unnamed-chunk-24-1.png){width=648}
:::
:::


# Reproducibility


::: {.cell}

```{.r .cell-code}
sessioninfo::session_info()
```

::: {.cell-output .cell-output-stdout}
```
─ Session info ───────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.2 (2022-10-31 ucrt)
 os       Windows 10 x64 (build 19044)
 system   x86_64, mingw32
 ui       RTerm
 language (EN)
 collate  English_United States.utf8
 ctype    English_United States.utf8
 tz       Europe/Prague
 date     2023-04-09
 pandoc   2.19.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

─ Packages ───────────────────────────────────────────────────────────────────
 ! package        * version date (UTC) lib source
   abind            1.4-5   2016-07-21 [1] CRAN (R 4.2.0)
   backports        1.4.1   2021-12-13 [1] CRAN (R 4.2.0)
   base64enc        0.1-3   2015-07-28 [1] CRAN (R 4.2.0)
   bayesplot        1.10.0  2022-11-16 [1] CRAN (R 4.2.2)
   bridgesampling   1.1-2   2021-04-16 [1] CRAN (R 4.2.2)
   brms           * 2.18.0  2022-09-19 [1] CRAN (R 4.2.2)
   Brobdingnag      1.2-9   2022-10-19 [1] CRAN (R 4.2.2)
   callr            3.7.3   2022-11-02 [1] CRAN (R 4.2.2)
   checkmate        2.1.0   2022-04-21 [1] CRAN (R 4.2.2)
   cli              3.6.0   2023-01-09 [1] CRAN (R 4.2.2)
   coda             0.19-4  2020-09-30 [1] CRAN (R 4.2.2)
   codetools        0.2-19  2023-02-01 [1] CRAN (R 4.2.2)
   colorspace       2.1-0   2023-01-23 [1] CRAN (R 4.2.2)
   colourpicker     1.2.0   2022-10-28 [1] CRAN (R 4.2.2)
   crayon           1.5.2   2022-09-29 [1] CRAN (R 4.2.2)
   crosstalk        1.2.0   2021-11-04 [1] CRAN (R 4.2.2)
   curl             5.0.0   2023-01-12 [1] CRAN (R 4.2.2)
   digest           0.6.31  2022-12-11 [1] CRAN (R 4.2.2)
   distributional   0.3.1   2022-09-02 [1] CRAN (R 4.2.2)
   dplyr          * 1.1.0   2023-01-29 [1] CRAN (R 4.2.2)
   DT               0.27    2023-01-17 [1] CRAN (R 4.2.2)
   dygraphs         1.1.1.6 2018-07-11 [1] CRAN (R 4.2.2)
   ellipsis         0.3.2   2021-04-29 [1] CRAN (R 4.2.2)
   evaluate         0.20    2023-01-17 [1] CRAN (R 4.2.2)
   fansi            1.0.4   2023-01-22 [1] CRAN (R 4.2.2)
   farver           2.1.1   2022-07-06 [1] CRAN (R 4.2.2)
   fastmap          1.1.1   2023-02-24 [1] CRAN (R 4.2.2)
   generics         0.1.3   2022-07-05 [1] CRAN (R 4.2.2)
   ggplot2        * 3.4.1   2023-02-10 [1] CRAN (R 4.2.2)
   glue             1.6.2   2022-02-24 [1] CRAN (R 4.2.2)
   gridExtra        2.3     2017-09-09 [1] CRAN (R 4.2.2)
   gtable           0.3.1   2022-09-01 [1] CRAN (R 4.2.2)
   gtools           3.9.4   2022-11-27 [1] CRAN (R 4.2.2)
   htmltools        0.5.4   2022-12-07 [1] CRAN (R 4.2.2)
   htmlwidgets      1.6.1   2023-01-07 [1] CRAN (R 4.2.2)
   httpuv           1.6.9   2023-02-14 [1] CRAN (R 4.2.2)
   igraph           1.4.1   2023-02-24 [1] CRAN (R 4.2.2)
   inline           0.3.19  2021-05-31 [1] CRAN (R 4.2.2)
   jsonlite         1.8.4   2022-12-06 [1] CRAN (R 4.2.2)
   knitr            1.42    2023-01-25 [1] CRAN (R 4.2.2)
   labeling         0.4.2   2020-10-20 [1] CRAN (R 4.2.0)
   later            1.3.0   2021-08-18 [1] CRAN (R 4.2.2)
   lattice          0.20-45 2021-09-22 [2] CRAN (R 4.2.2)
   lifecycle        1.0.3   2022-10-07 [1] CRAN (R 4.2.2)
   loo              2.5.1   2022-03-24 [1] CRAN (R 4.2.2)
   magrittr         2.0.3   2022-03-30 [1] CRAN (R 4.2.2)
   markdown         1.5     2023-01-31 [1] CRAN (R 4.2.2)
   Matrix           1.5-3   2022-11-11 [1] CRAN (R 4.2.2)
   matrixStats      0.63.0  2022-11-18 [1] CRAN (R 4.2.2)
   mgcv             1.8-41  2022-10-21 [2] CRAN (R 4.2.2)
   mime             0.12    2021-09-28 [1] CRAN (R 4.2.0)
   miniUI           0.1.1.1 2018-05-18 [1] CRAN (R 4.2.2)
   munsell          0.5.0   2018-06-12 [1] CRAN (R 4.2.2)
   mvtnorm          1.1-3   2021-10-08 [1] CRAN (R 4.2.0)
   nlme             3.1-162 2023-01-31 [1] CRAN (R 4.2.2)
   pillar           1.8.1   2022-08-19 [1] CRAN (R 4.2.2)
   pkgbuild         1.4.0   2022-11-27 [1] CRAN (R 4.2.2)
   pkgconfig        2.0.3   2019-09-22 [1] CRAN (R 4.2.2)
   plyr             1.8.8   2022-11-11 [1] CRAN (R 4.2.2)
   posterior        1.4.0   2023-02-22 [1] CRAN (R 4.2.2)
   prettyunits      1.1.1   2020-01-24 [1] CRAN (R 4.2.2)
   processx         3.8.0   2022-10-26 [1] CRAN (R 4.2.2)
   promises         1.2.0.1 2021-02-11 [1] CRAN (R 4.2.2)
   ps               1.7.2   2022-10-26 [1] CRAN (R 4.2.2)
   R6               2.5.1   2021-08-19 [1] CRAN (R 4.2.2)
   Rcpp           * 1.0.10  2023-01-22 [1] CRAN (R 4.2.2)
 D RcppParallel     5.1.7   2023-02-27 [1] CRAN (R 4.2.2)
   reshape2         1.4.4   2020-04-09 [1] CRAN (R 4.2.2)
   rlang            1.0.6   2022-09-24 [1] CRAN (R 4.2.2)
   rmarkdown        2.20    2023-01-19 [1] CRAN (R 4.2.2)
   rstan            2.26.16 2023-02-24 [1] local
   rstantools       2.2.0   2022-04-08 [1] CRAN (R 4.2.2)
   rstudioapi       0.14    2022-08-22 [1] CRAN (R 4.2.2)
   scales           1.2.1   2022-08-20 [1] CRAN (R 4.2.2)
   sessioninfo    * 1.2.2   2021-12-06 [1] CRAN (R 4.2.3)
   shiny            1.7.4   2022-12-15 [1] CRAN (R 4.2.2)
   shinyjs          2.1.0   2021-12-23 [1] CRAN (R 4.2.2)
   shinystan        2.6.0   2022-03-03 [1] CRAN (R 4.2.2)
   shinythemes      1.2.0   2021-01-25 [1] CRAN (R 4.2.2)
   StanHeaders      2.26.16 2023-02-24 [1] local
   stringi          1.7.12  2023-01-11 [1] CRAN (R 4.2.2)
   stringr        * 1.5.0   2022-12-02 [1] CRAN (R 4.2.2)
   tensorA          0.36.2  2020-11-19 [1] CRAN (R 4.2.0)
   threejs          0.3.3   2020-01-21 [1] CRAN (R 4.2.2)
   tibble           3.1.8   2022-07-22 [1] CRAN (R 4.2.2)
   tidyselect       1.2.0   2022-10-10 [1] CRAN (R 4.2.2)
   utf8             1.2.3   2023-01-31 [1] CRAN (R 4.2.2)
   V8               4.2.2   2022-11-03 [1] CRAN (R 4.2.2)
   vctrs            0.5.2   2023-01-23 [1] CRAN (R 4.2.2)
   withr            2.5.0   2022-03-03 [1] CRAN (R 4.2.2)
   xfun             0.37    2023-01-31 [1] CRAN (R 4.2.2)
   xtable           1.8-4   2019-04-21 [1] CRAN (R 4.2.2)
   xts              0.13.0  2023-02-20 [1] CRAN (R 4.2.2)
   yaml             2.3.7   2023-01-23 [1] CRAN (R 4.2.2)
   zoo              1.8-11  2022-09-17 [1] CRAN (R 4.2.2)

 [1] C:/Users/ftich/AppData/Local/R/win-library/4.2
 [2] C:/Program Files/R/R-4.2.2/library

 D ── DLL MD5 mismatch, broken installation.

──────────────────────────────────────────────────────────────────────────────
```
:::
:::

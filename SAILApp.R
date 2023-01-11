rm(list = ls())
setwd("/Users/jonandrews/Downloads/SAILApp")
QBData <- read.csv("QBdata.csv")
RBData <- read.csv("RBdata.csv")
WRTEData <- read.csv("WRTEdata.csv")
library(ggplot2)
library(dplyr)

eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

QBdataframe <- QBData %>%
  filter(Pos == "QB", Yds >= 500) %>%
  group_by(Age, Pos, Yds) %>%
  summarize(YdsPerYr = Yds/Age)

QBPlotYds <- ggplot(QBdataframe, aes(x=Age, y=Yds)) + 
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) + 
  geom_text(x = 30, y = 5200, label = eq(QBdataframe$Age,QBdataframe$Yds), parse = TRUE) + 
  ggtitle("QB Yds by Age")

QBPlotYdsPerYr <- ggplot(QBdataframe, aes(x=Age, y=YdsPerYr)) + 
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 35, y = 210, label = eq(QBdataframe$Age,QBdataframe$YdsPerYr), parse = TRUE) + 
  ggtitle("QB YdsPerYr by Age")



RBdataframe <- RBData %>%
  filter(Pos == "RB", Yds >= 500) %>%
  group_by(Age, Pos, Yds) %>%
  summarize(YdsPerYr = Yds/Age) 

RBPlotYds <- ggplot(RBdataframe, aes(x=Age, y=Yds)) + 
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 27.5, y = 1620, label = eq(RBdataframe$Age,RBdataframe$Yds), parse = TRUE) + 
  ggtitle("RB Yds by Age")

RBPlotYdsPerYr <- ggplot(RBdataframe, aes(x=Age, y=YdsPerYr)) + 
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 27.5, y = 80, label = eq(RBdataframe$Age,RBdataframe$YdsPerYr), parse = TRUE) + 
  ggtitle("RB YdsPerYr by Age")

WRdataframe <- WRTEData %>%
  filter(Pos == "WR", Yds >= 200) %>%
  group_by(Age, Pos, Yds) %>%
  summarize(YdsPerYr = Yds/Age)

WRPlotYds <- ggplot(WRdataframe, aes(x=Age, y=Yds)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 30, y = 1750, label = eq(WRdataframe$Age,WRdataframe$Yds), parse = TRUE) + 
  ggtitle("WR Yds by Age")

WRPlotYdsPerYr <- ggplot(WRdataframe, aes(x=Age, y=YdsPerYr)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 30, y = 62.5, label = eq(WRdataframe$Age,WRdataframe$YdsPerYr), parse = TRUE) +
  ggtitle("WR YdsPerYr by Age")


TEdataframe <- WRTEData %>%
  filter(Pos == "TE", Yds >= 200) %>%
  group_by(Age, Pos, Yds) %>%
  summarize(YdsPerYr = Yds/Age)

TEPlotYds <- ggplot(TEdataframe, aes(x=Age, y=Yds)) + 
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 27, y = 1250, label = eq(TEdataframe$Age,TEdataframe$Yds), parse = TRUE) +
  ggtitle("TE Yards by Age")
  
TEPlotYdsPerYr <- ggplot(TEdataframe, aes(x=Age, y=YdsPerYr)) + 
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +
  geom_text(x = 31, y = 45, label = eq(TEdataframe$Age,TEdataframe$YdsPerYr), parse = TRUE) +
  ggtitle("TE YdsPerYr by Age")
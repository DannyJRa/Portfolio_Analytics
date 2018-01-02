# # Load quotes test test    test      test
library(tidyquant)
library(ggplot2)
data(FANG)
FANG
#############
#### Overwrite FANG
currentPath=getwd()
setwd("C:/OneDrive/PowerBI_test/Portfolio_Slicer/Input")

library(data.table)
Quotes <- fread("C:/OneDrive/PowerBI_test/Portfolio_Slicer/Input/Quotes_detail.csv")
Quotes$date = as.Date(Quotes$date)


#Quotes=as.data.frame(Quotes)
Quotes <- subset(Quotes, symbol == "SIE.DE" | symbol == "CBK.DE" | symbol == "2PP.F")



########               does strange things
##Quotes = filter(Quotes, symbol == c("SIE.DE","CBK.DE"))
#t = subset(Quotes, date == "2007-01-02")
#########


## Option 1: Manual ticker
ticker <- c("GDAXI","SSMI","BIO.DE","ZIL2.DE",
    "SIE.DE","IFX.DE","CBK.DE","2PP.F","BAYN.DE","SDF.DE","KBC.DE")
	# Set name for BRK-A to BRK.A
	setSymbolLookup(GDAXI = list(name = "^GDAXI"))




##Option 2:Load ticker from xlsx

#AllocationCurrent = read.csv("AllocationCurrent.csv", sep="")

#(ticker <- as.vector(AllocationCurrent$Symbol))
ticker <- c("SIE.DE", "CBK.DE", "2PP.F")


setwd(currentPath)
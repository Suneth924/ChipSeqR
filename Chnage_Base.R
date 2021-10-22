##############################################################################################################
#This function change the base of the chromosom accrding to the percentage of the change of the base.       ##
#If precentage of the change of the base is greater than 60 percent this function will assigne a new base.  ##
#Author :- Suneth Jayawardana                                                                               ##
##############################################################################################################


##Import library
#Install Packages

install.packages("data.table")
install.packages("bench")
install.packages("ggbeeswarm")
install.packages("tidyr")

#load required library
library(data.table)
library(dplyr)
library(bench)
library(tidyr)
library(ggbeeswarm)

## Import data into R
newBase <- function(in_file){

d <- read.table("in_file",header = F)

##Naming the coulumns

names(d) <-  c("chromo_region","position","base","coverage","base_A","base_C","base_G","base_T")

##Convert all Base letters to uppercase and replace base column with upper case

d[,"base"]<-toupper(d[,"base"])

## Defining a varibale for each column

p<-copy(d$position)
b<-copy(d$base)
cv<-copy(d$coverage)
bA<-copy(d$base_A)
bC<-copy(d$base_C)
bG<-copy(d$base_G)
bT<-copy(d$base_T)

##Find percentage for each base and add new percentage columns

d[,"percent_A"] <- d[,"base_A"]*100/d[,"coverage"]
d[,"percent_C"] <- d[,"base_C"]*100/d[,"coverage"]
d[,"percent_G"] <- d[,"base_G"]*100/d[,"coverage"]
d[,"percent_T"] <- d[,"base_T"]*100/d[,"coverage"]

## Assign variables to new percentage columns

pA<-copy(d$percent_A)
pC<-copy(d$percent_C)
pG<-copy(d$percent_G)
pT<-copy(d$percent_T)

## Chnage the table d with new table dt by replacing base with more than 60 percent and store in nb variable

ndt<-d%>%mutate(new_Base=case_when(pA>=60 ~"A",pC>=60~"C",pG>=60 ~ "G", pT >= 60 ~ "T"))

## Rename base to old base
setnames(ndt,c("base"),c("old_base"))

##Looking at the data structure

head(ndt)


##Now save file as a txt file with seperate columns

write.table(ndt, file="newBase.txt", sep = "\t",row.names = FALSE)
}







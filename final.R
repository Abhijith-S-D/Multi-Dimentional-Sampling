#This is the final function which takes the input and gives the output in the table form in excell sheets 
source('~/study/cori/week 5/day 1/extract.R')
source('~/study/cori/week 5/day 1/random.R')
source('~/study/cori/week 5/day 1/kmean.R')
source('~/study/cori/week 5/day 1/generate_constrained_variables.R')
library(xlsx)
library(matlab)
final<-function(address)
{
  tic()
  setwd(address)
  l<-extract(paste(address,"input.txt",sep = ""))
  boolcon<-boolconvar(l$constraints,l$variables)
  x<-random(l)
  nv<-length(l$variables)
  col_names<-vector(mode = "character",length = nv)
  for (variable in 1:nv) {
    col_names[variable]<-paste("X",variable,sep = "")
  }
  res<-list()
  for (i in 1:length(x)) {
    temp<-kmean(x[[i]],l$n[i],l$variables,boolcon)
    res[[i]]<-temp
    colnames(res[[i]])<-col_names
  }
  dir.create("output")
  setwd(paste(address,"output",sep = ""))
  for (i in 1:length(res)) {
    write.xlsx(res[[i]],file = paste(i,".xlsx",sep = ""))
  }
  toc()
  return(res)
}
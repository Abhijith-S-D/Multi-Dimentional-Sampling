#triangle package to generate samples withtriangular distributions
library(triangle)
#extract function will take the string containing the address of input.txt and gives out a list of input details 
extract<-function(x)
{
  #The scan method takes all the contents of the txt file in the vector x where each element is a seperate line
  x<-scan(file = x,what = "character",sep = "\n")
  #The variable vector should contain only details about the variables from the text file
  variables<-character(0)
  #The constraints vector should contain only details about the constraints from text file
  constraints<-character(0)
  #The constraints vector should contain only details about the number of samples to be samples from text file
  n<-numeric(0)
  for(i in 1:(length(x)))
  {
    #Scince each line containing details about the variables contain "~" using this to identify the lines containing the details 
    #about the variables and storing the lines in the variables vector
    if(regexpr("~",x[i])>0)
    {
      if(length(variables)==0)
      {
        variables<-x[i]
      }else{
        variables<-c(variables,x[i])
      }
    }
    #Scince each line containing details about the constraints contains "<|>|=|>=|<=|!=" using this to identify the lines containing the details 
    #about the variables and storing the lines in the constraints vector
    if(regexpr("(<|>|=|>=|<=|!=)",x[i])>0)
    {
      if(length(constraints)==0)
      {
        constraints<-x[i]
      }else{
        constraints<-c(constraints,x[i])
      }
    }
    #Scince  line containing details about the number of samples  contains "Number of samples" using this to identify the lines containing the details 
    #about the number of samples and storing the lines in the n vector
    if(regexpr("Number of samples",text = x[i])>0)
    {
      n<-unlist(regmatches(x[i],gregexpr("([0-9]+)",x[i])))
    }
  }
  #Trying to convert the string containing the details about the variables into R statements and storing in the vector distribution
  distribution<-character(0)
  for (i in variables) {
    if(regexpr("Uniform",i,ignore.case = TRUE)>0)
    {
      temp<-paste("runif(1,",gsub("(\\()","",regmatches(i,regexpr("(\\().+(\\))",i))),sep = "")
      distribution<-c(distribution,temp)
    }
    if(regexpr("Normal",i,ignore.case = TRUE)>0)
    {
      temp<-paste("rnorm(1,",gsub("(\\()","",regmatches(i,regexpr("(\\().+(\\))",i))),sep = "")
      distribution<-c(distribution,temp)
    }
    if(regexpr("Weibull",i,ignore.case = TRUE)>0)
    {
      temp<-paste("rweibull(1,",gsub("(\\()","",regmatches(i,regexpr("(\\().+(\\))",i))),sep = "")
      distribution<-c(distribution,temp)
    }
    if(regexpr("Exponential",i,ignore.case = TRUE)>0)
    {
      temp<-paste("rexp(1,",gsub("(\\()","",regmatches(i,regexpr("(\\().+(\\))",i))),sep = "")
      distribution<-c(distribution,temp)
    }
    if(regexpr("Triangular",i,ignore.case = TRUE)>0)
    {
      temp<-paste("rtriangle(1,",gsub("(\\()","",regmatches(i,regexpr("(\\().+(\\))",i))),sep = "")
      distribution<-c(distribution,temp)
    }
  }
  #creating the list of created vectors
  l<-list(variables,constraints,n,distribution)
  #giving the names for created list members
  names(l)<-c("variables","constraints","n","distribution")
  #Scince the "*" symbol might not be used in the constraints, adding this "*" symbol at appropriate places
  l$constraints<-gsub("([0-9]+)(X)","\\1*\\2",gsub("\\s","",l$constraints))
  #converting n from character vector to integer
  l$n<-as.integer(l$n)
  #returning the final list
  return(l)
}
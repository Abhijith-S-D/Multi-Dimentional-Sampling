#This function takes the random variables generated in random.R along with the variable information and the logical vector 
#and use the kmean clustering algorithm to generate the final sample
kmean<-function(x,k,variables,boolcon)
{
  xc<-vector()
  for (i in 1:length(boolcon)) {
    if(boolcon[i])
    {
      xc<-cbind(xc,x[,i])
    }
  }
  clus<-kmeans(xc,k)
  number<-table(clus$cluster)
  res<-matrix(0,nrow = k,ncol = NCOL(x))
  for (i in 1:length(clus$cluster)) {
    temp<-clus$cluster[i]
    res[temp,]<-res[temp,]+x[i,]
  }
  for (i in 1:k) {
    res[i,]<-res[i,]/number[i]
  }
  for (j in 1:length(variables)) {
    if(!boolcon[j])
    {
      temp_var<-variables[j]
      if(regexpr("Uniform",temp_var,ignore.case = TRUE)>0)
      {
        res[,j]<-eval(parse(text = paste("runif(",k,",",gsub("(\\()","",regmatches(temp_var,regexpr("(\\().+(\\))",temp_var))),sep = "")))
      }
      if(regexpr("Normal",temp_var,ignore.case = TRUE)>0)
      {
        res[,j]<-eval(parse(text = paste("rnorm(",k,",",gsub("(\\()","",regmatches(temp_var,regexpr("(\\().+(\\))",temp_var))),sep = "")))
      }
      if(regexpr("Weibull",temp_var,ignore.case = TRUE)>0)
      {
        res[,j]<-eval(parse(text = paste("rweibull(",k,",",gsub("(\\()","",regmatches(temp_var,regexpr("(\\().+(\\))",temp_var))),sep = "")))
      }
      if(regexpr("Exponential",temp_var,ignore.case = TRUE)>0)
      {
        res[,j]<-eval(parse(text = paste("rexp(",k,",",gsub("(\\()","",regmatches(temp_var,regexpr("(\\().+(\\))",temp_var))),sep = "")))
      }
      if(regexpr("Triangular",temp_var,ignore.case = TRUE)>0)
      {
        res[,j]<-eval(parse(text = paste("rtriangle(",k,",",gsub("(\\()","",regmatches(temp_var,regexpr("(\\().+(\\))",temp_var))),sep = "")))
      }
    }
  }
  return(res)
}
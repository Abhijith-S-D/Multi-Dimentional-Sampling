#Out of all the variables some might be constrained while others might not be.
#Hence to distinguish them creating a boolean vector whose ith index tells whether Xi is present in the constraints(TRUE) or not(FALSE)
#THis functions take the constraints vector and the variables vector in the list given as output by extract function and outputs the boolean vector
boolconvar<-function(constraints,variables)
{
  res<-vector(mode = "logical",length = length(variables))
  for (i in 1:length(variables)) {
    flag<-FALSE
    for (j in 1:length(constraints)) {
      if(regexpr(paste("X",i,sep = ""),constraints[j])>0)
      {
        flag<-TRUE
      }
    }
    if(flag)
    {
      res[i]<-flag
    }
  }
  return(res)
}
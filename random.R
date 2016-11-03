#This function takes the list containing the information of the input.txt and generates the random samples of 50 times required number.
source('~/study/cori/week 5/day 1/generate_constrained_variables.R')
random<-function(x)
{
  v<-list()
  for (i in 1:(length(x$n))) {
    temp<-matrix(nrow = (x$n[i]*50),ncol = length(x$variables))
    restricted_var<-boolconvar(x$constraints,x$variables)
    for (j in 1:(x$n[i]*50)) {
      flag<-FALSE
      vect<-vector()
      while (!flag) {
        vect<-vector()
        for (k in 1:length(x$variables)) {
          if(restricted_var[k])
          {
            assign(paste("X",k,sep = ""),eval(parse(text = x$distribution[k])),envir = globalenv())
            vect<-append(vect,eval(parse(text = paste("X",k,sep = ""))))
          }else{
            vect<-append(vect,NA)
          }
        }
        flag2<-TRUE
        for (k in 1:length(x$constraints)) {
          if(!eval(parse(text = x$constraints[k])))
          {
            flag2<-FALSE
          }
        }
        if(flag2)
        {
          flag<-TRUE
          temp[j,]<-vect
        }
      }
    }
    v[[i]]<-temp
  }
  return(v)
}
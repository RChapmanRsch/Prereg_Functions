###Skew & Kurtosis
SkewKurtosis = function(Y, Skew.Formula="G1"){
#ref: https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
  N=length(na.omit(Y))
  s=sd(Y,na.rm=TRUE)
  Ybar=mean(Y,na.rm=TRUE)
  Ytilde=median(Y,na.rm=TRUE)
  Q=as.vector(quantile(Y, na.rm=TRUE))
  SkewOut=NA
  
  if(Skew.Formula == "G1"){SkewOut=sqrt(N*(N-1))/(N-2) * sum(((Y-Ybar)^3)/N, na.rm=TRUE)/s^3}
  if(Skew.Formula == "Sk2"){SkewOut=3*(Ybar-Ytilde)/s}
  if(Skew.Formula == "Galton"){SkewOut=(Q[1]+Q[3]-2*Q[2])/(Q[3]-Q[1])}
  
  return(c(
    "Skew"=SkewOut  ,
    "Kurtosis"= (sum(((Y-Ybar)^4)/N, na.rm=TRUE)/s^4)-3 ))
}

##QQplots <- needs work as a function  
QQplots = function(Data, folder="QQplots/"){
for(VarName in names(Data)){
jpeg(paste0(folders,DataObj,"_",VarName,".jpg"))
  qqnorm(Data[,VarName], pch=1, frame = FALSE)
  qqline(Data[,VarName], col="green", lwd = 2)
dev.off()}}

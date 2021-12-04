# Mean SD (plausible) & Z's < 3.29 | > 3.29
meanSD= function(x,na.rm=TRUE, format="f", width=4, digits=2){return(paste0(
  formatC(mean(x,na.rm=na.rm), width = width, digits=digits, format = "f", flag = "0"),"(",
  formatC(sd(x, na.rm=na.rm), width = width, digits=digits, format = "f", flag = "0"),")"))}

# zRange
zRange=function(x){
z=round(range(scale(x)),2)
paste(z[1],sep=":",z[2])}

# Boxplots
Boxplots=function(Data, folder="Boxplots/", fileprefix=""){
for(VarName in names(Data)){
jpeg(paste0(folder,fileprefix,VarName,".jpg"))
boxplot(Data[,VarName])
dev.off()}}

# Barplots
Barplots=function(Data, folder="Barplots/", fileprefix=""){
for(VarName in names(Data)){
jpeg(paste0(folder,fileprefix,VarName,".jpg"))
barplot(table(Data[,1]), main=names(Data)[1], ylab="count")
dev.off()}}


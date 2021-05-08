# Mean SD (plausible) & Z's < 3.29 | > 3.29
meanSD= function(x,na.rm=TRUE, format="f", width=4, digits=2){return(paste0(
  formatC(mean(x,na.rm=na.rm), width = width, digits=digits, format = "f", flag = "0"),"(",
  formatC(sd(x, na.rm=na.rm), width = width, digits=digits, format = "f", flag = "0"),")"))}

zRange=function(x){
z=round(range(scale(x)),2)
paste(z[1],sep=":",z[2])}

DataList=list(
    "Data"=Data, 
    "PROMIS_Recode_Scored"=PROMIS_Recode_Scored, 
    "FACT_Recode_Scored"=FACT_Recode_Scored,
    "SF36_Recode_Scored"=SF36_Recode_Scored)

sapply(DataList, function(x){
  apply(x, 2, function(y){paste0(meanSD(y),"\n", zRange(y))})})

# Boxplots
for(DataObj in names(DataList)){
for(VarName in names(DataList[[DataObj]])){
jpeg(paste0("boxplots/",DataObj,"_",VarName,".jpg"))
boxplot(DataList[[DataObj]][,VarName])
dev.off()}}

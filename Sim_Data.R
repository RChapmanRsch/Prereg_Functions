
#read in Codebook and set sample size for sim data
Codebook=readRDS("data/Codebook_list.RDS")
SampleSize=2000

#create dataframe of sim data from SampleSize (above) and number of variable codebook
Data=data.frame(matrix(NA, nrow=SampleSize, ncol=length(Codebook)))
names(Data)=names(Codebook)

#Create Sim data based on "values" in codebook list for each variables
for(Var in names(Data)){
  if(!all(is.na(Codebook[[Var]]$Value))){
    Data[,Var]=sample(Codebook[[Var]]$Value,SampleSize,replace = TRUE)}
  else if(Var=="respid"){Data[,Var]=c(1:SampleSize)}
  else {Data[,Var]=sample(1:100, SampleSize, replace = TRUE)}



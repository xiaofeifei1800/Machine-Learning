
setwd("I:/Machine Learning")
# calculate 信息熵

calcent<-function(data){
   nument<-length(data[,1])
   key<-rep("a",nument)
   for(i in 1:nument)
       key[i]<-data[i,length(data)]
   ent<-0
   prob<-table(key)/nument
   for(i in 1:length(prob))
   ent=ent-prob[i]*log(prob[i],2)
   return(ent)
}

test = read.table("mudat.txt", header = T)

calcent(test)

#subset
split_tree<-function(data,variable,value){
result<-data.frame()
for(i in 1:length(data[,1])){
    if(data[i,variable]==value)
          result<-rbind(result,data[i,-variable])
    }
return(result)
}

split_tree(mudat,2,1)

# base entropy seletion
#第一部分：求出一个分类的各种标签；
#第二部分：计算每一次划分的信息熵；
#第三部分：计算最好的信息增益，并返回分类编号
choose<-function(data){
    numvariable<-length(data[1,])-1
    baseent<-calcent(data)
    bestinfogain<-0
    bestvariable<-0
    infogain<-0
    featlist<-c()
    uniquevals<-c()
    for(i in 1:numvariable){
             featlist<-data[,i]
             uniquevals<-unique(featlist)
             newent<-0
             for(j in 1:length(uniquevals)){
                      subset<-split(data,i,uniquevals[j])
                      prob<-length(subset[,1])/length(data[,1])
                      newent<-newent+prob*calcent(subset)
                                           }
             infogain<-baseent-newent
             if(infogain>bestinfogain){
                      bestinfogain<-infogain
                      bestvariable<-i
                                       }
                           }
    return(bestvariable)
}

choose(mudat)

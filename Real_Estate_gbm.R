getwd()
setwd("F:\\DataScientist_R\\Data")
h_train=read.csv("housing_train.csv",stringsAsFactors = F)
h_test=read.csv("housing_test.csv",stringsAsFactors = F)


#Which seller has maximum value transactions? [ sum of price ]
#which council area has maximum variance in the price
#t=h_train %>% select(Price,CouncilArea) %>% group_by(CouncilArea) %>% summarise(avg_p=var(Price,na.rm = T))

library(dplyr)
library(randomForest)
glimpse(h_train)
glimpse(h_test)

h_test$Price=NA

h_train$data='train'
h_test$data='test'

table(h_all$Bathroom)

h_all=rbind(h_train,h_test)

View(h_all)

h_all=h_all %>% select(-Suburb)
h_all=h_all %>% select(-Address)
h_all=h_all %>% select(-SellerG)
h_all=h_all %>% select(-Postcode)

View(h_all)
table(h_all$Suburb)
table(h_train$Address)
table(h_train$Method)

h_all=h_all %>% 
  mutate(Type=ifelse(Type=='h',1,ifelse(Type=='t',2,ifelse(Type=='u',3,Type))))

h_all$Type=as.numeric(h_all$Type)

table(h_all$CouncilArea)

h_all$CouncilArea=ifelse(h_all$CouncilArea=='','Missing',h_all$CouncilArea)

#Dummy Function

create_dummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for (cat in categories) {
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    #name=gsub("/","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
    
  }
  data[,var]=NULL
  return(data)
}

h_all=create_dummies(h_all,"CouncilArea",100)
h_all=create_dummies(h_all,"Method",100)

glimpse(h_all)

#NA values

lapply(h_all, function(x) sum(is.na(x)))

for (col in names(h_all)) {
  if(sum(is.na(h_all[,col]))>0 & !(col%in% c("data","Price")))
  {
    h_all[is.na(h_all[,col]),col]=mean(h_all[h_all$data=="train",col],na.rm = T)
  }
  
}



#separate train and test

h_train=h_all %>% filter(data=="train") %>% select(-data)

h_test=h_all %>% filter(data=="test") %>% select(-data,-Price)

library(gbm)
library(cvTools)

param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}
num_trials=50
my_params=subset_paras(param,num_trials)

myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(gbm,Price~.,
             data =h_train,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(h_train), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  if(score.this<myerror){
    
    print(params)
     
    myerror=score.this
    
    print(myerror)
    
    best_params=params
  }
  print('DONE')
  
   
}
myerror

# Score =212467/myerror 
# Score

train.gbm.final=gbm(Price~.,data=h_train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "gaussian")

test.pred=predict(train.gbm.final,newdata=h_test,n.trees = best_params$n.trees)
write.csv(test.pred,"aditya_anand2_P1_part2.csv",row.names = F)
































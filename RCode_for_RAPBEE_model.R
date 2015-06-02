library(Rwordseg)
library(compiler)
#导入需要引用的包
rm(list=ls())#删除所有原始数据对象                                      
#-------------------------------------- #Part 1 数据导入与预处理--------------------------------------------------
#-----------数据导入--------------
Reviewslist<-read.delim("data/Reviews.txt",header=FALSE,sep="\t",as.is=TRUE,na.strings="NA",encoding="UTF-8")  #读入待处理评论数据
GoodsDetail<-readLines("data/GoodsDetail.txt")                                                                 #读入商品描述数据制作预备属性词典
#-----------数据导入--------------

#-----------词典导入--------------
adv.dic<-read.delim("data/adv.dic.txt",header=TRUE,as.is=TRUE,na.strings="NA",encoding="UTF-8")          #导入程度副词词典
Gen.att.dic<-read.delim("data/att.dic.txt",header=TRUE,as.is=TRUE,na.strings="NA",encoding="UTF-8")      #导入基础属性词典
emo.dic<-read.delim("data/emo.dic.txt",header=TRUE,as.is=TRUE,na.strings="NA",encoding="UTF-8")          #导入情感词典
Pri.dic<-c("没","没有","不","未","甭","勿","休","莫","非")                                                #导入否定词词典
Pun.dic<-c(",","，","。",".","!","?","~",";","'","&")                                                     #Punctuation的缩写


#---------评论数据的格式化处理--------------
names(Reviewslist)<-c("taobao.rank","time.rank","Review.ID","Reviewer.Credit",
                     "Review.content","Review.time","Review.time.min",
                     "Review.eva","Review.vote","Review.wordscount")               #导入评论数据Reviews.txt到对象Reviews中，并重新进行列命名。
Reviewslist$Review.time<-as.Date(Reviewslist$Review.time, format="%Y年%m月%d日")   #格式化Reviews_time
Reviewslist$Review.time.min<-strptime(Reviewslist$Review.time.min,"%H:%M")         #格式化Reviews_time_min
Reviewslist$Review.eva<-factor(Reviewslist$Review.eva, 
                                    ordered=TRUE, 
                                    levels=c("bad","ok","good")) #格式化Review_eva为因子变量
Reviewslist$Review.att.Ai<-""                                    #增加属性数量标示位
insertWords(c(adv.dic$Words,Gen.att.dic$Words,emo.dic$Words))
#---------评论数据的格式化处理--------------

#---------------------------------------------Part 1结束---------------------------------------------


#----------------------------------------#Part 2 主要函数定义------------------------------------------
#2.1（核心处理1）个性化属性词典的构建函数定义：开始
#-----------------------------------------------------------------------------------------------------
#函数的输入：评论序列（x条评论组成的字符向量）、商品描述详情GoodsDetail
#函数的输出：属性词典Rev.att.dic(与评论讨论内容相关，并且随着评论的增长而完善）

GetRev.att.dic<-function(Reviews,GD=GoodsDetail,Gen=Gen.att.dic){
   
#------------------------评论、商品详情语句拼接----------开始-----------
  allReviews<-""
  for(i in 1:length(Reviews)){
    allReviews<-paste(Reviews[i],allReviews,sep="。")  
  }
  allReviews<-paste(allReviews,GD)#对评论序列、商品描述详情进行拼接，转换成为一段语句
#------------------------评论、商品详情语句拼接----------结束-----------

#------------------------分词统计词频并排序--------------开始-----------
  cuttedReviews<-segmentCN(allReviews)#调用分词函数
  result<-list()
  for(i in 1:length(cuttedReviews)){
    temp<-cuttedReviews[i]
    result[[temp]]<-c(result[[temp]],i)
  }
  freqs<-sapply(result,length)
  Rev.att.dic<-names(result[order(freqs,decreasing=TRUE)])
  #统计分词词频，按词频高低对已分词词语进行排序，等待进一步处理
#------------------------分词统计词频并排序--------------结束-----------
  
#------------------------删除单字符无意义词语------------开始-----------
  for(i in 1:length(Rev.att.dic)){
    if(nchar(Rev.att.dic[i])==1)
      Rev.att.dic[i]="killwords"
  } #标记单个字符:替换为“Killwords”标记，用于表示该词为待删除词语，后续整体删除
#------------------------删除单字符无意义词语------------结束-----------
  
#------------------------标记待删除停顿词、情感极性词等其它无意义词语-----开始-----
  Need.del<-readLines("data/stopwords.txt")
  Need.kill<-readLines("data/killwords.txt")
  
  for(i in 1:length(Rev.att.dic)){
      if(sum(Need.del==Rev.att.dic[i]|sum(Gen==Rev.att.dic[i]))>0)
        Rev.att.dic[i]="killwords"#待删除词匹配，并标记
  }

  for(i in 1:length(Need.kill))
      {Rev.att.dic[grep(Need.kill[i],Rev.att.dic)]<-"killwords"
  }
#------------------------标记待删除停顿词、情感极性词等其它无意义词语-----结束-----
    
#------------------------属性词典构建-------------开始------------
  Rev.att.dic<-Rev.att.dic[which(!Rev.att.dic=="killwords")] #删除所有已标记为“Killwords”的字符
  
  for(i in length(Rev.att.dic)*0.3:length(Rev.att.dic))Rev.att.dic[i]="killwords"
  Rev.att.dic<-Rev.att.dic[which(!Rev.att.dic=="killwords")] 
                                                             #删除词频低于处理结果30%的词语，即去剩余词项的50%组成属性词典
#------------------------属性词典构建-------------结束------------
 
  return(Rev.att.dic)#保存属性词典，并返回
}

#-----------------------------------------------------------------------------------------------------
#2.1个性化属性词典的构建函数定义：结束

#2.2（核心处理2）产品相关度评估、情感倾向评估函数定义：开始
#-----------------------------------------------------------------------------------------------------
#函数的输入：评论数据框Reviewslist、各项词典（将Reviewslist赋给Reviews）
#函数的输出：含有属性词汇的数量、各项属性的情感值得分
Att.Emo.eva<-function(Reviews,Rev=Rev.att.dic,Gen=Gen.att.dic,Emo=emo.dic,adv=adv.dic,Pri=Pri.dic,Pun=Pun.dic)
{ 
  Reviews$Review.att.Ai<-as.numeric(Reviews$Review.att.Ai)
  num.Reviews<-length(Reviews$Review.content)
  Reviews<-cbind(Reviews,
                 CS_score=numeric(num.Reviews),
                 LS_score=numeric(num.Reviews),
                 PS_score=numeric(num.Reviews),
                 CU_score=numeric(num.Reviews),
                 DI_score=numeric(num.Reviews),
                 PQ_score=numeric(num.Reviews),
                 PP_score=numeric(num.Reviews),
                 PD_score=numeric(num.Reviews),
                 CR_score=numeric(num.Reviews),
                 RR_score=numeric(num.Reviews),
                 RW_score=numeric(num.Reviews),
                 GF_score=numeric(num.Reviews),
                 GR_score=numeric(num.Reviews),
                 No.define_score=numeric(num.Reviews),
                 tot_score=numeric(num.Reviews))           #扩展原始数据库，为结果数据存储做准备
  
  for(j in 1:num.Reviews){                                 #开始逐一读取各条评论进行计算
    
  acut<-segmentCN(Reviews$Review.content[j],nosymbol=FALSE)#对当前评论进行分词处理，并保留符号
  wordn<-nchar(Reviews$Review.content[j])                  #获取当前评论字符总数
  
#--------------------属性评估、各类词项类别标注-------开始-----------------------------------
  
#--------------------定义数据框，模拟出入栈操作-------开始---------------
  Review_analyze<-data.frame(Gen_Rev_Str=factor(100,levels=
                             c("CS","LS","PS","CU","DI","PQ","PP","PD","CR","RR","RW","GF","GR","No.define","tot","stopLine")),
                             emo_Str=integer(100),
                             emo_loc=integer(100),
                             Pri_Str=integer(100),
                             adv_Str=integer(100))
#--------------------定义数据框，模拟出入栈操作-------结束---------------
  
  loca<-1     #模拟栈指针游走
  emo.loc<-0  #情感倾向词位置标识符
  Rev.A<-0    #Review属性词典特征词数量
  Gen.A<-0    #通用（Gen）属性词典特征词数量
  Same<-0     #重复属性词典特征词数量
  
  for(i in 1:length(acut)){
#------------------遍历词项词典匹配与标注--开始-------------------
  tag.Rev<-0
  tag.Gen<-0
  emo.loc<-emo.loc+nchar(acut[i])

    if(sum(Pun==acut[i])>0)                            #判断当前分词项为断句符
    {
     if(Review_analyze$emo_Str[loca]!=0|(!is.na(Review_analyze$Gen_Rev_Str[loca])))loca<-loca+1   #栈指针递增
     next
    }

    if(sum(Rev==acut[i])>0)                            #判断当前分词项是否存在于Reviews属性词
    {
      if(!is.na(Review_analyze$Gen_Rev_Str[loca]))loca<-loca+1       
                                                       #判断当前项是否为空，执行思想：如果当前属性列已存在属性，则指针向下，新增一行，再执行插入操作
       Rev.A<-Rev.A+1                                  #计数+1
       tag.Rev<-1                                      #更改已查找标识符
       Review_analyze$Gen_Rev_Str[loca]<-"No.define"   #属性类型标注
    }
  
    if(sum(Gen$Words==acut[i])>0)                      #判断当前分词项是否存在于通用属性词典
    { 
       Gen.A<-Gen.A+1                                  #计数+1
       tag.Gen<-1                                      #更改已查找标识符
       Review_analyze$Gen_Rev_Str[loca]<-Gen$Category[Gen$Words==acut[i]]#属性类型标注
    }
    
    if(tag.Rev&tag.Gen)                                #判断属性是否重复计数
    {
       Same<-Same+1                                    #重复计数标识
    }
  
    if(sum(Emo$Words==acut[i])>0)                      #判断当前分词项是否存在于属性情感倾向词典
    {
       if(Review_analyze$emo_Str[loca]!=0)loca<-loca+1 #判断当前项是否为空，执行思想：如果当前情感列已存在情感标注，则指针向下，新增一行，再执行标注
       Review_analyze$emo_Str[loca]<-Emo$Strength[Emo$Words==acut[i]]#情感强度标注
       if(i==1)Review_analyze$emo_loc[loca]<-1
       else Review_analyze$emo_loc[loca]<-emo.loc-nchar(acut[i]) +1          #情感词位置标注
       next
    }
        
    if(sum(adv$Words==acut[i])>0)                      #判断当前分词项是否存在于程度副词词典
    {
       Review_analyze$adv_Str[loca]<-adv$Strength[adv$Words==acut[i]]#程度副词强度标注
       next
    }
     
    if(sum(Pri==acut[i])>0)                            #判断当前分词项是否存在于否定词词典
    {
       Review_analyze$Pri_Str[loca]<-(-1)              #否定词标注
       next
    }           
  }
#------------------遍历词项词典匹配与标注--结束-------------------
  
Review_analyze<-Review_analyze[1:loca,]                #提取已获得的结果子列，评论属性情感倾向结果

N<-length(segmentCN(Reviews$Review.content[j]))
if((Rev.A+Gen.A-Same)==0)Reviews$Review.att.Ai[j]<-1
if(N==(Rev.A+Gen.A-Same))Reviews$Review.att.Ai[j]<-2
else Reviews$Review.att.Ai[j]<-1+(N-(Rev.A+Gen.A-Same))/N #计算属性相关度
#--------------------属性评估、各类词项类别标注-------结束-----------------------------------
  
#------------------结果清洗与处理----------开始-------------------    
Review_analyze$Pri_Str[which(Review_analyze$Pri_Str==0)]=1 #将正常否定标注为1
Review_analyze$adv_Str[which(Review_analyze$adv_Str==0)]=2 #将正常强度标注为2
if(Review_analyze$emo_Str[loca]==0&is.na(Review_analyze$Gen_Rev_Str[loca]))Review_analyze$Gen_Rev_Str[loca]<-"stopLine"

Review_analyze$Gen_Rev_Str[which(is.na(Review_analyze$Gen_Rev_Str))]<-"tot"
Review_analyze$Gen_Rev_Str[which(Review_analyze$Gen_Rev_Str=="No.define")]<-"tot"
Review_analyze$Gen_Rev_Str[which(Review_analyze$Gen_Rev_Str=="GE")]<-"tot"
Review_analyze$Gen_Rev_Str[which(Review_analyze$Gen_Rev_Str=="GR")]<-"tot"

result<-data.frame(Review_analyze$Gen_Rev_Str,            
Review_analyze$emo_Str*Review_analyze$Pri_Str*Review_analyze$adv_Str
                      *(abs(2*Review_analyze$emo_loc-wordn)/wordn+1))     
                      #根据情感词在评论语句中的位置/副词修饰否定词修饰/调整情感特征词的情感强度
  
names(result)<-c("class","score")                                          #中间结果项重命名
store_result<-tapply(result$score,result$class,sum)                        #基于属性类型的因子进行分数统计（加总）
for(k in 12:26) Reviews[j,k]<- store_result[[k-11]]                        #存储处理结果于通过函数传入的Reviews

  }
  Reviews<-Reviews[c(1:22,26)]#筛选有效数据列返回
  return(Reviews)#返回处理结果
}
#-----------------------------------------------------------------------------------------------------
#2.2（核心处理2）产品相关度评估、情感倾向评估函数定义：结束

#2.3模型测试：结论数据清洗，离群点度量
#-----------------------------------------------------------------------------------------------------
CurrentReviewslist_Test<-function(Reviewslist){
  
  Rev.att.dic<-GetRev.att.dic(Reviewslist$Review.content)               #得到当前Reviewslist下的Rev属性词典
  Reviewslist<-Att.Emo.eva(Reviewslist)                                 #当前Reviewslist下的属性情感倾向评估
  for(i in 12:23)Reviewslist[which(is.na(Reviewslist[[i]])),i]<-0       #数据规范化处理
  Reviewslist$tot_score<-Reviewslist$tot_score/Reviewslist$Review.att.Ai
                                                                        #利用属性相关度调整情感属性得分中total项目的评估值
  Reviewslist$tot_score<-apply(Reviewslist[,12:23],1,sum)               #将属性项得分加总到总分项目
  
  for(i in 12:23)Reviewslist[which(Reviewslist[[i]]==0),i]<-NA          #数据规范化处理
  Reviewslist$Show_or_Not<-""                                           #增加是否展示判定列
  Reviewslist$Show_rank<-0                                              #增加展示排位判定列  
  Reviewslist$Show_or_Not[which(Reviewslist$Review.att.Ai==1)]<-'N'     #将属性相关度为零的评论标注为非展示评论(不相关)
  Reviewslist$Show_or_Not[which(Reviewslist$Review.att.Ai!=1)]<-'Y'
  Reviewslist_Yes.att<-Reviewslist[which(Reviewslist$Review.att.Ai!=1),]#将属性相关度的较高的评论存入属性情感待评库Reviewslist_Yse.att
                                                                        #下面开始重点计算Reviewslist_Yes.att库中的产品情感倾向的离群点
  Reviewslist_Yes.att$emo_score_depart<-0
  meanlist<-0
  for(m in 12:22)
    {meanlist[m-11]<-mean(Reviewslist_Yes.att[[m]],na.rm=T)
  names(meanlist)[m-11]<-paste(names(Reviewslist_Yes.att[m]),"mean",sep="-")
    }
  meanlist[12]<-mean(Reviewslist$tot_score,na.rm=T)
  names(meanlist)[12]<-"tot_score-mean"
  
  for(i in 1:length(Reviewslist_Yes.att$Review.ID)){  
Reviewslist_Yes.att$emo_score_depart[i]<-
  sum((Reviewslist_Yes.att[i,12:23]-meanlist)^2/(meanlist*sum(!is.na(Reviewslist_Yes.att[i,12:23]))),na.rm=TRUE)
    
  }  
  
}

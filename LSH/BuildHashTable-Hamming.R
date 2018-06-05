# Notes: Hamming LSH requair data values positive

#【参数声明】
k = 20
l = 10

#[找到最大坐标值C] 
C = 0 # C大写
for(i in (1:nrow(pointSet))){
  if(pointSet[i,6]=='query'){
    next
  } else {
    if(pointSet[i,2] > C){C <- pointSet[i,2]}
    if(pointSet[i,3] > C){C <- pointSet[i,3]}
  }
}

#【建立局部敏感哈希函数簇】
g <- list()
dN <- 2 #代表点的维数
for(i in (1:l)){
  h <- list()
  for(j in (1:k)){
    r <- sample(x = (1:(dN*C)),size = 1)
    h[j] <- list(r)
  }
  g[i] <- list(h) #用例：g[[l]][[k]][1] -> r
}

#【对所有数据点建立哈希索引】2个点验算核对,KL变化观察数据格式变化是否合理
library(hash)
table <- list()
for(i in (1:l)){
  table[i] <- list(hash()) #一共l个散列表的初始化，每个Gi对应一个散列表
}
for(i in (1:nrow(pointSet))){ #i迭代pointSet
  if(pointSet[i,6]=="query"){next}
  for(j in (1:l)){            #j迭代table/g
    x <- c()
    for(m in (1:k)){
      r <- g[[j]][[m]][1]
      cD <- floor(r/(C+1)) #cd代表'被选择的维度',表示r在变量中的具体维度
      sD <- r %% c #sD代表"被选择的位数",表示r在对应维度中的具体位数
      if(sD == 0){sD <- r}
      if(pointSet[i,cD+2] >= sD){
        xm <- 1
      } else {
        xm <- 0
      }
      x <- c(x,xm)
    }
    pointKey <- v2s(x) #向量x降维后作为键，原始点的索引作为值存入哈希表
    if(has.key(key = pointKey,hash = table[[j]])){
      .set(table[[j]], keys=pointKey,values=c(table[[j]][[pointKey]],pointSet[i,1]))
    } else {
      .set(table[[j]], keys=pointKey,values=pointSet[i,1])
    }
  }
}

#【查询/测试】这一块还没改过
rightN <- 0 #查询正确计数器
collisionN <- array(0,c(l,length(querySet))) #用来保存每个table中与查询点碰撞的点的数量
for(i in querySet){
  queryPoint <- pointSet[which(pointSet$index == i),]
  rightAnswer <- queryPoint[1,4]
  collision <- c()
  for(j in (1:l)){            #j迭代table/g
    x <- c()
    for(m in (1:k)){
      r <- g[[j]][[m]][1]
      cD <- floor(r/(C+1)) #cd代表'被选择的维度',表示r在变量中的具体维度
      sD <- r %% c #sD代表"被选择的位数",表示r在对应维度中的具体位数
      if(sD == 0){sD <- r}
      if(queryPoint[1,cD+2] >= sD){
        xm <- 1
      } else {
        xm <- 0
      }
      x <- c(x,xm)
    }
    queryPointKey <- v2s(x)
    # print(queryPointKey) # test code
    if(has.key(key = queryPointKey, hash = table[[j]])){
      collision <- c(collision, table[[j]][[queryPointKey]])
      collisionN[j,ceiling(i/2)] <- collisionN[j,ceiling(i/2)] + length(table[[j]][[queryPointKey]])
    }
  }
  collision <- unique(collision)
  if(rightAnswer %in% collision){
    rightN <- rightN + 1
  }
}
cat("总索引点数：",nrow(pointSet)-length(querySet),"查询数：",length(querySet),"   正确数：",rightN, "    正确率：", rightN/length(querySet)*100,"%\r\n")
collisionN
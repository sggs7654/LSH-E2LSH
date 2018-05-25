#【参数声明】
k = 20
l = 10
r = 50

#【建立局部敏感哈希函数簇】
g <- list()
for(i in (1:l)){
  h <- list()
  for(j in (1:k)){
    a <- rnorm(2)
    b <- runif(1,0,r)
    h[j] <- list(c(a[1],a[2],b))
  }
  g[i] <- list(h) #用例：g[[l]][[k]][1] -> a1, g[[l]][[k]][2] -> a2
}

#【对所有数据点建立哈希索引】
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
      xm <- floor((g[[j]][[m]][1] * pointSet[i,2] + g[[j]][[m]][2] * pointSet[i,3] + g[[j]][[m]][3]) / r) #x=(av+b)/r, 向下取整
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

#【查询/测试】
rightN <- 0 #查询正确计数器
collisionN <- array(0,c(l,length(querySet))) #用来保存每个table中与查询点碰撞的点的数量
for(i in querySet){
  queryPoint <- pointSet[which(pointSet$index == i),]
  rightAnswer <- queryPoint[1,4]
  collision <- c()
  for(j in (1:l)){            #j迭代table/g
    x <- c()
    for(m in (1:k)){
      xm <- floor((g[[j]][[m]][1] * queryPoint[1,2] + g[[j]][[m]][2] * queryPoint[1,3] + g[[j]][[m]][3]) / r)
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
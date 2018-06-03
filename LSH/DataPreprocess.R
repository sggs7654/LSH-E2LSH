#【数据结构声明】
pointIndex = 1 
querySet <- c() #查询点索引集合
pointSet = data.frame(
  index = c(),
  x = c(),
  y = c(),
  nearest = c(),
  R = c(),
  label = c()
)#如果该点是查询点，则nesarest中存放利他最近的点的索引，否则为NA; R为离查询点最近的邻近点点距


#【参数声明】
queryN <- 1     #待生成的查询点总数
noiseN <- 0     #带生成的干扰点总数
d = 3           #邻近点方块间距
c = 3           #cRNN中的距离倍数参数c
boardMin <- 0   #画布尺寸
boardMax <- 10



#产生查询点和邻近点
repeat{
  
  #产生查询点
  
  queryPoint <- c()
  repeat{                           #产生合格的新点x,y坐标
    queryPoint <- floor(runif(n = 2,min = boardMin,max = boardMax))
    if(check(pointSet,querySet,queryPoint[1],queryPoint[2],c)){break}
  }
  newPoint = data.frame(
    index = c(pointIndex),
    x = c(queryPoint[1]),
    y = c(queryPoint[2]),
    nearest = c(NA),
    R = c(NA),
    label = c('query')
  )
  pointSet <- rbind(pointSet,newPoint)
  pointIndex = pointIndex + 1
  
  
  #产生邻近点
  
  repeat{
    px = floor(runif(n = 1,min = queryPoint[1] - d, max = queryPoint[1] + d))
    py = floor(runif(n = 1,min = queryPoint[2] - d, max = queryPoint[2] + d))
    if((px!=queryPoint[1])&&(py!=queryPoint[2]&&check(pointSet,querySet,px,py,c))){break}
  }
  newPoint = data.frame(
    index = c(pointIndex),
    x = c(px),
    y = c(py),
    nearest = c(NA),
    R = c(NA),
    label = c('neighbor')
  )
  pointSet <- rbind(pointSet,newPoint)
  pointSet[pointIndex-1,4] = pointIndex #更新查询点的最邻近点索引
  pointSet[which(pointSet$index==pointIndex-1),5] <- Dist(pointSet,pointIndex-1,pointIndex) #更新查询点与邻近点的间距R
  querySet <- c(querySet,pointIndex-1) #把R已被赋值的查询点加入查询点集
  pointIndex = pointIndex + 1
  #退出判断
  if(pointIndex >= 2*queryN){break}
}

repeat{
  #产生干扰点
  noisePoint <- c()
  repeat{                           #产生合格的新点x,y坐标
    noisePoint <- floor(runif(n = 2,min = boardMin,max = boardMax))
    if(check(pointSet,querySet,noisePoint[1],noisePoint[2],c)){break}
  }
  newPoint = data.frame(
    index = c(pointIndex),
    x = c(noisePoint[1]),
    y = c(noisePoint[2]),
    nearest = c(NA),
    R = c(NA),
    label = c('noise')
  )
  pointSet <- rbind(pointSet,newPoint)
  pointIndex = pointIndex + 1
  
  #退出判断
  if(pointIndex > 2*queryN + noiseN){break}
}


#绘图
library("ggplot2")
# 基函数
ggplot(pointSet, aes(x = x, y = y, colour = label)) +
  # 散点图函数
  geom_point()



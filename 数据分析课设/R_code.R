# Excel剔除ID为841的行

#用pairs函数生成散点图
data1 <- read.csv("/Users/laiying/Downloads/1.2.csv",header = T)
pairs(~ EXPE+QUAL+LOYA+SATI,data=data1)

#箱线图
library(ggplot2)
sp1=boxplot(data1$EXPE,boxwex=0.7,main='EXPE')
sp2=boxplot(data1$QUAL,boxwex=0.7,main='QUAL')
sp3=boxplot(data1$LOYA,boxwex=0.7,main='LOYA')
sp4=boxplot(data1$SATI,boxwex=0.7,main='SATI')

sp5=boxplot(data1$EXPE,data1$QUAL,data1$LOYA,data1$SATI,boxwex=0.7,main='Box',
             names=c('EXPE','QUAL','LOYA','SATI'),col=c("red","yellow","blue",'green'))


#pearson相关系数矩阵
cor_pearson <- cor(data1, method = 'pearson')

data2 <- read.csv("/Users/laiying/")
#距离判别分析的函数为wmd, Trnx是训练样本数据。TrnG为分类结果，Tweight为指定权重，
#可以根据主成分贡献计算或者取相等（原始的判别分析法），Tstx为待测样本数据，
#var.equal指定协方差矩阵是否相等。
#wmd(Trnx,TrnG,Tweight=NULL,Tstx=NULL,var.equal=F)

#####变量聚类
data3 <-read.table("/Users/laiying/Downloads/exercise5_5.txt",,header = T)
#变量2，4
data31 <- data.frame(data3[,3],data3[,5])# 取后X2,X4列
wss <- (nrow(data31)-1)*sum(apply(data31,2,var)) # 计算离均差平方和
for (i in 2:15) wss[i] <- sum(kmeans(data31, 
                                     centers=i)$withinss) #计算不同聚类个数的组内平方和
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") # 绘图
# K-Means聚类分析
fit1 <- kmeans(data31, 3) # 设定聚类个数为3
# 获取聚类均值
aggregate(data31,by=list(fit1$cluster),FUN=mean) # aggregate()是一个分类汇总函数
res <- data.frame(data31, fit1$cluster)

#变量123
data32 <- data3[,2:4]# 取后X2,X4列
wss <- (nrow(data32)-1)*sum(apply(data32,2,var)) # 计算离均差平方和
for (i in 2:15) wss[i] <- sum(kmeans(data32, 
                                     centers=i)$withinss) #计算不同聚类个数的组内平方和
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") # 绘图
# K-Means聚类分析
fit2 <- kmeans(data32, 3) # 设定聚类个数为3
# 获取聚类均值
aggregate(data32,by=list(fit1$cluster),FUN=mean) # aggregate()是一个分类汇总函数
res <- data.frame(data32, fit1$cluster)

#变量1234
data33 <- data3[,2:5]# 取后X2,X4列
wss <- (nrow(data31)-1)*sum(apply(data33,2,var)) # 计算离均差平方和
for (i in 2:15) wss[i] <- sum(kmeans(data33, 
                                     centers=i)$withinss) #计算不同聚类个数的组内平方和
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") # 绘图
# K-Means聚类分析
fit3 <- kmeans(data33, 3) # 设定聚类个数为3
# 获取聚类均值
aggregate(data33,by=list(fit1$cluster),FUN=mean) # aggregate()是一个分类汇总函数
res <- data.frame(data33, fit1$cluster)





# 加载必要的包
library(MASS)

# 定义函数，计算距离判别分类器
distance.classifier <- function(x, mean1, mean2, cov) {
  # 计算距离
  d1 <- sqrt((x - mean1) %*% solve(cov) %*% t(x - mean1))
  d2 <- sqrt((x - mean2) %*% solve(cov) %*% t(x - mean2))
  
  # 判断分类
  if (d1 < d2) {
    return(1)
  } else {
    return(2)
  }
}

# 读入样本数据
data <- read.csv("/Users/laiying/Downloads/zhongliu.csv")

# 分离训练数据和测试数据
train <- data[1:8, ]
test1 <- data[9, ]
test2 <- data[10,]
test3 <- data[11,]

# 计算训练数据的均值和协方差矩阵
mean1 <- colMeans(train[train$class == 1, 1:5])
mean2 <- colMeans(train[train$class == 2, 1:5])
cov <- var(train[, 1:5])

# 对测试数据进行分类
predictions <- apply(test1[, 1:5], 1, distance.classifier, mean1, mean2, cov)
                     

# Load necessary libraries
library(MASS)

# Define the known sample data
sample_data <- matrix(c(13.54, 14.36, 87.46, 566.3, 0.09779,
                        13.08, 15.71, 85.63, 520, 0.1075,
                        9.504, 12.44, 60.34, 273.9, 0.1024,
                        17.99, 10.38, 122.8, 1001, 0.1184,
                        20.57, 17.77, 132.9, 1326, 0.08474,
                        19.69, 21.25, 130, 1203, 0.1096,
                        11.42, 20.38, 77.58, 386.1, 0.1425,
                        20.29, 14.34, 135.1, 1297, 0.1003), nrow = 8, ncol = 5, byrow = TRUE)
sample_data2 <-data.frame(sample_data)
# Define the known sample labels
sample_labels <- c(1, 1, 1, 2, 2, 2, 2, 2)
sample_labels2 <- data.frame((sample_labels))
# Define the unknown sample data
unknown_data <- matrix(c(16.6, 28.08, 108.3, 858.1, 0.08455,
                         20.6, 29.33, 140.1, 1265, 0.1178,
                         7.76, 24.54, 47.92, 181, 0.05263), nrow = 3, ncol = 5, byrow = TRUE)
unknown_data2 <-data.frame(unknown_data)
# Use distance discriminant model with equal covariance matrices
model1 <- lda(sample_labels2 ~ ., data = sample_data2, prior = c(0.375, 0.625))
# Make predictions on unknown samples using the distance discriminant model
predictions1 <- predict(model, unknown_samples2)

# Print the predictions
print(predictions1)




# 建立数据框
data <- data.frame(
  diameter = c(13.54, 13.08, 9.504, 17.99, 20.57, 19.69, 11.42, 20.29),
  texture = c(14.36, 15.71, 12.44, 10.38, 17.77, 21.25, 20.38, 14.34),
  perimeter = c(87.46, 85.63, 60.34, 122.8, 132.9, 130, 77.58, 135.1),
  area = c(566.3, 520, 273.9, 1001, 1326, 1203, 386.1, 1297),
  smoothness = c(0.09779, 0.1075, 0.1024, 0.1184, 0.08474, 0.1096, 0.1425, 0.1003)
)
# 添加标签
data$label <- c("benign", "benign", "benign", "malignant", "malignant", "malignant", "malignant", "malignant")
# 构建协方差矩阵相等的距离判别模型
model1 <- lm(label ~ ., data = data)
# 构建协方差矩阵不等的距离判别模型
model2 <- lm(label ~ ., data = data, weights = 1/var(data[, 1:5]))



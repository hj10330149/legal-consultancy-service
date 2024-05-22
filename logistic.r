data=read.csv('C:/Users/WEIYU/Desktop/政治大學/碩二上/統計諮詢/期末/data_1226_1039.csv')
data=data[,-c(1,2,19)] 
data$判決結果[data$判決結果==2]=0
table(data['判決結果'])
#model1=glm(判決結果~., family = 'binomial',data=data)
#summary(model1)

#train and test data
n=nrow(data)
set.seed(1234)
new=data[sample(n),] #將數據順序重新排列
index=sample(seq_len(n),size = round(0.7*n)) #70%測試 30%驗證
train=data[index,]
test=data[-index,]

#向後篩選
model2 <- glm(判決結果~.,train,family=binomial)
backward.glm = step(model2, 
                    # 這裡可以加下界(lower=null)，也可以不加, 
                    direction="backward")
summary(backward.glm)

#向前篩選
null = glm(判決結果 ~ 1, data = train, family=binomial)  
full = glm(判決結果 ~ ., data = train, family=binomial) # 建立上界，也就是完整的線性迴歸
forward.lm = step(null, 
                  # 從空模型開始，一個一個丟變數，
                  # 最大不會超過完整的線性迴歸
                  # (一定要加上界 upper=full，不可以不加) 
                  scope=list(lower=null, upper=full), 
                  direction="forward")
summary(forward.lm)

#Confusion Matrix
#install.packages('caret')
#install.packages('yardstick')
library(caret)
library(yardstick)
library(ggplot2)
result <- predict(model, newdata = test, type = "response")
truth=as.factor(test$判決結果)
predicted=as.factor(ifelse(result > 0.5, 1, 0))
truth_predicted=data.frame(truth,predicted)
cm=conf_mat(truth_predicted, truth, predicted)
autoplot(cm, type='heatmap')+
  scale_fill_gradient(low='#D6EAF8', high='#2E86C1')
confusionMatrix(cm$table, positive = "1") #整體準確率、敏感度、特異度




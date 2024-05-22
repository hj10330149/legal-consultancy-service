data=read.csv('C:/Users/WEIYU/Desktop/政治大學/碩二上/統計諮詢/期末/data_1226_1039.csv')
data=data[,-c(1,2,9,19)] #若要刪除證據的話要加上9, 藥物14
data$判決結果[data$判決結果==2]=0


#train and test data
n=nrow(data)
set.seed(1234)
new=data[sample(n),] #將數據順序重新排列
index=sample(seq_len(n),size = round(0.7*n)) #70%測試 30%驗證
train=data[index,]
test=data[-index,]

#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages("rattle")
#install.packages('yardstick')
#install.packages('caret')
library(rpart)
library(rpart.plot)
library(rattle)
library(yardstick)
library(caret)
cart.model=rpart( 判決結果 ~ . , method = "class",  data=train) 
#control = rpart.control(minsplit = 10,cp = 0.001,maxdepth = 30) 
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  

#train
pred <- predict(cart.model, newdata=train, type="class")
truth=as.factor(train$判決結果)
predicted=as.factor(pred)
truth_predicted=data.frame(truth,predicted)
cm=conf_mat(truth_predicted, truth, predicted)
autoplot(cm, type='heatmap')+
  scale_fill_gradient(low='#D6EAF8', high='#2E86C1')
#confusionMatrix(cm$table, positive = "1") #整體準確率、敏感度、特異度

#test
pred <- predict(cart.model, newdata=test, type="class")
truth=as.factor(test$判決結果)
predicted=as.factor(pred)
truth_predicted=data.frame(truth,predicted)
cm=conf_mat(truth_predicted, truth, predicted)
autoplot(cm, type='heatmap')+
  scale_fill_gradient(low='#D6EAF8', high='#2E86C1')
#confusionMatrix(cm$table, positive = "1") #整體準確率、敏感度、特異度


#variable.importance
barchart(sort(cart.model$variable.importance), col = "#1E90FF")

#new predict data
data2=read.csv('C:/Users/WEIYU/Desktop/政治大學/碩二上/統計諮詢/期末/data_1226_1231_test.csv')
data2=data2[,-c(1,2,9,19)] #若要刪除證據的話要加上9
data2$判決結果[data2$判決結果==2]=0
table(data2$判決結果)

#預測
pred <- predict(cart.model, newdata=data2, type="class")
truth=as.factor(data2$判決結果)
predicted=as.factor(pred)
truth_predicted=data.frame(truth,predicted)
cm=conf_mat(truth_predicted, truth, predicted)
autoplot(cm, type='heatmap')+
  scale_fill_gradient(low='#D6EAF8', high='#2E86C1')
#confusionMatrix(cm$table, positive = "1") #整體準確率、敏感度、特異度


library(rpart)
library(readxl)
library(rpart.plot)
library(gmodels)

patients <- read_excel("46patients.xls")
patients <- patients[,-1]
colnames(patients)[6] <- "Index"
colnames(patients)[8] <- "Target"
patients <- patients[!is.na(patients$id),]
patients$Anaesthetic <- patients$Anaesthetic > 0


head(patients)   #顯示前六筆資料
dim(patients)[1] #資料筆數
dim(patients)[2] #屬性數


# --- function ---
parseTree <- function( seed, testPercent, splitNum ) {
  cat("條件 : ", seed, testPercent, splitNum, "\n")
  set.seed(seed)
  num_of_data=ceiling(testPercent/100*nrow(patients)) #10%測試資料
  test.index=sample(1:nrow(patients),num_of_data) #隨機抽取10%的測試資料
  patients.testdata=patients[test.index,] #測試
  patients.traindata=patients[-test.index,] #訓練
  
  #下面建立訓練資料的決策樹
  patients.tree = rpart(Target ~.,data=patients.traindata,method="class", minsplit=splitNum, cp=1e-3)

  # ======訓練資料正確率======
  Target.traindata=patients$Target[-test.index]
  train.predict=predict(patients.tree, patients.traindata, type="class")
  #CrossTable(x = Target.traindata, y = train.predict, prop.chisq=FALSE) #畫出CrossTable
  train.corrcet=sum(train.predict==Target.traindata)/length(train.predict)#訓練資料之正確率
  cat("訓練資料正確率",train.corrcet*100,"%\n")
  if ( train.corrcet > best.train.corrcet ) best.train.corrcet <<- train.corrcet; best.train.corrcet.tree <<- patients.tree
  
  # ======測試資料正確======
  Target.testdata=patients$Target[test.index]
  test.predict=predict(patients.tree, patients.testdata, type="class")
  #CrossTable(x = Target.testdata, y = test.predict, prop.chisq=FALSE) #畫出CrossTable
  test.correct=sum(test.predict==Target.testdata)/length(test.predict)#測試資料之正確率
  cat("測試資料正確率",test.correct*100,"%\n")
  if ( test.correct > best.test.correct ) best.test.correct <<- test.correct; best.test.correct.tree <<- patients.tree
  
  
  if ( test.correct+train.corrcet > best.correct & train.corrcet > test.correct ) {
    best.correct <<- test.correct+train.corrcet
    best.tree <<- patients.tree
    best.set <<- c( seed, testPercent, splitNum )
  }
  
  cat("====================================\n")
  
}
# --- function ---


best.train.corrcet <- 0
best.test.correct <- 0
best.correct <- 0
best.set <- c() 

for ( i in 1:100 ) 
  for ( j in 1:8 ) 
    for ( k in 1:10 ) 
      parseTree( i, j*10, k )



patients.tree

summary(patients.tree)


prp(best.tree,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  


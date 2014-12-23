data = read.delim("train.dat",sep="\n", header=F , stringsAsFactor = F)
pixel = strsplit( data[,1] , split = " " ) # pixel value and class 
class = as.numeric( sapply(pixel, "[[", 1) )
class = as.factor(class)

test_data = read.delim("test.dat",sep="\n", header=F , stringsAsFactor = F)
pixel_test = strsplit( test_data[,1] , split = " " )

feature_matrix = list()
feature_test = list()
for( i in 1:14744){
  if(i%%50==0) print(i)
  
  if(i<=nrow(test_data)) {
    feature_t = strsplit( pixel_test[[i]][-1] , split=":" )
    index_t = as.numeric(sapply(feature_t, "[[", 1)) # feature index
    value_t = as.numeric(sapply(feature_t, "[[", 2))
    tmp = rep(0,12810)
    tmp[index_t] = value_t
    tmp[tmp>0] = 1 # 12/22
    feature_test[[i]] = tmp
  }
  
  feature = strsplit( pixel[[i]][-1] , split=":" )
  index = as.numeric(sapply(feature, "[[", 1)) # feature index
  value = as.numeric(sapply(feature, "[[", 2))
  
  
  
  tmp = rep(0,12810)
  tmp[index] = value
  tmp[tmp>0] = 1 # 12/22
  feature_matrix[[i]] = tmp
  
}
feature_matrix = do.call(rbind , feature_matrix) # train data
feature_test = do.call(rbind , feature_test) # test data
# scale
scale_feature_matrix = list() # train
scale_feature_test = list() # test
for(j in 1:610){
  tmp = rowSums(feature_matrix[,(j*21-20):(j*21)])/21
  tmp2 = rowSums(feature_test[,(j*21-20):(j*21)])/21
  scale_feature_matrix[[j]] = tmp
  scale_feature_test[[j]] = tmp2
}
scale_feature_matrix = do.call(cbind , scale_feature_matrix)
scale_feature_test = do.call(cbind , scale_feature_test)

scale_train = subset(scale_feature_matrix , spl==T) # train
scale_test = subset(scale_feature_matrix , spl==F) # validation


library(caTools)
spl = sample.split(class , SplitRatio = 0.8)
train = subset(feature_matrix , spl==T)
test = subset(feature_matrix , spl==F)
train_class = subset(class , spl==T) 
test_class = subset(class , spl==F)

# svm
library(e1071)
svm_model = svm(x=scale_train , y=train_class , kernel="linear")
svm_model = svm(x=scale_train , y=train_class , kernel="polynomial" , degree=2 , gamma = 0.01)
prediction = predict(svm_model , newdata = scale_test)


# result
correction = function(pred , test_class){
	for(i in 22:31){
		test_class[test_class==i] = i-10
		pred[pred==i] = i-10
	}
	a = sum(test_class==pred)
	b = length(pred)
	round(c(a,b ,a/b*100),3)
}

# write
prediction = as.numeric(prediction) -1
write.table(file = "submission.dat" , x = prediction , row.names = F , col.names= F)

# -----------------------------------------------------------------
tmp = matrix(feature_matrix[1,] , nrow=105 , ncol = 122)
# Visualization
heatmap(t(tmp) , Rowv = NA , Colv = NA , scale="none" , revC = 1)

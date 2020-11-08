setwd(dir = "C:/Users/Yazid/Desktop/Data mining")
df0 = read.table('fruit_data.txt', h=TRUE)

df <- df0[sample(nrow(df0)),]

n = nrow(df)

X = df[,1:ncol(df)-1]
y = as.character(df[,ncol(df)])

X_train = X[1:as.integer(0.7*n),]
y_train = y[1:as.integer(0.7*n)]

X_test = X[as.integer(0.7*n +1):n,]
y_test = y[as.integer(0.7*n +1):n]

preds = knn_predict(X_train, y_train, X_test, 5)
accuracy_knn(y_test, preds)
summary(y_test, preds)
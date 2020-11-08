pos_pts = rnorm(n = 100, mean = 1, sd = 5)
neg_pts = rnorm(n = 100, mean = 10, sd = 10)

f1 = seq(from = 0.05, to = 10, by = 0.05)
f2 = c(pos_pts, neg_pts)

labels = c(rep(as.character('positive'),100),rep(as.character('negative'),100))

df0 = data.frame(f1, f2, labels) #shuffle data
df <- df0[sample(nrow(df0)),]

plot(f1,f2)

# 70% de la data pour le training, 30% pour le test
n = length(f1)

X = df[,1:ncol(df)-1]
y = as.character(df[,ncol(df)])

X_train = X[1:as.integer(0.7*n),]
y_train = y[1:as.integer(0.7*n)]

X_test = X[as.integer(0.7*n +1):n,]
y_test = y[as.integer(0.7*n +1):n]

preds = knn_predict(X_train, y_train, X_test, 2)
accuracy_knn(y_test, preds)
summary(y_test, preds)
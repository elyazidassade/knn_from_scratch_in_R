#Euclidean Distance calculation

euclideanDist = function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

#fonction de prédiction par l'algorithme k-nearest neighbors

knn_predict = function(X_train, y_train, X_test, k_value){
  list_labels = c(as.character(unique(y_train)))
  num_labels = length(list_labels)
  pred <- c()  #vecteur prediction vide 
  #boucle-1
  for(i in c(1:nrow(X_test))){   #boucle sur chaque instance de donnée (sur les lignes)
    eu_dist =c()          #creer deux vecteurs vides eu_dist pour les distances et eu_char pour les labels 'positive' ou 'negative'
    eu_char = c()
    counters = rep(c(0),num_labels)
    
    #boucle-2 sur la training data
    for(j in c(1:nrow(X_train))){
      #ajouter la distance euclidienne entre le point i de test_data et tous les points du training set
      eu_dist <- c(eu_dist, euclideanDist(X_test[i,], X_train[j,]))
      
      #ajouter le label du training set in eu_char
      eu_char <- c(eu_char, as.character(y_train[j]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #créer une dataframe pour chaque point i du test_data contenant la distance entre 
                                       #le point i et tous les points j du training set et le label du point j
    
    eu <- eu[order(eu$eu_dist),]       #trier la dataframe eu de la plus petite distance à la plus grande
    eu <- eu[1:k_value,]               #sélectionner les k premières instances
    
    #boucle-3 sur les instances choisies pour détérminer le label majeur
    
    for(k in c(1:nrow(eu))){
      for(i in 1:num_labels){
        if(as.character(eu[k,'eu_char']) == list_labels[i]){
          counters[i] = counters[i] + 1
        }
      }
    }
    
      #comparer le nombre de labels "positive" celui de "negative"
      # si la majorité est "positive" on met "negative" dans le vecteur pred
      
    pred <- c(pred, list_labels[which.max(counters)])
    
  }
  return(pred) #retourner le vecteur pred
}

#metrics for evaluation

#accuracy
accuracy_knn = function(y_test, preds){
  return(length(which(y_test==preds))/length(y_test))
}

#summary
library(MLmetrics)
summary = function(y_test, preds){
  our_classes = c('', unique(y_test))
  l = length(our_classes)
  s = data.frame(c('Precision','Recall','F1-score','Sensitivity'), matrix(NA, nrow=4, ncol=l-1))
  names(s) = our_classes
  
  for(j in c(2:l)){
    s[1,j] = round(Precision(y_test, preds, our_classes[j]),3)
    s[2,j] = round(Recall(y_test, preds, our_classes[j]),3)
    s[3,j] = round(F1_Score(y_test, preds, our_classes[j]),3)
    s[4,j] = round(Sensitivity(preds, y_test,our_classes[j]),3)
    #s[5,j] = round(Specificity(preds, y_test,our_classes[j]),3)
    
  }
  
  return(s)
  }


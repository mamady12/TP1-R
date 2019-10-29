calcul_erreur = function(data_app, data_val, data_test, modele, idx_classe){
	
	# Cette fonction doit renvoyer un vecteur de trois éléments : 
  
	# le nb d'erreurs faites par modele sur jeu d'apprentissage (data_app)
  #reg_app = glm(Classe~., data = data_app, family = binomial(link = 'logit'))
  p_app = predict(modele, data_app, type = 'response')
  predict_app=ifelse(p_app>=0.5,1,0)
  Nbre_err_app=sum( predict_app!=data_app[,idx_classe])
  
	# le nb d'erreurs faites par modele sur jeu de validation (data_val)
  #reg_val = glm(Classe~., data = data_val, family = binomial(link = 'logit'))
  p_val = predict(modele, data_val, type = 'response')
  predict_val=ifelse(p_val>=0.5,1,0)
  Nbre_err_val=sum( predict_val!=data_val[,idx_classe])
	# le nb d'erreurs faites par modele sur jeu de test (data_test)
  #reg_test = glm(Classe~., data = data_test, family = binomial(link = 'logit'))
  p_test = predict(modele, data_test, type = 'response')
  predict_test=ifelse(p_test>=0.5,1,0)
  Nbre_err_test=sum( predict_test!=data_test[,idx_classe])
	# idx_classe correspond à l'indice de la colonne qui contient les classes à prédire
	# Inspirez vous des commandes utilisées auparavant
	
	resultats = data.frame(data_app=c(Nbre_err_app),data_val=c(Nbre_err_val), data_test=c(Nbre_err_test), row.names=c("Nbre d'erreur"))

	return(resultats) 

	}


remplissage = function(data_app,data_val,modele, puissance, coul0, coul1, xmin, xmax,ymin, ymax){

# Cette fonction représente les données d'apprentissage et de validation dans un plan (les données doivent être en 2 dimensions). Les extrémités du plan sont données par xmin, xmax, ymin, ymax. Pour tous les points du plan, on prédit, en utilisant le modèle, quelle est la classe de ce point, et on l'affiche dans le plan avec la couleur associée (coul0 si classe 0, coul1 si classe 1). Le paramètre puissance représente la puissance à laquelle on a élévé les données (par la fonction polynomial). puissance = 1 si on n'a rien fait de particulier 

	
	#dev.new()
  plot(data_app[which(data_app[,ncol(data_app)]==0),1:2], col = coul0, xlim = c(xmin,xmax), ylim = c(ymin,ymax))
	points(data_app[which(data_app[,ncol(data_app)]==1),1:2], col = coul1, xlim = c(xmin,xmax), ylim = c(ymin,ymax))
  if(length(data_val)>0){
	points(data_val[which(data_val[,ncol(data_val)]==1),1:2], col = coul1, pch = 2)
	points(data_val[which(data_val[,ncol(data_val)]==0),1:2], col = coul0, pch = 2)
}
	
	x = seq(xmin,xmax, length.out = 50)
	y = seq(ymin, ymax, length.out = 50)
	
	if(puissance[1]==1){
		for(i in 1:50){
			for(j in 1:50){
				new=as.data.frame(rbind(c(x[i],y[j])))
				colnames(new) = colnames(data_app)[1:(ncol(data_app)-1)]
				p= predict(modele, new, typ = 'response')
				col = ifelse(p>=0.5,1,0) 
				if(col==0){points(x[i],y[j], col = coul0, pch = 3)}
				else{points(x[i],y[j], col = coul1, pch = 3)}
				
				}
			}	
		
	}
	else{

		for(i in 1:50){
			for(j in 1:50){
				new=as.data.frame(rbind(c(x[i],y[j],0)));
				colnames(new) = colnames(data_app)[1:(ncol(data_app))]
				new_p= puissance(new,puissance);
				p = predict(modele,new_p[,1:(ncol(new_p)-1)], typ = 'response')
				col = ifelse(p>=0.5,1,0) 
				if(col==0){points(x[i],y[j], col = coul0, pch = 3)}
				else{points(x[i],y[j], col = coul1, pch = 3)}
				
			}
		}

	}
	
}
puissance = function(data,p){
  
  n = ncol(data)
  out = data[,1:(n-1)]
  for(i in 1:length(p)){
  d2 = data[,1:(n-1)]^p[i];
  out = cbind(out,d2)
  }
  out = cbind(out,data[,n])
  colnames(out) = c(paste("x",c(1:((length(p)+1)*(n-1))),sep=""),colnames(data)[ncol(data)])
  return(out)
  
}

polynomial = function(data, puissance){
	
	# Cette fonction élève les données (les 2 coordonnées x et y) à la puissance p en créant les nouvelles variables : x^p, y^p, x*y^(p-1), x^(p-1)*y, x^2*y^(p-2), x^(p-2)*y^2, etc...
	
	# on crée une matrice à 0 colonnes (pour l instant)
	new_data = matrix(0,nrow(data),0) 
	
	
	for(i in 1:puissance){
		for(j in 0:i){
			new_data = cbind(new_data,data[,1]^(i-j)*data[,2]^(j))
		}
		
	}
	
	# on met la classe en dernière colonne
	new_data = cbind(new_data, data[,ncol(data)])
	new_data[,ncol(new_data)] = new_data[,ncol(new_data)]  -1
	colnames(new_data) = c(paste("V", c(1:(ncol(new_data)-1)), sep=""),colnames(data)[ncol(data)])
	
	return(as.data.frame(new_data))
	
	
	
}



# Fonction d affichage d'une matrice (pour la partie sur la reconnaissance de chiffres)

display<-function(...){
  imageList<-list(...)
  totalWidth<-0
  maxHeight<-0
  for (img in imageList){
    if(is.character(img))
      img<-readPNG(img)
    dimg<-dim(img)
    totalWidth<-totalWidth+dimg[2]
    maxHeight<-max(maxHeight,dimg[1])
  }
  par(mar=c(0,0,0,0))
  plot(c(0,totalWidth),c(0,maxHeight),type="n",asp=1,xaxt="n",yaxt="n",xlab="x",ylab="y")
  offset<-0
  for (img in imageList){
    dimg<-dim(img)
    rasterImage(img,offset,0,offset+dimg[2],dimg[1])
    offset<-offset+dimg[2]
  }
}


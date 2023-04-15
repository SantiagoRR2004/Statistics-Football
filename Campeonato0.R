dd <- read.table("resultados.txt",header=TRUE,sep="\t") # Importamos los datos
knitr::kable(dd[,1:8])

prediction <- function(dd) { # Creamos esta función que calcula los resultados del fin de temporada
    ll <- function(data, n) {    # Creamos esta función que calcula los resultados de una ronda de partidos
      return( sample(x=c(3,1,0),size = 1, replace=TRUE, prob=c(data/n)) ) # n es el número de partidos jugados y data son el número de partidos ganados, empatados y perdidos
    }
    updatedd <- function(data) { # Creamos esta función que actualiza los resultados para cada ronda de partidos
      n <- mean(data[,3]) # n es el número de partidos jugados 
      x <- apply(data[,4:6],MARGIN=1,ll,n) # x es un vector con el número de puntos que se le suma a cada equipo
      data[,2] <- data[,2] + x # Sumamos las puntuaciones
      data[,3] <- data[,3] + 1 # Sumamos uno al número de partidos jugados 
      data[,4] <- data[,4] + as.integer(x==3) # Sumamos uno al número de partidos ganados a quienes ganaron 
      data[,5] <- data[,5] + as.integer(x==1) # Sumamos uno al número de partidos empatados a quienes empataron 
      data[,6] <- data[,6] + as.integer(x==0) # Sumamos uno al número de partidos perdidos a quienes perdieron 
      return (data) # Devolvemos los datos actualizados
    }
    for (i in 1:13) { # Esto actualiza la información de los 13 partidos nuevos
      dd <- updatedd(dd)
    }
    return (order(dd[,2],decreasing = TRUE)) # La función predictión devuelve el orden en el que quedaron los equipos
}


if(file.exists("predictions0.txt")) { # Intenta encontrar si ya se habian hecho los cálculos antes
  pred <- read.table("predictions0.txt" , header = TRUE) # Guarda los datos anteriores
  x0 <- apply(X=pred,MARGIN = 1, FUN= function(x) {mean(rep(1:nrow(dd),times=x))} ) # Establecemos la media de lo que quedó cada equipo
} else {# Si no se han hecho los cálculos anteriormente
  pred <- matrix(0L,nrow = nrow(dd),ncol = nrow(dd)) # Creamos una matriz vacía
  rownames(pred) <- dd[,1] # Ponemos como nombre de las filas el nombre de los equipos
  colnames(pred) <- paste(1:nrow(dd),"º",sep="") # Ponemos como nombre de las columnas la psición de la que acabaron
  x0 <- 1:nrow(dd) # Establecemos la media de lo que quedó cada equipo a cero si no se hizo antes
}

for (i in 1:1000) {# Hacemos la predicción 1000 veces
  for (i in (asplit(cbind(1:nrow(dd),prediction(dd)), MARGIN=1))) { # Esto junta el orden de los equipos y su orden original
    pred[i[1],i[2]] <- pred[i[1],i[2]] + 1 # Se suma 1 a la posición el la que quedó el equipo
  }  
}
x1 <- apply(X=pred,MARGIN = 1, FUN= function(x) {mean(rep(1:nrow(dd),times=x))} ) # Establecemos la media nueva de lo que quedó cada equipo

err = 10**-3 # Establecemos el error máximo que tiene que tener la posición media de cada equipo
while (!all(abs(x1 - x0) < err)) { # Combrobamos si la media de cada equipo es menor que el error
  x0 <- x1 # Las antiguas medias se convierten en las nuevas medias
  for (i in 1:100) {# Hacemos la predicción 100 veces
    for (i in (asplit(cbind(1:nrow(dd),prediction(dd)), MARGIN=1))) { # Hacemos esto para tener el número de fila de cada equipo y la posición en la que quedaron
      pred[i[1],i[2]] <- pred[i[1],i[2]] + 1
    }  
  }
  x1 <- apply(X=pred,MARGIN = 1, FUN= function(x) {mean(rep(1:nrow(dd),times=x))} ) # Calculamos las nuevas medias
}

write.table(pred, file = "predictions0.txt") # Guardamos los resultados en el fichero prediction
1- sum(pred["Celta",(ncol(pred)-3):ncol(pred)])/sum(pred["Celta",]) # Calculamos la probabilidad de que el Celta descienda
sum(pred["Atlético",1:5])/sum(pred["Atlético",]) # Calculamos la probabilidad de que el Atlético ascienda
print(x1)

'
if(file.exists("predictions0.txt")) { # Intenta encontrar si ya se habian haecho los cálculos antes
  pred <- read.table("predictions0.txt", header = TRUE) # Guarda los datos anteriores
} else {# Si no se han hecho los cálculos anteriormente
  pred <- matrix(nrow = 0,ncol = nrow(dd)) # Creamos una matriz vacía
  colnames(pred) <- dd[,1] # Ponemos como nombre de las columnas el nombre de los equipos
}
for (i in 1:1000) {# Hacemos la predicción 1000 veces
  pred <- rbind(pred, prediction(dd))# Añadimos los resultados a pred
}
print(paste("La probabilidad del Atlético",mean(pred[,"Atlético"]<=5))) # Calculamos la probabilidad de que el Atlético quede entre los cinco primeros
print(paste("La probabilidad del Celta",mean(pred[,"Celta"]<=(ncol(pred)-3))))# Calculamos la probabilidad de que el Celta quede entre los tres últimos
write.table(pred, file = "predictions0.txt") # Guardamos los resultados en el fichero prediction
'






#Cargamos funciones
source(file="scripts/functions.r")

#Procesados por días
file = "raws/all_30m.csv"
datos = MycutDateHour(file=file)
#datos = MyCompleteDateHour(datos,debug="true",umbralValorNA = 10, interval = "30 mins",valNA = NA, date = c("2012-10-31","2014-11-01"))
#infoDays = MyGetSerieDateHour(datos,date =  c("2012-10-31","2014-11-01"), interval = "30 mins", valNA = NA, umbralNA = 0, minimalSize = 336 )
#hist(infoWeeks$Size,main="Histograma de series (horas)",xlab = "Tamaño", ylab="Cantidad de series")

interval = "30 mins"
minimalSize = 336


#infoDays = MyGetSerieDateHourV2(all,date =  c("2012-10-31","2014-11-01"), interval = "30 mins", valNA = NA, umbralNA = 0, minimalSize = 336 )



for(nodo in unique(datos$idNodo)){
  print(paste("Cogiendo datos del nodo",nodo))
  partial = datos[datos$idNodo==nodo,]

  start = as.POSIXct("2011/01/01 00:00:00")
  end = as.POSIXct("2015/01/01 00:00:00")

  full <- seq(start,end, by='30 min' )

  print(paste("Completando datos con valores NA",nodo))
  all = data.frame(Fecha=full, Total=with(partial, Total[match(full, Fecha)]), idNodo=nodo,stringsAsFactors = TRUE,row.names = NULL)

  print(paste("Realizando modificación de NA a Ceros",nodo))
#Ahora cambiamos los NA consecutivos por ceros
Max_contaNA = 14
minValue = 0;
pb <- txtProgressBar(min = 0, max = nrow(all),style = 3)
contaNA = 0
isSerie = FALSE
for(i in seq(from = 1,to = nrow(all), by= 1)){
  setTxtProgressBar(pb = pb,value = i)
  if(is.na(all$Total[i])) {
    if(isSerie==TRUE){
    #Ver si lo ponemos mínimo
    if(contaNA < Max_contaNA){
      l = FALSE
      
      for(j in seq(from=i+1,to = i+Max_contaNA)){
        l = l || !is.na(all$Total[j])
        if(l == TRUE) {
          break
        }
      }
      
      if(l==TRUE){
        all$Total[i] = minValue
      }else{
        isSerie=FALSE
      }
      
    }
    }
    contaNA = contaNA +1
  }else{
    contaNA = 0
    isSerie=TRUE
  }
}
close(pb)
plot(all$Total,type="l")
#na.exclude(all[all$Total == 0,])
print(paste("Calculando series",nodo))
#infoDays = MyGetSerieDateHourV2(all,date =  c("2012-10-31","2014-11-01"), interval = "30 mins", valNA = NA, umbralNA = 0, minimalSize = 336 )
#hist(infoDays)

#Calculando series
all$serie <- with(rle(!is.na(all$Total)), rep(seq_along(lengths),lengths))
separado <- split(all[!is.na(all$Total),], all[!is.na(all$Total),"serie"] )

pb <- txtProgressBar(min = 0, max = length(separado),style = 3)
contaPrime = 0
for(i in separado){
  temp = i
  
  setTxtProgressBar(pb = pb,value = contaPrime)
  contaPrime = contaPrime + 1;
  
  conta = length(temp$Total)
  
  
  if(conta > minimalSize ){
    print(conta)
    write(temp[,"Total"],paste(interval,nodo,min(temp[,"Fecha"]),max(temp[,"Fecha"]),conta,min(temp[,"Total"],na.rm = TRUE),max(temp[,"Total"],na.rm = TRUE),".csv",sep = "_"), sep=",",ncolumns = 1)   
    write(as.matrix(t(temp[,c("Fecha","Total")])),paste(interval,nodo,min(temp[,"Fecha"]),max(temp[,"Fecha"]),conta,min(temp[,"Total"],na.rm = TRUE),max(temp[,"Total"],na.rm = TRUE),"TS.csv",sep = "_"), sep=",",ncolumns = 2)
  }
}

}






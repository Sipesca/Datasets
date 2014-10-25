options(scipen=999)

#Script para eliminar los campos no necesarios
 MycutDateHour <-function(file){
  datos = read.csv(file)
  datos = datos[,1:3]
  write.table(datos,file=file, sep=",",append=FALSE,quote=FALSE)
  datos$Fecha <- as.POSIXct(datos$Fecha,"%Y-%m-%d %H:%M:%s")
  return (datos)
 }

#Script para eliminar los campos no necesarios
MycutDate <-function(file){
  datos = read.csv(file)
  datos = datos[,1:3]
  write.table(datos,file=file, sep=",",append=FALSE,quote=FALSE)
  datos$Fecha <- as.Date(datos$Fecha,"%Y-%m-%d")
  return (datos)
}

MySetDateHour <-function(x){
  x$Fecha <- as.POSIXct(datos$Fecha,"%Y-%m-%d %H:%M:%s")
  return (x)
}

MySetDate <-function(x){
  x$Fecha <- as.Date(datos$Fecha,"%Y-%m-%d")
  return (x)
}

MyCompleteDate <- function(x,date=c("2012-12-31","2013-02-01"),interval="days",valNA = NA,valMin = 0, umbralValorNA = 1){
  contaNA = 0;
  contaMIN = 0;
  for(i in seq (from = 1, to = length(unique(x$idNodo))) ){
    s <- seq(as.Date(date[1]),as.Date(date[2]),interval)
    print(paste("Range:",length(s),sep="" ))
    conta = 0
    EmpiezaSerie = FALSE
    
    for(j in seq_along(s) ){    
      if((length(x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(1)]))==0){
        if(EmpiezaSerie){
          #print (paste("Add NA value for",unique(x$idNodo)[i],s[[j]],length(x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,1])), sep = ";\t")  
          
          if(conta<umbralValorNA){
            #Se trata de un fallo puntual, completamos con valor minimo
            print (paste("Add MIN value for",unique(x$idNodo)[i],s[[j]]), sep = ";\t")         
            newRow <- data.frame(Fecha=s[[j]], idNodo=unique(x$idNodo)[i], Total=valMin)
            contaMIN = contaMIN + 1
          }else{
            #El valor anterior era 
            print (paste("Add NA value for",unique(x$idNodo)[i],s[[j]]), sep = ";\t")         
            newRow <- data.frame(Fecha=s[[j]], idNodo=unique(x$idNodo)[i], Total=valNA)
            contaNA = contaNA + 1
          }
          
          conta = conta + 1
          x = rbind(x,newRow)
        }
      }else{
        EmpiezaSerie = TRUE
        conta = 0
        #print (paste(s[[j]],length(datos[datos$idNodo == unique(datos$idNodo)[i] & datos$Fecha == s[[j]] ,1])), sep = ";\t")  
      }
    }
  }
  print("Sorting Dataset...")
  x = x[order(x$Fecha, decreasing = FALSE), ]
  print("Done")
  print(paste("MIN values added:",contaMIN))
  print(paste("NA values added:",contaNA))
  return (x)  
}



MyCompleteDateHour <- function(x,date=c("2012-12-31 00:00:00","2013-02-01 00:00:00"),interval="15 min",valNA = NA,valMin = 0, umbralValorNA = 1){
  contaNA = 0;
  contaMIN = 0;
    for(i in seq (from = 1, to = length(unique(x$idNodo))) ){
      s <- seq(as.POSIXct(date[1]),as.POSIXct(date[2]),interval)
      print(paste("Range:",length(s),sep="" ))
      conta = 0
      EmpiezaSerie = FALSE
      
      for(j in seq_along(s) ){    
        if((length(x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(1)]))==0){
          if(EmpiezaSerie){
            #print (paste("Add NA value for",unique(x$idNodo)[i],s[[j]],length(x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,1])), sep = ";\t")  
            
            if(conta<umbralValorNA){
              #Se trata de un fallo puntual, completamos con valor minimo
              print (paste("Add MIN value for",unique(x$idNodo)[i],s[[j]]), sep = ";\t")         
              newRow <- data.frame(Fecha=s[[j]], idNodo=unique(x$idNodo)[i], Total=valMin)
              contaMIN = contaMIN + 1
            }else{
              #El valor anterior era 
              print (paste("Add NA value for",unique(x$idNodo)[i],s[[j]]), sep = ";\t")         
              newRow <- data.frame(Fecha=s[[j]], idNodo=unique(x$idNodo)[i], Total=valNA)
              contaNA = contaNA + 1
            }
            
            conta = conta + 1
            x = rbind(x,newRow)
          }
        }else{
          EmpiezaSerie = TRUE
          conta = 0
          #print (paste(s[[j]],length(datos[datos$idNodo == unique(datos$idNodo)[i] & datos$Fecha == s[[j]] ,1])), sep = ";\t")  
        }
      }
    }
  print("Sorting Dataset...")
  x = x[order(x$Fecha, decreasing = FALSE), ]
  print("Done")
  print(paste("MIN values added:",contaMIN))
  print(paste("NA values added:",contaNA))
  return (x)  
}


MyGetSerieDate <- function(x,date=c("2012-12-31 00:00:00","2013-02-01 00:00:00"),interval="days", valNA=NA, umbralNA = 0,TS = FALSE,minimalSize = 15){
  for(i in seq (from = 1, to = length(unique(x$idNodo))) ){
    s <- seq(as.Date(date[1]),as.Date(date[2]),interval)
    print(paste("Range:",length(s),sep="\t" ))
    conta = 0
    contaNA = 0
    EmpiezaSerie = FALSE
    temp = data.frame(row.names = c("TS","VALUE"))

    for(j in seq_along(s) ){
      print(j)
      if((length(x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(1)]))==1){
        print(paste("Existe",x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(3)]))
        if(is.na((x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(3)]))){
          print("Es nulo")
          #Valor nulo
          contaNA = contaNA + 1;
          
          if(contaNA >= umbralNA){
            #TERMINA LA SERIE
            #TODO
            
            #MIRAR QUE EXISTAN VALORES EN LA SERIE
            if(conta> minimalSize){
            print(temp)
            temp = temp[order(temp[,1], decreasing = FALSE), ]
            print(paste("SERIE:",max(temp[,1]),min(temp[,1]), conta,sep = "\t"))
            
              if(TS){
                write(temp[,1:2],paste(interval,unique(datos$idNodo)[1],min(temp[,1]),max(temp[,1]),".csv",sep = "-"), sep=";",ncolumns = 2)
              }else{
                write(temp[,2],paste(interval,unique(datos$idNodo)[1],min(temp[,1]),max(temp[,1]),".csv",sep = "-"), sep=";",ncolumns = 1)                
              }
            }
            
            rm(temp)
            conta = 0
            contaNA = 0
            temp = data.frame(row.names = c("TS","VALUE"))
            print(paste("SIZE",length(temp)))
            
          }else{
            conta = conta +1;
            t = data.frame(s[[j]],x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(3)])
            temp = rbind(temp,t)
            print(paste("Temp size:",length(temp[,1])))
          }
          
        }else{
          print("No es nulo lo metemos")
          conta = conta + 1;
          #Cualquier otro valor
          t = data.frame(s[[j]],x[x$idNodo == unique(x$idNodo)[i] & x$Fecha == s[[j]] ,c(3)])
          temp = rbind(temp,t)
          print(paste("Temp size:",length(temp[,1])))
        }
      }
    }
  }
  
  #MIRAR QUE EXISTAN VALORES EN LA SERIE
  if(conta> minimalSize){
    print(temp)
    temp = temp[order(temp[,1], decreasing = FALSE), ]
    print(paste("SERIE:",max(temp[,1]),min(temp[,1]), conta,sep = "\t"))
    
  
      write(as.matrix(t(temp[,1:2])),paste(interval,"-",unique(datos$idNodo)[1],"-",min(temp[,1]),max(temp[,1]),"-TS.csv",sep = ""), sep=";",ncolumns = 2)
    
      write(temp[,2],paste(interval,unique(datos$idNodo)[1],min(temp[,1]),max(temp[,1]),".csv",sep = ""), sep=";",ncolumns = 1)                
    
  }
  
  print("Sorting Dataset...")
  #x = x[order(x$Fecha, decreasing = FALSE), ]
  print("Done")
  print(paste("NA values added:",contaNA))
  #return (x)  
}


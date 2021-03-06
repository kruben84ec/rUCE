#http://francodatascience.com/2015/08/25/funcion-para-datos-no-agrupados/

#Distribuci�n de frecuencias de datos no agrupados

#Funci�n para elaborar tabla de distribuci�n de frecuencias de datos no agrupados

tdfNA <- function(y){  # Los datos deberan estar guardados 
                       # en un vector
                       # Tabla de frecuencias absolutas
                         tabla1 <- as.data.frame(table(y))     

                       # A�adimos frecuencias acumuladas 
                       # y frecuencias relativas
                         tabla1 <- transform(tabla1, 
                                     F.Acum = cumsum(Freq), 
                                        F.R = prop.table(Freq)) 

                       # A�adimos frecuencias relativas 
                       # acumuladas 
                         tabla1 <- transform(tabla1, 
                                      F.R.Acum = cumsum(F.R))      

                       # Multiplicamos las frecuencas
                       # relativas por 100
                         tabla1 <- transform(tabla1,  
                                         FR.Porc = 100*F.R, 
                                        FRA.Porc = 100*F.R.Acum)     

                     tabla1
}


#http://francodatascience.com/2015/08/27/funcin-para-datos-agrupados-4/

#La siguiente funci�n construye una tabla de distribuci�n de frecuencias de datos agrupados

tdfA <- function(y) {# Los datos deben de estar guardados en un vector

                     # Calculamos el n�mero de clases usando la f�rmula de Sturge
                       k <- nclass.Sturges(y)

                     # Formamos los intervalos de clase
                       intervalos <- cut(y, breaks = k)

                     # Tabla de frecuencias absolutas
                       tabla2 <- as.data.frame(table(intervalos))

                     # A�adimos frecuencias acumuladas y frecuencias relativas
                       tabla2 <- transform(tabla2, F.Acum = cumsum(Freq), F.R = prop.table(Freq))

                     # A�adimos frecuencias relativas acumuladas 
                       tabla2 <- transform(tabla2, F.R.Acum = cumsum(F.R))

                     # Multiplicamos las frecuencas relativas por 100
                       tabla2 <- transform(tabla2, FR.Porc = 100*F.R, FRA.Porc = 100*F.R.Acum)

                     # Para obtener los puntos medios de los intervalos de clase, usaremos 
                     # siguiente funci�n obtenida del sitio 
                     # http://www.r-bloggers.com/finding-the-midpoint-when-creating-intervals/
                       midpoints <- function(x, dp=2){
                                                      lower <- as.numeric(gsub(',.*','',gsub('\\(|\\[|\\)|\\]','', x)))
                                                      upper <- as.numeric(gsub('.*,','',gsub('\\(|\\[|\\)|\\]','', x)))
                                                      return(round(lower+(upper-lower)/2, dp))
                                                     }
                     # Agregamos la columna de puntos medios de los intervalos de clase                                 
                       tabla2 <- transform(tabla2, MC = midpoints(intervalos))
                       tabla2
                    }   

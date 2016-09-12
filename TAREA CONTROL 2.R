## EJERCICIO 1
Eje1<- function(a,b){ ##Parametro a secuencia y b numero o caracter
  as.character(a)
  as.character(b)
  a[a==b]= NA ## Cambio de valores de secuencia
  print(a)}

##EJERCICIO 2
Eje2<-function(a,b){ ##Parametros a vector y b entero
  r=0
  for (i in a) {if(a[[i]]==b) r=r+1} ##Uso de contadores para evaluar valor en vector, (tambien posibe con break)
  if(r>0) print("TRUE")
  else print("FALSO")}

##EJERCICIO 3
Eje3<-function(a,b,d){ ##Tres parámetros de ecuacion cuadratica ax^2 + bx + c
  r<- (b*b) - (4*a*d) ##Calculo discriminante
  x1<- (-b + sqrt(r))/(2*a) ##Raiz primera
  x2<- (-b - sqrt(r))/(2*a) ##Raiz segunda
  return(c(x1, x2)) }

##EJERCICIO 4
Eje4<-function(a,b){ ##Parametro a vector y b entero
  r=0
  for (i in a) {if(a[[i]]==b) r=r+1} ## Sumatoria de veces
  cat("El entero aparece",r,"veces en el vector")}

##EJERCICIO 5
Eje5<-function(a){ ##Parametro vector
  ##PROMEDIO
  r=0
  for (i in a) {r=r+a[[i]]} ##Sumatoria de números
  p=r/length(a) ##Calculo Promedio
  s=0
  ##DESVIACIÓN ESTANDAR
  for (i in a) {s=s+(a[[i]]-p)^2} ##Sumatoria cuadrados
  d=sqrt(s/((length(a))-1)) ##Calculo desviación
  cat("El promedio de los valores del vector es",p,", con una desviación estandar de", d)
} ##Tambien posible con funciones mean(x) y sd(x)

##EJERCICIO 6
Eje6<-function(a){ ## Función con parámetro de un numero entero
  i=2
  C=0
  while(i<a){if(a%%i==0) C=C+1; i=i+1} ##Ciclo de calculo de divisores con método del resto
  cat("El número tiene",C,"divisores distintos de 1 y si mismo") ##Forma de imprimir con texto
}
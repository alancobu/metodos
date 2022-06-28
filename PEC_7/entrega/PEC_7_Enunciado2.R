library(polynom)
# Clear the workspace
rm(list = ls())

####### Funciones auxiliares #######

# Importamos la función de lectura de datos


# Funci?n 'myReadData_byDate'
#
#	Lee datos de un fichero csv de nombre 'file'.
#	Devuelve un vector que contiene los datos de la columna especificada por la etiqueta 'column'
#	desde la fecha inicial 'date_ini' hasta la fecha final 'date_fin'.
# 	
# Argumentos:
#	- file: Cadena de caracteres (string) especificando el nombre del fichero.
#	- date_ini: Cadena de caracteres (string) especificando la fecha inicial. Formato dd/mm/aaa.
#	- date_fin: Cadena de caracteres (string) especificando la fecha final. Formato dd/mm/aaa.
#	- column: Cadena de caracteres (string) especificando la columna (etiqueta o cabecera) que se desea leer.
#
# A?adir la siguiente linea para hacer llamadas a esta funci?n desde otros ficheros:
#
#	source('Lectura_datos_por_fecha.R')
#

myReadData_byDate = function(file, date_ini, date_fin, column){
  
  df = read.csv(file, sep = ',', row.names = 1)
  idx_Dates = as.character( seq(as.Date(date_ini, format = '%d/%m/%Y'), as.Date(date_fin, format = '%d/%m/%Y'), 'days') )
  return( na.omit(df[idx_Dates, column]) )
}


############## Ejemplo de uso ##############

# Leyendo datos de casos nuevos desde el 1 de marzo de 2021 al 31 de marzo de 2021 (1 mes).
data = myReadData_byDate('WHO-COVID-19-global-data-SPAIN.csv', '01/03/2021', '31/03/2021', 'New_cases')
data

# Imprimir los primeros y últimos datos
print(head(data))
print(tail(data))


# Función que evalúa los polinomios de Legendre hasta grado 'n' en 'x' (que puede ser un vector)

my_legendre = function(x, n){
	
	m = length(x)
	L = matrix(1, n+1, m)
  if (n > 0)
    for(i in 1:n)
      L[2,] = x# Coeficiente L1

	if (n > 1)
		for (k in 1:(n-1))
		  L[k+2,] = (2*k+1)
	
	return(L)
}

my_legendre(c(-1,0,1),2)

n=3
for(k in 1:n){
  print(p^k)
}

poly.calc(1:-1)

L# Dados los coeficients de Legendre, retorna la función 
# aproximada evaluada en 't' (que puede ser un vector)
myf_Legendre = function(t, ck){

	x = (2*t-(1+length(t)))/length(t)-1
	Lk = my_legendre(x, n)
	
	return(colSums(ck*Lk))
}

# Implementación de la derivación numérica
my_diff = function(f, h){
	return( )
}

# Contrucción de la matriz de los datos evaluados en las funciones base (Phi)
myPhi = function(x, n){
	
	Phi = matrix(1, length(x), n+1)
	for (i in 1:n) {
		Phi[, i+1] = # funciones base para el ajuste polinómico
	}
	return(Phi)
}

# Dados los coeficients del ajuste polinómico, retorna la función aproximada evaluada en 'x'
myeval = function(x, c){
	
	f = 0
	for (i in 1:length(c)) {
		f = f + # Construir la aproximación lineal polinómica
	}
	return(f)
}

####### Ejercicio 1 #######

### Lectura de datos ###
# En 'Y' almacenamos los valores de casos nuevos y en 't' los días
Y = myReadData_byDate('WHO-COVID-19-global-data-SPAIN.csv', '01/12/2020', '31/03/2021', 'New_cases') # Incluir fechas y etiqueta

Y
m = length(Y)
m
t = # Definir la variable que representa a los días
plot(,) # Representar los datos


####### Ejercicio 1 #######
## Cambio de variable
a = min()
b = 
x = # Definir el cambio de variable adecuado a partir de 'a' y 'b'

N = c(2, 4, 6, 8, 10)
color = c('cyan', 'yellow', 'magenta', 'blue', 'red')
for(i in 1:length(N)) {

	n = N[i]
	
	## Polinomios de Legendre
	Lk = my_legendre(x, n)
	Ak = # Términos de ortogonalidad
	fLk = as.vector( Lk%*%Y )
	ckL = (2/(b - a))*fLk/Ak

	## Representación gráfica
	hh = 0.01
	tt = seq(a, b, hh)
	fL = # Evaluar la aproximación con grado 'n' en los puntos 'tt'
	lines(tt, fL, col=color[i], lwd=3)
}


####### Ejercicio 2 #######
n=10

## Ajuste polinómico
lout = lsfit(, ) # Argumentos: Matriz con los datos evaluados en las funciones base (Phi) y lado derecho
ckP = lout[[1]]

## Representación gráfica
hh = 0.01
tt = seq(a, b, hh)
fL = # Evaluar la aproximación mediante Legendre con grado 'n' en los puntos 'tt'
fP = # Evaluar la aproximación mediante ajuste lineal polinómico con grado 'n' en los puntos 'tt'
plot(,) # Representar los datos
lines(tt, fL, col='red', lwd=3)
lines(tt, fP, col='green', lwd=3)

## Cálculo del coeficiente de determinación para Legendre y ajuste polinómico
St = # Discrepancias de los datos con respecto a la media

Sr_L = # Sumatorio de los residuos de Legendre
Sr_P = # Sumatorio de los residuos del ajuste polinómico

r2_L = # Coeficiente de determinación para Legendre
r2_P = # Coeficiente de determinación para el ajuste polinómico
print(r2_L)
print(r2_P)

####### Ejercicio 3 #######
# Calculamos la derivada de la aproximación de Legendre y la representamos
hh = 0.01
tt = seq(a, b, hh)
df = # Calcular la derivada de una de las aproximaciones en los puntos 'tt' mediante derivación numérica
plot( , df, col='red', lwd=3) # Representar la derivada en los puntos anteriores


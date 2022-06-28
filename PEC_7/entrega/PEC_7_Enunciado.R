# Clear the workspace
rm(list = ls())

####### Funciones auxiliares #######

# Importamos la función de lectura de datos
source('Lectura_datos_por_fecha.R')

# Función que evalúa los polinomios de Legendre hasta grado 'n' en 'x' (que puede ser un vector)
nth_diff <- function(n){
  pol <- c(-1,0,1)
  a <- polynomial(pol)^n
  for(k in 1:n){
    a <- deriv(a)
  }
  return(a)
}

my_legendre = function(x, n){
  
  m = length(x)
  #p <- c(-1,0,1)
  L = matrix(1, n+1, m)
  if (n > 0){
    L[2, ] = x# Coeficiente L1
  }
  if (n > 1)
    for (k in 1:(n-1)){
      pol <- as.function(nth_diff(k+1)*(2^(k+1)*factorial(k+1))^-1)
      L[k+2, ] = pol(x)
    }
  return(L)
}

# Dados los coeficients de Legendre, retorna la función aproximada evaluada en 't' (que puede ser un vector)
myf_Legendre = function(t, ck){
	
  x = (2*t-(a+b))/(b-a)
  # Cambio de variables
	Lk = my_legendre(x, n)
	
	return(colSums(ck*Lk))
}




# Implementación de la derivación numérica
my_diff = function(f,h){
  return ((f(x + h) - f(x)) / h)
}

# Contrucción de la matriz de los datos evaluados en las funciones base (Phi)
myPhi = function(x, n){
	
	Phi = matrix(1, length(x), n+1)
	for (i in 1:n) {
		Phi[, i+1] =x # funciones base para el ajuste polinómico
	}
	return(Phi)
}

# Dados los coeficients del ajuste polinómico, retorna la función aproximada evaluada en 'x'
myeval = function(x, c){
	
	f = 0
	for (i in 1:length(c)) {
		f = c(f,c[i])# Construir la aproximación lineal polinómica
	}
	a <- polynomial(f)
	b <- as.
	return(a)
}

####### Ejercicio 1 #######

### Lectura de datos ###
# En 'Y' almacenamos los valores de casos nuevos y en 't' los días
Y = myReadData_byDate('WHO-COVID-19-global-data-SPAIN.csv', '01/12/2020', '31/03/2021', 'New_cases') # Incluir fechas y etiqueta
m = length(Y)
t = 1:m # Definir la variable que representa a los días
plot(t,Y) # Representar los datos


####### Ejercicio 1 #######
## Cambio de variable
a = 1
b = length(Y)
x = (2*t-(a+b))/(b-a) # Definir el cambio de variable adecuado a partir de 'a' y 'b'
#x=0.5*(a+b)+0.5*(b-a)*t
N = c(2,4,6,8,10)
color = c('cyan', 'yellow', 'magenta', 'blue', 'red')
#plot(NA, ylim = c(2000, 20000), xlim = c(0, 200))
for(i in 1:length(N)) {

	n = N[i]
	
	## Polinomios de Legendre
	Lk = my_legendre(x, n)
	Ak <- c()
	for(w in 1:(n+1)){
	  o <- 2/(2*(w-1)+1)
	  Ak <- c(Ak,o)
	}# Términos de ortogonalidad
	fLk = as.vector( Lk%*%Y )
	#ckL = (2/(b - a))*fLk/Ak
	ckL = (2/(b - a))*fLk/Ak
	## Representación gráfica
	hh = 0.01
	tt = seq(a, b, hh)
	fL = myf_Legendre(tt, ckL)# Evaluar la aproximación con grado 'n' en los puntos 'tt'

	lines(tt, fL, col=color[i], lwd=3)
}


####### Ejercicio 2 #######
n=10

## Ajuste polinómico
lout = lsfit(myPhi(x,n),Y) # Argumentos: Matriz con los datos evaluados en las funciones base (Phi) y lado derecho
ckP = lout[[1]]

ckP
## Representación gráfica
hh = 0.01
tt = seq(a, b, hh)
fL = myf_Legendre(tt, ckL)# Evaluar la aproximación mediante Legendre con grado 'n' en los puntos 'tt'
eva = 
fP = myeval(tt,ckP)# Evaluar la aproximación mediante ajuste lineal polinómico con grado 'n' en los puntos 'tt'
plot(t,Y) # Representar los datos
lines(tt, fL, col='red', lwd=1)
lines(tt, fP, col='green', lwd=3)
fp

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
df = my_diff(myf_Legendre(tt, ckL),1)# Calcular la derivada de una de las aproximaciones en los puntos 'tt' mediante derivación numérica
plot( tt, df, col='red', lwd=3) # Representar la derivada en los puntos anteriores





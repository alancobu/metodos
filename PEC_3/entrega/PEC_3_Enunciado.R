## Solucion de sistema de ecuaciones mediante metodos directos

# Resolucion del sistema mediante factorizacion LU
install.packages("pracma")
library(pracma)

#require(pracma)
# Definicion del  sistema Ax = b
A = 
b = 
  
#Factorizacion LU
mlu = lu(A, scheme='ijk') # metodo de Doolittle
L = mlu$
U = mlu$U


# Resolucion del sistema a partir de L y U
y = solve() #Ly = b
x = solve() #Ux = y



#Comprobamos que obtenemos el mismo resultado, resolviendo de forma directa con solve
x_ = solve() #Ax_ = b




## Solucion de sistema de ecuaciones mediante metodos iterativos
# Empleamos el comando de R itersolve(A, b, x0, tol, method), cuyos argumentos son:
# A: la matriz del sistema
# b: el lado derecho del sistema
# x0: aproximacio inicial
# tol: condici?n de parada en base al error cometido
# method: metodo a utilizar ("Gauss-Seidel" o "Jacobi")

#require(pracma)


# Resolucion del sistema mediante Jacobi

n = #dimension del sistema

A = diag()
for(i in 1:n-1) { A[i,i+1]<- }
for(i in :n) { A[i,i-1]<- }

b = rep(,n)
b[]<-

# Primera iteracion
D = diag(diag())
L = -tril(A, )
U = -triu(, 1)

J = #D^{-1}*(L+U)
c = #D^{-1}*b
  
#Calculamos los autovalores de J
lambda <-

#calculamos el radio espectral (el máximo de los valores absolutos de los autovalores)
max_autovalor = max(abs())


# Primera iteracion

x0 = rep(,n)
x1 = 


# Resolucion iterativa
sol = itersolve(A, b, x0, tol =, method = "Jacobi") #solucion con un error de aproximacion maximo
print(sol)
sol1 = itersolve(A, b, x0, nmax = , method = "Jacobi") #Solución con un numero maximo de iteraciones
print(sol1)

#calcular el error relativo


# Resolucion del sistema mediante Gauss-Seidel

n =  #dimension del sistema
  
A = diag()
for(i in 1:n-1) { A[i,i+1]<- }
for(i in :n) { A[i,i-1]<- }

b = rep(,n)
b[]<-



# Primera iteracion
D = diag(diag())
L = -tril(A, )
M = D - L
U = -triu(, 1)

G = #M^{-1}*U
d = #M^{-1}*b
  

x0 = rep(,n)


# Resolucion iterativa
sol = itersolve(A, b, x0,  tol = 1e-6, method = "Gauss-Seidel")#solucion con un error de aproximacion maximo
print(sol)
sol1 = itersolve(A, b, x0, nmax = , method = "Gauss-Seidel")#Solución con un numero maximo de iteraciones
print(sol1)

#calculo del error relativo 


#Comprobamos que obtenemos el mismo resultado, resolviendo de forma directa con solve
x_ = solve(A, b)
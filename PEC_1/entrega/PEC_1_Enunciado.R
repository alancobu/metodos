## PEC 1

#Introducir la matriz L

#Calculo de los valores s1, s2, s3, s4

#Calculamos la traza de las potencias de la matriz L

L2 = L multiplicada por L
L3 = L2 multiplicada por L
L4 = L3 multiplicada por L

s1 = sum(diag())
s2 = sum(diag())
s3 = sum(diag())
s4 = sum(diag())

#Calculamos los valores propios de la matriz L 

lambda <-eigen(L,only.values=TRUE)
s1_1 = sum(lambda[[1]])
s2_1 = sum(lambda[[1]] elevar al cuadrado)
s3_1 = sum(lambda[[1]] elevar al cubo)
s4_1 = sum(lambda[[1]] elevar a la cuarta)

#Calculo de lo coeficientes del polinomio carcateristico

c1 = -s1
c2 = ()*(-s2 - c1*s1)
c3 = ()*(-s3 - c1*s2 - c2*s1)
c4 = ()*(-s4 - c1*s3 - c2*s2 - c3*s1)

#El polinomio obtenido es:
cat("x^4", c1, 'x^3', c2, 'x^2', c3, 'x', c4)
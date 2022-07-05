x1 <- c(-1,0,2,3)
y1 <- c(1,-1,2,2)

polyinterp=function(x,y)
{
  # comprobamos que la longitud de los vectores sea la misma
  if(length(x)!=length(y))
    stop ("La longitud de los vectores x e y debe ser la misma" )
  # calculamos el valor de n que es el grado del polinomio
  # a partir del numero de puntos menos 1
  n=length(x)-1
  # creamos la primera columna de la matriz de vandermonde
  vandermonde=rep(1,length(x))
  # iteramos para ir agregando las columnas siguientes según
  # el grado del polinomio
  for(i in 1:n)
  {
    # la matriz contiene columnas sucesivas de los valores de x
    # elevados a la nth potencia
    xi=x^i
    vandermonde=cbind(vandermonde,xi)
  }
  # resolvemos el sistema de ecuaciones
  beta=solve(vandermonde,y, tol=1e-22)
  # borramos los nombres de las columnas (xi)
  names(beta)=NULL
  # nos retorna los coeficientes del polinomio
  return(beta)
}

polyinterp(x1,y1)


x2 <- c(1,2,5)
y2 <- c(3,12,63)
polyinterp(x2,y2)
install.packages('pracma')
library('pracma')
conv(c(1,-2),c(1,-3),c(1,-4))




fun <- function(x){
  return(2^x-5)
}
simp=function(f,a,b,m)
{ 
  # definiremos los puntos laterales y medio donde evaluaremos la función
  x.ends=seq(a,b,length.out=m+1)
  y.ends=f(x.ends)
  x.mids=(x.ends[2:(m+1)]-x.ends[1:m])/2+x.ends[1:m] 
  y.mids=f(x.mids)
  # de tal forma que el area correspondera a la sumade 4 veces los valores intermedios, 
  # dos veces los valores laterales y solo añadiremos
  # una vez cada valor extremo
  p.area=sum(y.ends[2:(m+1)]+4*y.mids [1: m ]+y.ends[1:m])
  p.area=p.area * abs(b - a) / (6 * m)
  return(p.area)
}


x3 <- c(1,2,4,5,8)
y3 <- fun(x3)





simp(fun,0,1,10)





polinomio_L<-function(x,T){
  
  # Recibe los parámetros:
  #   y : vector con las abscisas
  #   L : un polinomio de Lagrange para un valor de x
  
  
  L=c(0)
  # iniciamos el polinomio para poder utilizarlo en la iteración
  for(i in 1:length(T)){
    L[i]=1
    for(j in 1:length(T)){
      # debemos tener en cuenta la condición de la formula de Lagrange
      # i debe ser diferente de j
      if (i!=j){
        # realizamos el productorio, con cada iteración se añade un factor
        L[i]=L[i]*((x-T[j])/(T[i]-T[j]))
      }
    }
  }
  return(L)
}



p_interpolación<- function(y,L){
  # Recibe los parámetros:
  #   y : vector con las abscisas
  #   L : un polinomio de lagrange para un valor de x
  
  p=0
  for(i in 1:length(y)){
    # realizamos la multiplicación de cada valor de los intereses en la tabla f(x)
    # con los polinomios de Lagrange obtenidos con la función
    p=p+(y[i]*L[i])
  }
  return(p)
}

xl <- c(0,1,2,5,8)
yl <- c(4,-4.65,7.24,9.351,4.381)

L=polinomio_L(6,xl)
xf <- p_interpolación(yl,L)
xf



for(i in xl){
  print(9*sin(4*i)+4*cos(i))
}
9*sin(4*6)+4*cos(6)

horner=function(x,coefs)
  # esta función auxiliar recibe los argumentos:
  #       x     :     puntos a evaluar 
  #       coefs :     los coeficientes del polinomio
{
  y=rep(0,length(x))
  for(i in length(coefs):1)
    y=coefs[i]+x*y
  
  return(y)
}


p= polyinterp(xl,yl)
r1 = horner ( 6,p)
r1


trap=function(f,a,b,m)
{
  # creamos un vector con los subintervalos entre a y b definidos por m paneles que
  # requerira m+1 evaluaciónes del integrando
  x=seq(a,b,length.out=m+1)
  # almacenamos en y los resultados de la función phi evaluada en todos lo x
  y=f(x)
  # realizamos la sumatoria de los valores de tal forma que los valores del medio se sumen
  # dos veces y solo una vez los valores extremos
  p.area=sum((y[2:(m+1)]+y[1:m]))
  # finalmente multiplicamos 
  p.area=p.area*abs(b-a)/(2*m)
  return(p.area)
}

f2 = function(x){1/(1+x)}

seg = 16

n1=c()
for(i in 1:seg){
  n1=c(n1,trap(f2,0,1,i))
}
n2=c()
k=2
for(i in 0:(length(n1)-1)){
  # print((4*n1[i+1]-n1[i])/(4-1))
  a=2**i
  b=2**(i+1)
  if(!is.na(n1[b])){
    n2=c(n2,(4**(k-1)*n1[b]-n1[a])/(4**(k-1)-1))
    
  }
}
n2
n3=c()
k=3

for(i in 1:(length(n2)-1)){
  # print((4*n1[i+1]-n1[i])/(4-1))
  a=i
  b=i+1
  if(!is.na(n2[b])){
    n3=c(n3,(4**(k-1)*n2[b]-n2[a])/(4**(k-1)-1))
    
  }
}
n3
n4=c()
k=4
for(i in 1:(length(n3)-1)){
  # print((4*n1[i+1]-n1[i])/(4-1))
  a=i
  b=i+1
  if(!is.na(n3[b])){
    n4=c(n4,(4**(k-1)*n3[b]-n3[a])/(4**(k-1)-1))
    # print(n3[a])
  }
}
n4
n5=c()
k=5
for(i in 1:(length(n4)-1)){
  # print((4*n1[i+1]-n1[i])/(4-1))
  a=i
  b=i+1
  if(!is.na(n4[b])){
    n5=c(n5,(4**(k-1)*n4[b]-n4[a])/(4**(k-1)-1))
    # print(n3[a])
  }
}
n5














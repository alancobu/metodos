#Tipos de interes
ti1=
ti2=
ti3=
ti4=
ti5=
ti6=
ti7=
ti8=
ti9=
ti10=
  
#Vencimientos a interpolar
T1=
T2=
T3=
T4=

#interpolacion por tramos lineal
#r=m*T+b
linterp=function(x1,y1,x2,y2)
{
  m=(y2 y1) (x2 x1)
  b=y2 m x2
  return(c(b,m))
}

#resultados interpolacion por tramos lineal
p1=linterp(,ti1,,ti2)
r1=p1[[2]]*T1+p1[[1]]

p2=linterp(,ti4,,ti5)
r2=p2[[2]]*T2+p2[[1]]

p3=linterp(,ti8,,ti9)
r3=p3[[2]]*T3+p3[[1]]

p4=linterp(,ti9,,ti10)
r4=p4[[2]]*T4+p4[[1]]

#calculo del polinomio interpolador (Vandermonde)
polyinterp=function(x,y)
{
  if(length(x)!=length(y))
    stop ("La longitud de los vectores x e y debe ser la misma" )
  n=length(x)-1
  vandermonde=rep(1,length(x))
  for(i in 1:n)
  {
    xi=x i
    vandermonde=cbind(vandermonde,xi)
  }
  beta=solve(vandermonde,y)
  names(beta)=NULL
  return(beta)
}

#regla de Horner para evaluar polinomios
horner=function(x,coefs)
{
  y=rep(0,length(x))
  for(i in length(coefs):1)
    y=coefs[i]+x*y
  return(y)
}

#resultados interpolacion mediante el polinomio interpolador
T=c(,,,,,,,,,)
r=c(ti1,ti2,ti3,ti4,ti5,ti6,ti7,ti8,ti9,ti10) 
p=polyinterp(T,r)
r1p=horner(T1,p)
r2p=horner(T2,p)
r3p=horner(T3,p)
r4p=horner(T4,p)

#grafico del polinomio interpolador
x=seq(,,)
y=horner(x,p)
plot(x,y)
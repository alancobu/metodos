#parametros de la opcion
S=
K=
r=
sigma=
T=

#distintas formas de calcular Phi
#formula de Hastings
te=function(x)
{
  alpha=
  return(1/(1+alpha*x))
}

phi=function(x)
{
  return((1/sqrt(2*pi))*exp(-x^2/2))
}

PhiHpos=function(x)
{
  a1=
  a2=
  a3=
  a4=
  a5=
  t=te(x)
  Phitilde=
  return(Phitilde)
}

PhiH=function(x)
{
  if(x>=0) return(PhiHpos(x))
  else return(1-PhiHpos(-x))
}

#regla de los trapecios
trap=function(f,a,b,m)
{
  x=seq(a,b,length.out=m+1)
  y=f(x)
  p.area=sum((y[2:(m+1)]+y[1:m]))
  p.area=p.area*abs()/()
  return(p.area)
}

PhiTpos=function(x)
{
  return(+trap(phi,,x,))
}

PhiT=function(x)
{
  if(x>=0) return(PhiTpos(x))
  else return(1-PhiTpos(-x))
}

#regla de Simpson
simp=function(f,a,b,m)
{
  x.ends=seq(a,b,length.out=m+1)
  y.ends=f(x.ends)
  x.mids=(x.ends[2:(m+1)]-x.ends[1:m])/2+x.ends[1:m] 
  y.mids=f(x.mids)
  p.area=sum(y.ends[2:(m+1)]+4*y.mids [1: m ]+y.ends[1:m])
  p.area=p.area*abs()/()
  return(p.area)
}

PhiSpos=function(x)
{
  return(+simp(phi,,x,))
}

PhiS=function(x)
{
  if(x>=0) return(PhiSpos(x))
  else return(1-PhiSpos(-x))
}

#Monte Carlo
mcint=function(f,a,b,m)
{
  set.seed(5)
  x=runif(m,min=a,max=b)
  y.hat=f(x)
  area=
  return(area)
}

PhiMCpos=function(x)
{
  return(+mcint(phi,,x,))
}

PhiMC=function(x)
{
  if(x>=0) return(PhiMCpos(x))
  else return(1-PhiMCpos(-x))
}

#precio de la call
BScall=function()
{
  d1=(log(S/K)+(r+0.5*sigma^2)*(T))/(sigma*sqrt(T))
  d2=d1-sigma*sqrt(T)
  call=
  return(call)
}

vPhiH=BScall(PhiH) #valor con la Phi de Hastings
vPhiT=BScall(PhiT) #valor con la Phi de trapecios
vPhiS=BScall(PhiS) #valor con la Phi de Simpson
vPhiMC=BScall(PhiMC) #valor con la Phi de Monte Carlo

#errores
abserrT=abs(vPhiT-vPhiH)
relerrT=abserrT/vPhiH

abserrS=abs(vPhiS-vPhiH)
relerrS=abserrS/vPhiH

abserrMC=abs(vPhiMC-vPhiH)
relerrMC=abserrMC/vPhiH









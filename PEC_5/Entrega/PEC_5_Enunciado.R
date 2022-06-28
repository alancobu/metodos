#parametros de la call
K
r
sigma
T

#delta de la call (formula exacta)
BSdeltacall=function(S)
{
  d1
  deltacall
  return(deltacall)
}

#gamma de la call (formula exacta)
BSgammacall=function(S)
{
  d1
  gamma
  return(gamma)
}

#valor exacto de delta
vd=BSdeltacall()

#valor exacto de gamma
vg=BSgammacall()

#precio de la call
BScall=function(S)
{
  d1
  d2
  call
  return(call)
}

#primera derivada numerica (delta)
findiff=function(f,x,h)
{
  return((f()-f())/)
}

#segunda derivada numerica (gamma)
findiff2=function(f,x,h)
{
  return((f()-2*f()+f())/())
}

#valor aproximado de la delta para h=0.1 y errores
vdapprox=findiff(,,)
abserrdh1
relerrdh1

#valor aproximado de la delta para h=0.001 y errores
vdapprox=findiff(,,)
abserrdh2
relerrdh2

#valor aproximado de la gamma para h=0.1 y errores
vgapprox=findiff2(,,)
abserrgh1
relerrgh1

#valor aproximado de la gamma para h=0.01 y errores
vgapprox=findiff2(,,)
abserrgh2
relerrgh2

#representacion error absoluto para delta
vec_h=c(0.1,0.01,0.001,0.0001)
index=c(1,2,3,4)
vdapprox_vech=findiff(,,)
error
plot(index,error,type="b")









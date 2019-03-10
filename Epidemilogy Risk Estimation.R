risk_a =function(df,I,w,t){ 
  Rj=((I)/(df-(w/2)))
  for (i in Rj) {
    Ro= 1-cumprod(1-Rj)
  }
  result2 = data.frame(Desiese_free_pop = df, Incidence=I, Withdraws = w,Rj=Rj, Rto_tj=Ro)
  result2
}
risk_d =function(df,I,w,t,dt){
  pt = (df-((I+w)/2))*t
idj = (I/pt)
Rj = 1-exp(-idj*dt)
for (i in Rj) {
 Ro= 1-exp(-cumsum(idj)*dt)
  }
result1 = data.frame(Desiese_free_pop = df, Incidence=I, Withdraws = w, Population_time = pt, IDj=idj, Rj=Rj, Rto_tj=Ro)
result
}



df = c(13,11,8,3,2)
Inc=c(1,1,2,1,0)
withd = c(1,2,3,0,1)
t = 1

risk_a(df,Inc,withd,1)
risk_d(df,Inc,withd,1,1)

d = c(35,50,60,55)
df1 =d*1000
I1 = c(70,488,2182,4400)
c1 = c(175,1220,5455,11000)










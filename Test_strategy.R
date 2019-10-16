price<-function(i,t){
  if(t==1){
    price<-5-0.02*i
  }else if(t==2){
    price<-4-0.03*i
  }else{
    price<-3-0.04*i
  }
  return(price)
}

Q<-50


days<-100
price_am<-rep(0,days)
price_pm<-rep(0,days)
price_night<-rep(0,days)
profit<-rep(0,days)
for (i in c(1:days)) {
  price_am[i]<-price(Q,1)
  D_am<-rpois(days,(10-2*price_am[i])*5)
  price_pm[i]<-price(max((Q-D_am[i]),0),2)
  D_pm<-rpois(days,(10-2*price_pm[i])*6)
  price_night[i]<-price(max((Q-D_am[i]-D_pm[i]),0),3)
  D_night<-rpois(days,10-2*price_night[i])
  profit[i]<-ifelse(Q<D_am[i],Q,D_am[i])*price_am[i]+ifelse(Q<D_am[i]+D_pm[i],Q-D_am[i],D_pm[i])*price_pm[i]+ifelse(Q<D_am[i]+D_pm[i]+D_night[i],Q-D_am[i]-D_pm[i],D_night[i])+max(Q-D_am[i]-D_pm[i]-D_night[i],0)*0.5-Q
}
print(mean(profit))


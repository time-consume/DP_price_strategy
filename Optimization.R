#Search range
start<-2.8
end<-3.2
interval<-0.1

#Global parameter
ps=0.5
qn<-c(2:8)
qa<-c(25:40)
qm<-c(45:65)

##############################################################################
#Night function
myfun1<-function(start,end,interval){
  x<-c()
  y<-c()
  z<-c()
  for (p in seq(start,end,interval)) {
    all<-matrix(0,nrow = 100,ncol=length(qn))
    for (k in 1:100) {
      n<-rpois(100,10-2*p)
      profit<-matrix(0,nrow=length(qn),ncol=100)
      for (i in 1:length(qn)) {
        for (j in 1:100) {
          if(qn[i]<n[j]){
            profit[i,j]<-p*qn[i]-1*qn[i]
          }else{
            profit[i,j]<-p*n[j]+ps*(qn[i]-n[j])-1*qn[i]
          }
        }
      }
      profit<-t(profit)
      all[k,]<-apply(profit, 2, mean)
    }
    level<-apply(all, 2, mean)
    x<-append(x,p)
    y<-append(y,qn[which.max(level)])
    z<-append(z,max(level))
    output<-data.frame(x,y,z)
  }
  colnames(output)<-c("price","order_qnuantity","profit")
  return(output)
}

#return price and order qnuantity for night
q1<-myfun1(start,end,interval)
#View(q1)
#Price for the night
pn<-q1[which.max(q1$profit),1]
##############################################################################
#Afternoon function
myfun2<-function(start,end,interval){
  x<-c()
  y<-c()
  z<-c()
  for (pa in seq(start,end,interval)) {
    all<-matrix(0,nrow = 100,ncol=length(qa))
    for (k in 1:100) {
      afternoon<-rpois(100,(10-2*pa)*6)
      night<-rpois(100,(10-2*pn))
      profit<-matrix(0,nrow=length(qa),ncol=100)
      for (i in c(1:length(qa))) {
        for (j in 1:100) {
          if(qa[i]<afternoon[j]){
            profit[i,j]<-pa*qa[i]-1*qa[i]
          }else if(qa[i]<afternoon[j]+night[j]){
            profit[i,j]<-pa*afternoon[j]+pn*(qa[i]-afternoon[j])-1*qa[i]
          }else{
            profit[i,j]<-pa*afternoon[j]+pn*night[j]+ps*(qa[i]-afternoon[j]-night[j])-1*qa[i]
          }
        }
      }
      profit<-t(profit)
      all[k,]<-apply(profit, 2, mean)
    }
    level<-apply(all, 2, mean)
    x<-append(x,pa)
    y<-append(y,qa[which.max(level)])
    z<-append(z,max(level))
    output<-data.frame(x,y,z)
  }
  colnames(output)<-c("price","order_quantity","profit")
  return(output)
}
#This is the result
q2<-myfun2(start,end,interval)
#View(q2)
#Price for the afternoon
pa<-q2[which.max(q2$profit),1]
##############################################################################
#Morning function
myfun3<-function(start,end,interval){
  x<-c()
  y<-c()
  z<-c()
  for (pm in seq(start,end,interval)) {
    all<-matrix(0,nrow = 100,ncol=length(qm))
    for (k in 1:100) {
      morning<-rpois(100,(10-2*pm)*5)
      afternoon<-rpois(100,(10-2*pa)*6)
      night<-rpois(100,(10-2*pn))
      profit<-matrix(0,nrow=length(qm),ncol=100)
      for (i in c(1:length(qm))) {
        for (j in 1:100) {
          if(qm[i]<morning[j]){
            profit[i,j]<-pm*qm[i]-1*qm[i]
          }else if(qm[i]<morning[j]+afternoon[j]){
            profit[i,j]<-pm*morning[j]+pa*(qm[i]-morning[j])-1*qm[i]
          }else if(qm[i]<morning[j]+afternoon[j]+night[j]){
            profit[i,j]<-pm*morning[j]+pa*afternoon[j]+pn*(qm[i]-morning[j]-afternoon[j])-1*qm[i]
          }else{
            profit[i,j]<-pm*morning[j]+pa*afternoon[j]+pn*night[j]+ps*(qm[i]-morning[j]-afternoon[j]-night[j])-1*qm[i]
          }
        }
      }
      profit<-t(profit)
      all[k,]<-apply(profit, 2, mean)
    }
    level<-apply(all, 2, mean)
    x<-append(x,pm)
    y<-append(y,qm[which.max(level)])
    z<-append(z,max(level))
    output<-data.frame(x,y,z)
  }
  colnames(output)<-c("price","order_quantity","profit")
  return(output)
}
#This is the result
q3<-myfun3(start,end,interval)
#View(q3)
#Price for the morning
pm<-q3[which.max(q3$profit),1]

##############################################################################
#ouput 
output<-data.frame(t(q1[which.max(q1$profit),]),t(q2[which.max(q2$profit),]),t(q3[which.max(q3$profit),]))
colnames(output)<-c("night","afternoon","morning")
output<-output[,order(-output[3,])]
#View(output)

#fancy
fancy<-data.frame(q1,q2$order_quantity,q2$profit,q3$order_quantity,q3$profit)
colnames(fancy)<-c("price","n_q","n_p","a_q","a_p","m_q","m_p")
#View(fancy)
##############################################################################
#INVENTORY ADJUSTMENT
#Inventory adjustment for night
ts1<-function(){
  p<-seq(1,5,0.1)
  days<-100
  ps<-0.5
  q<-c(1:25)
  all<-c(1:100)
  inventory<-c(1:length(q))
  
  find<-function(q){
    for (w in 1:100) {
      profit<-matrix(0,nrow = length(p),ncol = (days))
      for (i in 1:length(p)) {
        night<-rpois(days,10-2*p[i])
        for (j in 1:days) {
          if(q<night[j]){
            profit[i,j]<-q*p[i]-q
          }else{
            profit[i,j]<-night[j]*p[i]+(q-night[j])*ps-q
          }
        }
      }
      mean<-rowMeans(profit)
      which.max(mean)
      p[which.max(mean)]
      all[w]<-p[which.max(mean)]
    }
    return(mean(all))
  }
  
  
  for (k in 1:length(q)) {
    inventory[k]<-find(q[k])
  }
  output<-data.frame(inventory,q)
  colnames(output)<-c("price","inventory")
  return(output)
  
}
inventory1<-ts1()

#Inventory adjustment for afternoon
ts2<-function(){
  p<-seq(1,5,0.1)
  days<-100
  ps<-0.5
  q<-c(20:50)
  all<-c(1:100)
  inventory<-c(1:length(q))
  
  find<-function(q){
    for (w in 1:100) {
      profit<-matrix(0,nrow = length(p),ncol = (days))
      for (i in 1:length(p)) {
        afternoon<-rpois(days,(10-2*p[i])*6)
        night<-rpois(days,10-2*pn)
        for (j in 1:days) {
          if(q<afternoon[j]){
            profit[i,j]<-q*p[i]-q
          }else if(q<afternoon[j]+night[j]){
            profit[i,j]<-p[i]*afternoon[j]+pn*(q-afternoon[j])-q
          }else{
            profit[i,j]<-p[i]*afternoon[j]+pn*night[j]+ps*(q-afternoon[j]-night[j])-q
          }
        }
      }
      mean<-rowMeans(profit)
      which.max(mean)
      p[which.max(mean)]
      all[w]<-p[which.max(mean)]
    }
    return(mean(all))
  }
  
  
  for (k in 1:length(q)) {
    inventory[k]<-find(q[k])
  }
  output<-data.frame(inventory,q)
  colnames(output)<-c("price","inventory")
  return(output)
  
}
inventory2<-ts2()

#Inventory adjustment for morning
ts3<-function(){
  p<-seq(1,5,0.1)
  days<-100
  ps<-0.5
  q<-c(40:70)
  all<-c(1:100)
  inventory<-c(1:length(q))
  
  find<-function(q){
    for (w in 1:100) {
      profit<-matrix(0,nrow = length(p),ncol = (days))
      for (i in 1:length(p)){
        morning<-rpois(days,(10-2*p[i])*5)
        afternoon<-rpois(days,(10-2*pa)*6)
        night<-rpois(days,10-2*pn)
        for (j in 1:days) {
          if(q<morning[j]){
            profit[i,j]<-q*p[i]-q
          }else if(q<afternoon[j]+morning[j]){
            profit[i,j]<-p[i]*morning[j]+pa*(q-morning[j])-q
          }else if(q<afternoon[j]+morning[j]+night[j]){
            profit[i,j]<-p[i]*morning[j]+pa*afternoon[j]+pn*(q-morning[j]-afternoon[j])-q
          }else{
            profit[i,j]<-p[i]*morning[j]+pa*afternoon[j]+pn*night[j]+ps*(q-morning[j]-afternoon[j]-night[j])-q
          }
        }
      }
      mean<-rowMeans(profit)
      which.max(mean)
      p[which.max(mean)]
      all[w]<-p[which.max(mean)]
    }
    return(mean(all))
  }
  
  
  for (k in 1:length(q)) {
    inventory[k]<-find(q[k])
  }
  output<-data.frame(inventory,q)
  colnames(output)<-c("price","inventory")
  return(output)
}
inventory3<-ts3()

write.csv(inventory1,"night.csv")
write.csv(inventory2,"afternoon.csv")
write.csv(inventory3,"morning.csv")


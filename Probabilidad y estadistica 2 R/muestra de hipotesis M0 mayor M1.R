rm(list = ls())
#H0:mu=70
#Ha:mu>70
zcal=(71.8-70)/(8.9/sqrt(100))
zcal
#Para el P-valor
p_value=pnorm(zcal,lower.tail = F)
p_value
#region de rechazo
zalpha=qnorm(0.05,lower.tail = F)
zalpha
alpha=0.05
if( p_value<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(zcal>=zalpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}


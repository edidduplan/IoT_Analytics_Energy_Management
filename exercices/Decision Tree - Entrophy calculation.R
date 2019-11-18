# Calculating Entrophy
f_entrophy <- function(p,q){
if (p == 0 | q == 0){
  0
} else{
  -p/(p+q)*log(p/(p+q),2) - q/(p+q)*log(q/(p+q),2)
}
}

f_entrophy(2,1)
e_ave <- function(e1, e2, e3, ob1, ob2, ob3){
  (ob1*e1 + ob2*e2 + ob3*e3)/(ob1 + ob2 + ob3)
}

e_ave(0, 1, 0, 2, 2, 1)

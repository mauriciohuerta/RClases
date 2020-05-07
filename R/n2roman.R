n2roman <- function(n){
  if(!is.numeric(n)){
    stop("El argumento n debe ser numerico.")
  }
  if(length(n)!=1){
    stop("El argumento n debe ser unico")
  }
  if((n>=4000)|(n<=0)){
    stop("El argumnento n debe estar entre 1 y 3999")
  }
  m <- floor(n/1000)
  c <- floor((n-(m*1000))/100)
  d <- floor((n-(m*1000)-(c*100))/10)
  u <- n%%10
  mRoman <- MultiGsub(0:3, c("","M","MM","MMM"), m)
  cRoman <- MultiGsub(0:9, c("","C","CC","CCC","CD","D","DC","DCC","DCCC","CM"), c)
  dRoman <- MultiGsub(0:9, c("","X","XX","XXX","XL","L","LX","LXX","LXXX","XC"), d)
  uRoman <- MultiGsub(0:9, c("","I","II","III","IV","V","VI","VII","VIII","IX"), u)
  Roman <- paste(mRoman, cRoman, dRoman, uRoman, sep="")
  return(Roman)
}

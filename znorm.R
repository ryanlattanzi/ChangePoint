##modified l0 norm


znorm=function(tau){
  l0=0;  if(tau==-Inf| tau==Inf){l0=0}
  if(is.finite(tau)==T){l0=1}
  return(l0)
}

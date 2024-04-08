##########################################################################################
#
# PROGRAM: This function is usefull to fit rayleigh regression
#
##########################################################################################


"rr.fit" <- function(x,y,diag=1){
  
  
  rr<-function(mu) 
  {
    n<-length(mu)
    
    u<- runif(n)
    y<- 2*mu*sqrt(-log(1-u)/pi) 
    
    y
  }
  
  qr<-function(alpha,mu=1)
  {
    q<- 2*mu*sqrt((-log(1-alpha))/pi)
    q
  }
  
  dr<-function(x,mu=1) 
  {
    d<- pi*x/(2*mu^2)*exp(-(pi*x^2)/(4*mu^2))
    d
  }
  
  pr<-function(x,mu=1) 
  {
    p<- 1- exp((-pi*x^2)/(4*mu^2))
    p
  }
  
  x <- as.matrix(x)
  x1 <- cbind(rep(1,length(y)),x)
  y <- as.vector(y)
  
  
  r <- ncol(x1)
  n <- length(y)
  
  eta1<-c()
  
  loglik <- function(beta)
  {
    eta1 <- as.vector(x1%*%beta)
    
    mu <- exp(eta1) 
    
    ll <- sum(
      log(pi/2)+log(y)-log(mu^2)-(pi*y^2)/(4*(mu^2))
    )
    ll
  }
  
  escore <- function(beta)
  {
    eta1 <- as.vector(x1%*%beta)
    
    mu <- exp(eta1) 
    
    T = diag(mu) 
    
    v <- (pi*y^2)/(2*mu^3)-(2)/(mu)
    
    Ubeta  = t(x1) %*% T %*% v
    Ubeta
  }
  
  
  ystar = log( y )
  betaols<- (lm.fit(x1, ystar))$coefficients
  
  ini <- betaols
  
  opt <- optim(ini, loglik, escore,
               method = "BFGS", control = list(fnscale = -1, maxit = 500, reltol = 1e-9))
  
  if (opt$conv != 0)
    warning("FUNCTION DID NOT CONVERGE!")
  
  
  k <- c()
  coef <- opt$par
  k$coef <- coef
  k$conv <- opt$convergence
  k$loglik <- opt$value
  k$counts <- as.numeric(opt$counts[1])
  k$y <- y
  k$x <- x
  
  eta_hat <- as.vector(x1%*%(k$coef))
  k$eta.hat <- eta_hat
  
  mu_hat <- exp(eta_hat)
  k$mu.hat <- mu_hat
  
  W<- diag(((4)/(mu_hat^2))*(mu_hat^2))
  
  K <- t(x1)%*%W%*%x1
  
  k$K <- K
  
  vcov <- chol2inv(chol(K))
  k$vcov <- vcov
  
  var_y <- as.vector((mu_hat^2)*(4/pi -1))
  
  k$resid2 <- (y-mu_hat)/sqrt(var_y) # standardized
  
  k$resid1 <- as.vector(qnorm(pr(y,mu_hat))) # quantile residuals
  
  l1<-2*(log(dr(y,y))-log(dr(y,mu_hat)) )
  l1[which(l1<0)]<-0
  k$resid3 <- as.vector(sign(y-mu_hat)*sqrt( l1 ) ) # deviance
  
  stderror <- sqrt(diag(vcov))
  k$stderror <- stderror
  
  beta.nul<-function(y){
    y <- as.vector(y)
    
    #loglik null
    loglikt <- function(theta){
      
      mu <- theta[1]
      
      
      ll <- sum(
        log(pi/2)+log(y)-log(mu^2)-(pi*y^2)/(4*(mu^2))
      )
      
      ll
    }
    
    mu_ini <-mean(y)
    
    ini_nul <- c(mu_ini)
    
    opt_nul <- optim(ini_nul, loglikt, method = "BFGS", control = list(fnscale = -1))
    
    k <- c()
    k$loglik<- opt_nul$value
    
    return(k)
  }
  
  
  R2 <- 1-exp((-2/n)*(opt$value-beta.nul(y)$loglik))
  k$R2 <- R2
  #print(beta.nul(y)$loglik)
  k$zstat <- abs(coef/stderror)
  k$pvalues <- 2*(1 - pnorm(k$zstat) )
  
  k$loglik <- opt$value
  k$counts <- as.numeric(opt$counts[1])
  k$aic <- -2*k$loglik+2*(r)
  k$bic <- -2*k$loglik+log(n)*(r)
  
  model_presentation <- cbind(round(coef,4),round(k$stderror,4),round(k$zstat,4),round(k$pvalues,4))
  rownames<-rownames(model_presentation)
  rownames[1]<-"Intercept"
  colnames(model_presentation)<-c("Estimate","Std. Error","z value","Pr(>|z|)")
  rownames(model_presentation)<-rownames
  
  k$model <- model_presentation
  
  if(diag==1)
  {
    print("Fitted model",quote=F)
    print(k$model)
    print(" ",quote=F)
    print(c("Log-likelihood:",round(k$loglik,4)),quote=F)
    print(c("Number of iterations in BFGS optim:",k$counts),quote=F)
    print(c("AIC:",round(k$aic,4)),quote=F)
    #print(c("Pseudo R-square:",round(k$R2,4)),quote=F)
    
    print("Residuals:",quote=F)
    print(summary(k$resid1))
    
    plot((k$resid1),pch=3)
    abline(h=c(-2,2),lty=3)
    abline(h=c(-3,3),lty=2)
  }
  return(k)
}
\name{LaplaceApproximation}
\alias{LaplaceApproximation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A simple class the maximum likelihood optimizes each component of the model
}
\description{
This is a simple slice sampler that uses stepping out for interval estimation.
}
\usage{
 sampler = new("LaplaceApproximation",model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	Constructor takes the model object create by the lexer
  }


}
\details{
This sampler inherits from MCMCsampler, so that it can benefit from the there implement functionality. It overwrites the sample function that gets called with the likelihood and prior. It used the R optim function to optimize the posterior and sets this value
}
\value{
Returns a valid sampler object.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
 Package Overview:
\code{\link[Slice:Slice-package]{Slice-Package}}

Base sampler class: 
\code{\link[Slice:Sampler]{Sampler}}

Slice sampler class: 
\code{\link[Slice:MCMCsampler]{MCMCsampler}}


}
\examples{

require(Slice)


set.seed(1234)
n <- 100
lambda = 1.125
beta   <- c(1.4,-0.5)
sigma2 <- 0.1
n_time = 10

beta_new = beta[1]%*% exp(-lambda* (0:(n_time-1))  )
beta_new = cbind(beta_new, beta[2])

X<-cbind(matrix(rnorm(n*n_time,2,1) ,ncol = n_time) ,1)

val = X%*% t(beta_new) 
y<- matrix( val + rnorm(n, 0,sigma2) ,ncol = 1)

colnames(y) = 'y'
x<- matrix(X[,1:(ncol(X)-1) ],ncol = n_time)
colnames(x)= paste('x_',1:n_time,sep = '')

model_str= "
model regModel{

  y ~ dnorm( mu , sigma) 
  sequ =  lambda * seq(0,weights-1) %*%-1
  mu = x %*% ( beta1 * exp( sequ ) ) + beta0

  lambda~dgamma(0.001,0.001)
  sigma~dgamma(0.001,0.001)
  beta0 ~ dnorm(0,s)
  beta1 ~ dnorm(0,s)
  s~dgamma(0.001,0.001)
}
"

data_list = list( 'y'=y , 'x'=x ,'weights'=n_time)

lex = new('Lexer')
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

laplace = new('LaplaceApproximation',root_plate)
laplaceApprox = laplace$takeSample(50)

plot(laplaceApprox[,1])

}



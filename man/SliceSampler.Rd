\name{SliceSampler}
\alias{SliceSampler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A simple out stepping Slice sampler class
}
\description{
This is a simple slice sampler that uses stepping out for interval estimation.
}
\usage{
 sampler = new("SliceSampler",model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	Constructor takes the model object create by the lexer
  }


}
\details{
This sampler inherits from MCMCsampler, so that it can benefit from the there implement functionality. It overwrites the sample function that gets called with the likelihood and prior. It samples than a random value regarding the heights and uses the stepping out procedure to estimate the with that height.
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


require('Slice')
#### ordered logit  model

#Generate data:
set.seed(1234)
n<-200
x<-cbind(rnorm(n), rnorm(n) ) 
true.beta<-c(0.2,1.1)
true.alpha = c(0.5,1.8)
  

mu = x%*%true.beta +rnorm(n,0,0.01)
y = c()

for( i in 1:n){
  probs=c()
  p = 0
  q = 0
  for(k in 1:(length(true.alpha)) ){
    tmp = 1/(1+exp(-(true.alpha[k]-mu[i]) ) )
    p = tmp -q
    q = tmp;
    probs = c(probs,p)
  }
  p = 1-q
  probs = c(probs,p)
  
  y=c(y,sample(c(1:3),probs,size = 1,replace=FALSE))

}

y = matrix(y,ncol=1)

###

data_list = list(y=matrix(y),x1=matrix(x[,1]),x2=matrix(x[,2]) )

sigmoid = function(x){ matrix(1/(1+exp(-x) )) }

model_str = '
model{
    mu = x1*beta[1] + x2*beta[2]

  alpha = sort(alpha0) 

    Q[1] =  sigmoid( alpha[1]-mu)
    p[1] = Q[1]

    for(j in 2:2){
      Q[j] = sigmoid(alpha[j]-mu)
      p[j] = Q[j] - Q[j-1]
    }
    p[3] =  1 - Q[2] 
    
    y ~ dcat(p[1],p[2],p[3])

 
  ## priors over thresholds
  for(r in 1:2){
    alpha0[r] ~ dnorm(0,1)
  }

  for(j in 1:2){
    beta[j] ~ dnorm(0,1)
  }
}
'

lex = new('Lexer')
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

sliceSampler = new('SliceSampler',root_plate)

##### ok lets use it to set initial values and sample

samplesFromProblem = sliceSampler$takeSample(100,initialValues =list('alpha0[1]'=matrix(0),'alpha0[2]'=matrix(1) ))

colMeans(samplesFromProblem)

}



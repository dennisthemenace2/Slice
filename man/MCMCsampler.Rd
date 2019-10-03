\name{MCMCsampler}
\alias{MCMCsampler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A simple MCMC sampler class
}
\description{
This is a simple MCMC sampler class that can be used to generade samples from a model.
}
\usage{
 sampler = new("MCMCsampler",model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	Constructor takes the model object create by the lexer
  }


}
\details{
This sampler inherits from the base class sampler. The function takeSample can be used to sample from the model.
Before the sampler starts, it creates a list with all nodes to sample from. This is done to speed up the sampling process.
It then calls the sample funtion, with a list of likelihood objects and prior prior object. This this can be overwritten for convenience by other samplers.
The takeSample function can take up to 3 parameters (nSamples,initialValues = list(),addtionalNodes=list()). The first one is how many samples are to be taken, the second one is list with names and inital values to set, the last parameter is a list of names with addtional nodes to track. This can be usefull if model variables (helperNodes) are to be tracked as well.
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
\code{\link[Slice:SliceSampler]{SliceSampler}}


}
\examples{
require('Slice')

#define a regression model 
my_model_str= "
model regModel{
  #this is a comment
  mu = x*beta1 +beta0
  y~ dnorm(mu,sigma)
  sigma~dgamma(0.01,0.01)
  beta0 ~ dnorm(0,s)
  s ~ dunif(0,1)
  beta1 ~ dnorm(0,1) #one more comment
}
"

#generate some data

set.seed(1234)
n <- 100
beta   <- c(1.4,-0.8)
sigma2 <- 1
X = cbind(rnorm(n,2,1),1)
val = X %*% beta  + rnorm(n,0,sigma2) 
y = matrix( val,ncol = 1)
x = matrix(X[,1],ncol = 1)
data_list = list('y'=y,'x'=x)

lex = new('Lexer') ##create lexer obj
lex$setModelString(my_model_str) #set model
lex$setModelData(data_list) # set data

lex$lexModel() ##lexx the syntax 
 
root_plate = lex$parseModel() ##create the model

mcmcSample = new('MCMCsampler',root_plate)

samplesFromProblem = mcmcSample$takeSample(500, # take 500 samples from the posterior
                                           addtionalNodes=c('mu')) # also track mu


colMeans(samplesFromProblem)
plot(samplesFromProblem[,1])

}



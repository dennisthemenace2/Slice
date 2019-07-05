##test

rm(list = ls())

printf <- function(...) invisible(print(sprintf(...)))



##

my_model_str= "
model regModel{
  #this is a comment
  y~ dnorm(x*beta1 +beta0,sigma)
  sigma~dgamma(0.01,0.01)
  beta0 ~ dnorm(0,s)
  s ~ dunif(0,1)
  beta1 ~ dnorm(0,1) #one more comment
}
"



source('./distributions/cNodes.R')
source('./distributions/base.R')
source('./parser/parser.R')

##
#generate some data
set.seed(1234)
n <- 100
beta   <- c(1.4,-0.8)
sigma2 <- 1
X<-cbind(rnorm(n,2,1),1)
y<- matrix(rnorm(n,X%*% beta ,sigma2) ,ncol = 1)
x<- matrix(X[,1],ncol = 1)

data_list = list('y'=y,'x'=x)
####

lex = Lexer() ##create lexer obj
lex$setModelString(my_model_str) #set model
lex$setModelData(data_list) # set data

lex$lexModel() ##lexx the syntax 
 
root_plate = lex$parseModel() ##create the model

##model is now in a tree structure that can be processed
mcmcSample = MCMCsampler(root_plate)## load model into sampler

samplesFromProblem = mcmcSample$takeSample(500) ##take samples from the posterior

hist(samplesFromProblem[,1])
hist(samplesFromProblem[,2])

colMeans(samplesFromProblem)

mean(samplesFromProblem[,1])
mean(samplesFromProblem[,2])

plot(samplesFromProblem[,1])
plot(samplesFromProblem[,2])


## load model in Slice sampler

sliceSample = SliceSampler(root_plate)

sliceSamples = sliceSample$takeSample(500)
plot(sliceSamples[,1])
plot(sliceSamples[,2])
plot(sliceSamples[,3])
plot(sliceSamples[,4])

colMeans(sliceSample)

#### now added hierarchical model structure



model_str= "
model regModel{
  #this is a comment
  for(c in 1:k){
    y[c] ~ dnorm(x[c]*beta1[c]+beta0[c] ,sigma)
  }
  sigma~dgamma(0.01,0.01)
  beta0[k] ~ dnorm(0,s)
  beta1[k] ~ dnorm(0,1) #one more comment
  s ~ dunif(0,1)
}
"

### now we have to use lists
data_list = list( 'y'=list(y,y) , 'x'=list(x,x) ,'k'=2)



lex = Lexer() 
lex$setModelString(model_str) 
lex$setModelData(data_list) # set data

lex$lexModel() ##lexx the syntax 
 
root_plate = lex$parseModel() ##create the model


mcmcSample = MCMCsampler(root_plate)## load model into sampler

samplesFromProblem = mcmcSample$takeSample(500) ##take samples from the posterior


colMeans(samplesFromProblem)


## load model in Slice sampler

sliceSample = SliceSampler(root_plate)
sliceSamples = sliceSample$takeSample(500)

colMeans(sliceSample)



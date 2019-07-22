# Slice
Bayesian inference engine with focus on slice sampling.



This is currently under development.

It is supposed to be a highly flexible inference engine, that allows to utilize and implement different samplers.





Model similar to the BUGS language can be parsed to create a tree:
```R
model_str= "
model regModel{
  #hierarchical structure of the model
  for(c in 1:k){
    y[c] ~ dnorm(x[c]*beta1[c]+beta0[c] ,sigma)
  }
  sigma~dgamma(0.01,0.01)
  beta0[k] ~ dnorm(0,s)
  beta1[k] ~ dnorm(0,1)
  # this one depends on k parent nodes. notice that the same is true for sigma.
  s ~ dunif(0,1)
}
"
```



This tree can be used to sample componentwise from the posterior distribution.


```R
#generate some data
set.seed(1234)
n <- 100
beta   <- c(1.4,-0.8)
sigma2 <- 1
X<-cbind(rnorm(n,2,1),1)
y<- matrix(rnorm(n,X%*% beta ,sigma2) ,ncol = 1)
x<- matrix(X[,1],ncol = 1)

#preare data list that contains the data and variables
data_list = list( 'y'=list(y,y) , 'x'=list(x,x) ,'k'=2)


lex = Lexer()
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel() ## preapres the model
root_plate = lex$parseModel() ## creates the model

### parsed model can be used to sample
sliceSample = SliceSampler(root_plate)
sliceSamples = sliceSample$takeSample(1000) ## take 1000 samples

colMeans(sliceSamples)
#>  beta1[1]   beta0[1]   beta1[2]   beta0[2]          s      sigma 
#> 1.3266858 -0.6038927  1.3168006 -0.5858345  0.6682655  1.0420163 
```

Storage nodes have been added. They are defined by the '=' operator. They are considered constants and resolved during parsing but still added to the root plate.

More dimensional arrays are now supported, and user defined functions which allows for the implementation of an ordered logit model.


First lets generate the data:
```R
#Generate data:
set.seed(1234)
n<-200
x<-cbind(rnorm(n), rnorm(n) ) 
true.beta  = c(0.2,1.1)
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
  
  y=c(y,sample(c(1:3),probs,size = 1,replace=F))

}

y = matrix(y,ncol=1)
```

Next, we define the model:
```R
#define out sigmoid function
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

  # thresholds
  for(r in 1:2){
    alpha0[r] ~ dnorm(0,1)
  }

  for(j in 1:2){
    beta[j] ~ dnorm(0,1)
  }
}
'

```


Notice that we use user defined functions for sigmoid and the R sort function.
Next we set the data list, parse the model, and use the MCMC sampler (Slice would be perfectly fine too).

```R
data_list = list(y=matrix(y),x1=matrix(x[,1]),x2=matrix(x[,2]) )


lex = Lexer()
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

mcmcSample = MCMCsampler(root_plate)

## I set initial values for an inital correct ordering, will provide a support function in the sampler class for that shortly
#initalValues(root_plate,list('alpha0[1]'=matrix(0),'alpha0[2]'=matrix(1))

samplesFromProblem = mcmcSample$takeSample(1000)

colMeans(samplesFromProblem)
#>  beta[1]   beta[2] alpha0[1] alpha0[2] 
#>0.2085427 1.0558603 0.5668031 1.9566632 
```


The model makes use of indexing, which needs further implementation. Also, the syntax needs some simplifications.

Since apparently, one distribution can be part of multiple slots, the slot members are made unique over the distribution to avoid repeated sampling. 

Further, debugging functionality will hopefully be provided soon, to plot and debug the model easier.


## Recurrent Bayesian Neuronal Networks.


Yes, Bayesian RNN is interesting and would be a nice direction to steer the project towards

So, it took me all weekend, to make a simple RNN structure work in Slice. Consider the simple Elman or Vanilla RNN structure.
As demonstrated in the paper "Bayesian Recurrent Neural Network Models for Forecasting and Quantifying Uncertainty in Spatial-Temporal Data" [( McDermott and Wikle, 2018)](https://arxiv.org/pdf/1711.00636.pdf) this model can be estimated using traditional sampling, although some tricks are included.


Here, you have the basic model definition:

```R
model_str = '
model elman{
W[3,3] ~ dnorm(0,1)
U[3,1] ~ dnorm(0,1)
V[1,3] ~ dnorm(0,1)
mu ~ dnorm(0,1)

B[3,1] ~ dnorm(0,1)

for( e in 1:nexamples){
  h_t[e,1] = 0 
  for( i in 1:ncol(X[e] ) ){
    h_t[e,i+1] =  W * h_t[e,i] +  U* t( X[e,i] ) 
  }
 res[e] = ( mu + (V * h_t[e,seqLength[e]+1] ) ) * B
}
 Y ~dnorm( res  , 1 )
}
'
```
Regarding the technical implementation, notice, that variables like "res", "W", or "V" are used by there names but defined differently.
So, this will lead to the creation of ComputationHelperNodes during parsing which links to the StorageNodes or Distribution.
This means, that StorageNodes can not be pruned but they dont need to be computed since the HelperNodes do this. 
Futher column Vectors are the default for vectors now.

Concerning the model, well, you can see that a lot has been done in terms of supported syntax. The model is very slow, even for redicolous little examples.

The model is not done, yet, but it already spawns a lot of interesting questions. How to measure convergence, correlation, and in general how to notice that my model is not mixing well. 

In the testRnn.R, you will find the working example.



# Todo list

* debugging functions
* add bootstrapping support
* more distributions
* more Sampler 
* data section for data preprocessing ?
* support multiple models sections and utilize results across them

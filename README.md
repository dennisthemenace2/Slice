# Slice
Bayesian inference engine with focus on slice sampling.



This is currently under development. The code has now been moved inside a R package.

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



This tree can be used to sample component-wise from the posterior distribution.


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


lex = new(Lexer)
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel() ## preapres the model
root_plate = lex$parseModel() ## creates the model

### parsed model can be used to sample
sliceSample = new('SliceSampler',root_plate)
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
    Q[1] =  sigmoid( alpha[1]-mu)
    p[1] = Q[1]

    for(j in 2:2){
      Q[j] = sigmoid(alpha[j]-mu)
      p[j] = Q[j] - Q[j-1]
    }
    p[3] =  1 - Q[2] 
    y ~ dcat(p)

  ## priors over thresholds
  alpha0[2] ~ dnorm(0,1)
  
  alpha = sort(alpha0)  
  beta[2] ~ dnorm(0,1)
}
'

```


Notice that we use user defined functions for sigmoid and the R sort function.
Next we set the data list, parse the model, and use the MCMC sampler (Slice would be perfectly fine too).

```R
data_list = list(y=matrix(y),x1=matrix(x[,1]),x2=matrix(x[,2]) )


lex = new('Lexer')
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

mcmcSample = new('MCMCsampler',root_plate)

samplesFromProblem = mcmcSample$takeSample(1000,initialValues =list('alpha0[1]'=matrix(0),'alpha0[2]'=matrix(1) ))


colMeans(samplesFromProblem)
#>  beta[1]   beta[2] alpha0[1] alpha0[2] 
#>0.2085427 1.0558603 0.5668031 1.9566632 
```


The model makes use of indexing, which needs further implementation. Also, the syntax needs some simplifications.

Since apparently, one distribution can be part of multiple slots, the slot members are made unique over the distribution to avoid repeated sampling. 

Further, debugging functionality will hopefully be provided soon, to plot and debug the model easier.

## Decayed Regression.

Assume y is predicted by the features which have decreasing influence on the prediction.
instead of estimating one beta for each feature be assume one beta that decreases over the features

y =x1 * beta1*exp(-lambda*0) + x2 * beta1*exp(-lambda*1)+..+ beta0 

The first number in the exponent we want to be 0 so that exp(0)=1

Lets write down the joint probability 
p(y,x,beta, lambda, s, sigma) = p(y|x*b*exp(-lambda*t), sigma )
                                p(beta|0,s) p(lambda|e0,f0) p(sigma|a0,b0) p(s|c0,c0)   

Now, we can implement this in Slice:

```R
model_str = '
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
'
```

The implementation can be found in the "testDecayRegression.R" file.
I also made a variational implementation and Gibbs sampler for this model, which can be found here:[Variational Model](https://github.com/dennisthemenace2/hBReg/blob/master/decayRegression.R)

## Recurrent Bayesian Neuronal Networks.


Yes, Bayesian RNN is interesting and would be a nice direction to steer the project towards

So, it took me all weekend, to make a simple RNN structure work in Slice. Consider the simple Elman or Vanilla RNN structure.
As demonstrated in the paper "Bayesian Recurrent Neural Network Models for Forecasting and Quantifying Uncertainty in Spatial-Temporal Data" [( McDermott and Wikle, 2018)](https://arxiv.org/pdf/1711.00636.pdf) this model can be estimated using traditional sampling, although some tricks are included.


Here, you have the basic model definition:

```R
model_str = '
model elman{
WL[3,3] ~ dtruncnorm(-1,1,0,1)
WU[3,3] ~ dtruncnorm(-1,1,0,0.1)

U[3,1] ~ dnorm(0,1)
V[1,3] ~ dnorm(0,1)
mu ~ dnorm(0,1)
gamma[3,3] ~ dbern(0.5)

delta ~ dunif(0,1)

W = gamma * WL + (1-gamma) * WU 

B[3,1] ~ dnorm(0,1)

for( e in 1:nexamples){
  h_t[e,1] = matrix(0,3,3)
  for( i in 1:ncol(X[e] ) ){
    h_t[e,i+1] =   tanh(  (delta / largestEigenValue(W)) * (W %*% h_t[e,i] ) +  U %*% t( X[e,i] ) )
  }
 res[e] = ( mu + (V %*% h_t[e,seqLength[e]+1] ) ) %*% B
}
 Y ~dnorm( res  , 1 )
}
'
```
Regarding the technical implementation, notice, that variables like "res", "W", or "V" are used by there names but defined differently.
So, this will lead to the creation of ComputationHelperNodes during parsing which links to the StorageNodes or Distribution.
This means, that StorageNodes can not be pruned but they dont need to be computed since the HelperNodes do this. 
Futher column Vectors are the default for vectors now. The operator %*% is matrix multiplication in agreement with R.

Concerning the model, well, you can see that a lot has been done in terms of supported syntax. The model is very slow, even for redicolous little examples.

The model is not done, yet, but it already spawns a lot of interesting questions. How to measure convergence, correlation, and in general how to notice that my model is not mixing well. 

In the testRnn.R, you will find the working example.



# Todo list

* add support for different samplers among levels
* debugging functions
* add bootstrapping support
* more distributions
* more Sampler 
* data section for data preprocessing ?
* support multiple models sections and utilize results across them

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
sliceSamples = sliceSample$takeSample(1) ## take 1 sample only

#>sliceSamples
#>  beta1[1]   beta0[1]   beta1[2]   beta0[2]          s      sigma 
#> 1.3394608 -0.4819224  1.3985531 -0.5516930  0.6699176  1.0572532 

```



# Todo list

* "Computation nodes" to store and use computations
* support for more dimensional arrays
* add bootstrapping support
* more distributions
* add support for user defined functions
* more Sampler 
* data section for data preprocessing ?
* support multiple models sections and utilize results across them

# Slice
Bayesian inference engine with focus on slice sampling.



This is currently under development.

It is supposed to be a highly flexible inference engine, that allows to utilize and implement different samplers.




Model similar to the BUGS language can be parsed to create a tree:

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

This tree can be used to sample component wise from the posterior distribution.

##test

rm(list = ls())

printf <- function(...) invisible(print(sprintf(...)))




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
model_str = my_model_str


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

lex = Lexer()
lex$setModelString(my_model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()


mcmcSample = MCMCsampler(root_plate)

samplesFromProblem = mcmcSample$takeSample(1000)

hist(samplesFromProblem[,1])
hist(samplesFromProblem[,2])

colnames(samplesFromProblem)

mean(samplesFromProblem[,1])
mean(samplesFromProblem[,2])

plot(samplesFromProblem[,1])
plot(samplesFromProblem[,2])



## load model in Slice sampler

sliceSample = SliceSampler(root_plate)

sliceSamples = sliceSample$takeSample(1000)
plot(sliceSamples[,1])
plot(sliceSamples[,2])
plot(sliceSamples[,3])
plot(sliceSamples[,4])

colnames(sliceSamples)

mean(sliceSamples[,1])
mean(sliceSamples[,2])
mean(sliceSamples[,3])
mean(sliceSamples[,4])





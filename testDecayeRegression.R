set.seed(1234)
n <- 100
lambda = 1.125
beta   <- c(1.4,-0.5)
sigma2 <- 0.1
n_time = 10

beta_new = beta[1]%*% exp(-lambda* (0:(n_time-1))  )
beta_new = cbind(beta_new, beta[2])

X<-cbind(matrix(rnorm(n*n_time,2,1),ncol = n_time) ,1)

y<- matrix(rnorm(n,X%*% t(beta_new) ,sigma2) ,ncol = 1)

predErr = sum( (X%*%t(beta_new) -y)^2 )

colnames(y) = 'y'
x<- matrix(X[,1:(ncol(X)-1) ],ncol = n_time)
colnames(x)= paste('x_',1:n_time,sep = '')

#model = lm('y~.',as.data.frame(cbind(x,y)))
#summary(model)



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

lex = Lexer()
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

mcmcSample = MCMCsampler(root_plate)

samplesFromProblem = mcmcSample$takeSample(5000)

plot(samplesFromProblem[,'beta1'])
plot(samplesFromProblem[,'beta0'])
plot(samplesFromProblem[,'lambda'])
plot(samplesFromProblem[,'s'])
colMeans(samplesFromProblem)

sliceSample = SliceSampler(root_plate)

sliceSamples = sliceSample$takeSample(5000)
plot(sliceSamples[,'beta1'])
plot(sliceSamples[,'beta0'])
plot(sliceSamples[,'lambda'])
plot(sliceSamples[,'s'])
colMeans(sliceSamples)


laplace = LaplaceApproximation(root_plate)
laplaceApprox = laplace$takeSample(50)
plot(laplaceApprox[,1])





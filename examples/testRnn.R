###test rnn structure
require(Slice)
printf <- function(...){}


largestEigenValue = function(W){
  eigenvalues = eigen(W)$values
  Me =  max(Re(eigenvalues[abs(Im(eigenvalues)) < 1e-6]))
  
  return(matrix(Me) )
}

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
##generate data
V =  matrix(seq(0.5,1,length.out = 3),nrow=1)
W = matrix(seq(0,1,length.out = 9) ,ncol=3)
B= matrix(seq(0.5,1,length.out = 3),ncol= 1)
U = matrix(seq(0.5,1,length.out = 3),ncol=1 )

set.seed(1234)
X_list = list()
Y = c()
N_examples = 4
for(i in 1:N_examples){
  ht= matrix(0,ncol=3,nrow=3)
  X = matrix(,nrow=3,ncol=0)
  for(slen in 1:4){
   x = matrix(runif(3))
   X = cbind(X, x)
   ht = W%*%ht + U%*%t(x)
   yt = (0.1 + V%*%ht) %*% B 
  }
  Y= c(Y, yt )
  X_list = append(X_list, list(X) )
}

data_list = list('Y'=matrix(Y),'X'=X_list,'nexamples'=length(X_list), 'seqLength'=rep(4,N_examples) )
  
  lex = new('Lexer')
  lex$setModelString(model_str)
  lex$setModelData(data_list)
  
  lex$lexModel()
  
  root_plate = lex$parseModel()

mcmcSample = new('MCMCsampler',root_plate)

set.seed(1234)
samplesFromProblem = mcmcSample$takeSample(2,addtionalNodes=c('W'))

colMeans(samplesFromProblem)


#sliceSample = new('SliceSampler',root_plate)

#sliceSamples = sliceSample$takeSample(2)

createAutoCorrelationPlot= function(samples){

  nc = ncol(samples)
  cnames = colnames(samples)
  par(mfrow=c(ceiling( sqrt(nc) ),ceiling( sqrt(nc) )))
  
  for( x in 1:nc){
    xname = cnames[x]
    acf(samples[,xname],main=xname)
  }
  par(mfrow =c(1,1))  
}
createAutoCorrelationPlot(samplesFromProblem[,1:16])

createCorrelationPlot = function( samples) {
  
  #old_par = par()
  nc = ncol(samples)
  
  cnames = colnames(samples)
  par(mfrow=c(nc,nc))
  
  for( x in 1:nc){
    xname = cnames[x]
     for( y in 1:nc){
       yname = cnames[y]
       if(x==y){
         acf(samples[,yname],main=yname)
         next
       }
       ccf(samples[,xname], samples[,yname],main=paste(xname,'/',yname ) )
     }
  }
  par(mfrow =c(1,1))
}

createCorrelationPlot(samplesFromProblem[,10:14])

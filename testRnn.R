###test rnn structure


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
##generate data
V =  matrix(seq(0.5,1,length.out = 3),nrow=1)
W = matrix(seq(0,1,length.out = 9) ,ncol=3)
B= matrix(seq(0.5,1,length.out = 3),ncol= 1)
U = matrix(seq(0.5,1,length.out = 3),ncol=1 )

set.seed(1234)
X_list = list()
Y = c()
N_examples = 3
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
  
lex = Lexer()
lex$setModelString(model_str)
lex$setModelData(data_list)
  
lex$lexModel()
  
root_plate = lex$parseModel()

mcmcSample = MCMCsampler(root_plate)

samplesFromProblem = mcmcSample$takeSample(10)

colMeans(samplesFromProblem)

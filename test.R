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



#### ordered logit  model


#Generate data:
set.seed(1234)
n<-200
x<-cbind(rnorm(n), rnorm(n) ) 
true.beta<-c(0.2,1.1)
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


###


data_list = list(y=matrix(y),x1=matrix(x[,1]),x2=matrix(x[,2]) )


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

 
  ## priors over thresholds
  for(r in 1:2){
    alpha0[r] ~ dnorm(0,1)
  }

  for(j in 1:2){
    beta[j] ~ dnorm(0,1)
  }
}
'

lex = Lexer()
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

mcmcSample = MCMCsampler(root_plate)



##recurrent function for debugging potentially. also used right now to set initial values

##create vertex list
getSourceTarget = function(root_plate,skipStorage =F,init_list=list()){
    
  if( class(root_plate)!='Plate'){
    printf(' is not plate')
  }
  
  printf('Plate: %s',root_plate$getName())
  nodeList = root_plate$getNodeList()
  sourceNodes = c()
  targetNodes = c()
  for(i in 1:length(nodeList)){
    printf('nodes: %d/%d',i, length(nodeList))
    node = nodeList[[i]]
    if(inherits(node,'Distribution')){
      printf('Node: %s is Distribution',node$getName())
      if(class(node) =='StorageNode' & skipStorage){
        printf('is StorageNode')
        next;
      }
      
      slots = node$slots
      if(length(slots)==0){
        print('next')
        next;
      }
      for(s in 1:length(slots)){
        slotPlate = slots[[s]]
        if(class(slotPlate)!= 'Plate'){
          printf('is not plate')
          break;
        }
        printf('slotName: %s', slotPlate$getName())
        nodeList2 = slotPlate$getNodeList()
        for( k in 1:length(nodeList2)){
          node2 = nodeList2[[k]]
          printf('ChildNodes: %s', node2$getName())
          
          ###init 
          if(length(init_list)>0){
            distNames = names(init_list)
            idx = which(distNames ==node2$getName())
            if(length(idx)>0){
              printf('old value:%f',node2$cvalue)
              node2$cvalue = init_list[[idx]]
              printf('new value:%f',node2$cvalue)
            }
          }
          
          sourceNodes=c(sourceNodes,node$getName())
          targetNodes = c(targetNodes,node2$getName() )
        }
        ret = getSourceTarget(slotPlate)
        sourceNodes=c(sourceNodes,ret$source)
        targetNodes = c(targetNodes,ret$target )
        
        
      }
    }
    
    
  }
  return(list('source'=sourceNodes,'target' =targetNodes))

}

ret = getSourceTarget(root_plate,T)

library(igraph)

# create data:
links=data.frame(
  source=ret$source,
  target=ret$target
)


network=graph_from_data_frame(d=links, directed=T) 

deg=degree(network, mode="all")

plot(network)



##### ok lets use it to set initial values and sample


samplesFromProblem = mcmcSample$takeSample(1000,initialValues =list('alpha0[1]'=matrix(0),'alpha0[2]'=matrix(1) ))

colMeans(samplesFromProblem)


#sliceSample = SliceSampler(root_plate)
#sliceSamples = sliceSample$takeSample(1000)
#colMeans(sliceSamples)







### for testing the calculation needs refinement for future debuging fcntlty

getComputation = function(csNode) {
  printf("class:%s",class(csNode) )
  if(class(csNode) == 'ComputationNodeRef'){
    ##only has one 
    printf('node:%s',csNode$a$getName() )
    printf("class of refnode:%s",class(csNode$a) )
    if(class(csNode$a) =='StorageNode'){
      getComputation (csNode$a$cslots[[1]])
    }
    
  }else if(class(csNode) == 'ComputationNodeFunction'){
    printf('node is funtion:%s',csNode$fcnt )
    for(i in 1:length(csNode$slots)){
      printf('argument %d:',i)
      getComputation(csNode$slots[[i]])
    }
  }else if(class(csNode) == 'ComputationNodeAdd'){
    printf('node is +' )
    printf('a:')
    getComputation(csNode$a)
    printf('b:')
    getComputation(csNode$b)
    
  }else if(class(csNode) == 'ComputationNodeSub'){
    printf('node is -' )
    printf('a:')
    getComputation(csNode$a)
    printf('b:')
    getComputation(csNode$b)
  }else if(class(csNode) == 'ComputationNodeMultiply'){
    printf('node is *' )
    printf('a:')
    getComputation(csNode$a)
    printf('b:')
    getComputation(csNode$b)
  }else if(class(csNode) == 'ComputationNodeDiv'){
    printf('node is /' )
    printf('a:')
    getComputation(csNode$a)
    printf('b:')
    getComputation(csNode$b)
  }else if(class(csNode) =='ComputationNodeValue'){
    printf('data name:%s',csNode$name)
  }else if( class(csNode) =='ComputationNodeIndex'){
    printf('is index')
    getComputation(csNode$a)
  } 
  else{
    printf('unknown')
  }
  
}
getComputation( root_plate$getNodeList()[[1]]$cslots[[1]])






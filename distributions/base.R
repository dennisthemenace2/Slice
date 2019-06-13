#distrubution
          
DataContainer <- setRefClass("DataContainer",
                            fields = list(data='matrix',empty='logical' ),
                            methods = list(
                              initialize = function(data=NULL) {
                                if(is.null(data)){
                                  .self$data = matrix()
                                  .self$empty=TRUE
                                }else{
                                  .self$data=data
                                  .self$empty=FALSE    
                                }
                                
                              }
                            )
                            )

Plate <- setRefClass("Plate",
                            contains='Node',
                            fields = list(plate_name='character',nodes='list'),
                            methods = list(
                              initialize = function(name=NULL) {
                                if(is.null(name)){
                                  .self$plate_name = 'NoName'
                                }else{
                                  .self$plate_name = name
                                }
                                
                              },
                              addNode=function(node){
                                .self$nodes =append(.self$nodes, node)
                              },
                              getNodeList=function(){
                                return(nodes)
                              },
                              getName=function(){
                                return(plate_name)
                              },
                              getNumberOfNodes=function(){
                                return(length(nodes))
                              }
                              
                            )
)

Distribution <- setRefClass("Distribution",
                            contains='Node',
                     fields = list(dist_name='character',cvalue = 'matrix',
                                   slots='list',cslots='list'),
                     methods = list(
                       compute = function(){
                         return(.self$cvalue)
                       },
                       getName = function(){
                         return( dist_name)
                       },
                       initialize = function(name=NULL) {
                         .self$slots = list()
                         .self$cslots = list()
                         
                         if(is.null(name)){
                           .self$dist_name ='NoName'
                         }else{
                           .self$dist_name = name
                         }
                         .self$cvalue= matrix(runif(1)) ##think about initial values
                       },
                       logLike = function(){
                       }
                       )
                     )

Constant <- setRefClass("Constant",
                                  fields = list(),
                                  contains = "Distribution",
                                  methods = list(
                                    initialize = function(value) {
                                      #print(class(value))
                                      .self$dist_name = as.character(value)
                                      if(is.numeric(value)){
                                        value = as.matrix(value,ncol=1)
                                      }
                                      if(is.character(value)){
                                        
                                        value = as.matrix(as.numeric(value) ,ncol=1)
                                      }
                                      .self$cvalue = value
                                    },
                                    logLike = function(){
                                      return(log(cvalue)) 
                                    }
                                  )
)

NormalDistribution <- setRefClass("NormalDistribution",
                              fields = list(data='DataContainer'),
                              contains = "Distribution",
                              methods = list(
                                setData = function(data){
                                  .self$data = DataContainer(data)
                                },
                                initialize = function(name=NULL,data=NULL,mean=NULL,cmean=NULL ,variance=NULL,cvariance=NULL){
                                  callSuper(name)
                                  if(is.null(data)){
                                    data= DataContainer() #create empty one
                                  }
                                 # .self$dist_name = 'normal'
                                #  .self$mean = mean
                                #  .self$variance = variance
                                  .self$data = data
                                #  .self$cnA = Node()
                                #  .self$cnB = Node()
                                  if(!is.null(mean)){
                                    .self$slots[[1]]=mean
                                    if(is.null(cmean)){
                                      .self$cslots[[1]]= ComputationNodeRef(mean)
                                    }else{
                                      .self$cslots[[1]]=cmean
                                    }
                                  }
                                  if(!is.null(variance)){
                                    .self$slots[[2]]=variance
                                  ##if computation nodes are null, than they are terminal nodes so i can just create some 
                                  #cref nodes
                                    if(is.null(cvariance)){
                                      .self$cslots[[2]]=ComputationNodeRef(variance)
                                    }else{
                                      .self$cslots[[2]]=cvariance
                                    }
                                  }
                             
                                },
                                logLike = function(){
                                  pred =   cslots[[1]]$compute()
                                  var =   cslots[[2]]$compute()
                                #  printf("pred:%f,var:%f",pred,var)
                                  if(!.self$data$empty){ ##obsevred likelyhood
                                    singlelikelihoods = dnorm(.self$data$data, mean = pred, sd = var, log = T)  
                                  }else{##unobseverd take sample at current position
                                    singlelikelihoods = dnorm(.self$cvalue, mean = pred, sd = var, log = T)  
                                    printf("singlelikelihoods:%f,.self$cvalue:%f",singlelikelihoods,.self$cvalue)
                                  }
                                  
                                  sumll = sum(singlelikelihoods)
                                 # printf("sumll:%f",sumll)
                                  return(sumll)    
                                }#,
                                #sample=function(position){
                                #we have to take a sample from proceeding distr.
                                  ##  
                                  
                                #}
                                  
                              )
                       )

##
#generate some data
#set.seed(1234)
#n <- 100
#beta   <- c(1.4,-0.8)
#sigma2 <- 1
#X<-cbind(rnorm(n,2,1),1)
#y<- matrix(rnorm(n,X%*% beta ,sigma2) ,ncol = 1)
#x<- matrix(X[,1],ncol = 1)

#const0 =Constant(0)
#const1 =Constant(1)


#cnX = ComputationNodeValue(x)
#beta1Dist = NormalDistribution(name='beta[1]',data=NULL,mean=const0,variance = const1)
#beta0Dist = NormalDistribution(name='beta[0]',data=NULL,mean=const0,variance =const1)

#plate_beta = Plate('beta')
#plate_beta$addNode(beta0Dist)
#plate_beta$addNode(beta1Dist)


#beta0Dist$compute()

#refnodebeta0 = ComputationNodeRef(beta0Dist)
#refnodebeta1 = ComputationNodeRef(beta1Dist)

#refnodebeta0$compute()
#beta0Dist$cvalue=matrix(0.666)
#refnodebeta0$compute()

#multiNode = ComputationNodeMultiply(cnX,refnodebeta1)
#multiNode$compute()
#cdistNode = ComputationNodeAdd(multiNode,refnodebeta0)

#cdistNode$compute()

#comp = computationNodeAdd(computationNodeMultiply(computationNodeValue(5),computationNodeValue(5)),computationNodeValue(5))


#model = NormalDistribution(name='Y',data=DataContainer(y),mean = plate_beta,cmean=cdistNode ,variance = Constant(1) )
# put it in a plate

#plate_root = Plate('Y_root')
#plate_root$addNode(model)


#model$sample(0.1)



##
Sampler <- setRefClass("Sampler",
                             fields = list(model='Node' ),
                             methods = list(
                               initialize = function(model) {
                                  .self$model = model
                               },
                               takeSample = function(nSamples){
                               } 
                             )
)


MCMCsampler <- setRefClass("MCMCsampler",
                           contains = 'Sampler',
                       fields = list(),
                       methods = list(
                         initialize = function(model) {
                           callSuper( model)
                         },
                         takeSample = function(nSamples){
                           ##ok.... we have the root, we nee to update samples now
                           if(nSamples<=0){
                             printf('Invalid number samples:%f',nSamples)
                             return()
                           }
                           #root
                           rootList = .self$model$getNodeList()
                           if(length(rootList)==0){
                             printf('No nodes in%s',.self$model$getName())
                           }
                           if(class(.self$model)!='Plate'){
                             printf('%s is not a Plate',.self$model$getName())
                           }
                           ##go recurrent throut the model.
                           ret = unlist( walkPlate(.self$model)  )
                           for( ns in 1:(nSamples-1) ){
                             s1 = walkPlate(.self$model)
                             ret = rbind(ret,unlist(s1))
                           }
                           ret
                         },
                         walkPlate=function(root){
                           samplesList = list()
                           
                           if(class(root)!='Plate'){
                             printf('%s is not a Plate',root$getName()) #what should i do , just return ot what
                           }
                           rootList = root$getNodeList()
                           if(length(rootList)==0){
                             printf('No nodes in%s',root$getName())
                           }
                           for(r in 1:length(rootList)){
                             node = rootList[[r]] # this is the likelyhood we can take samples now.
                             # we shoudl check for data before taking sample...
                             if(class(node)=='Plate'){
                               printf('node is plate:%s',node$getName())
                             }else{
                               if(node$data$empty){
                                 print('node has no data but ok...')
                               }
                             }
                             
                             ##for each slot this has to be a distibution
                             if(length(node$slots)==0){
                               printf('No slots in node in%d',r)
                               return(samplesList)
                             }
                             #need to sample from each one
                             for(s in 1:length(node$slots)){
                               #get plate ...
                               cplate = node$slots[[s]]
                               if(class(cplate)=="Plate"){ #but it doenst need to be, but i could so if it wants evaluation
                                 print('ok is plate!')
                                 ## we have to sample for each element
                                 cnodes = cplate$getNodeList()
                                 if(length(cnodes)==0 ){
                                   print('empty plate !')
                                   return(samplesList)
                                 }
                                 
                                
                                 for(e in 1:length(cnodes)){
                                   ##ok this is the prior node finally. shoudl we check for values ? of if its constand and doesnt contain data
                                   priorNode = cnodes[[e]]
                                   ##echeck for type palte
                                   if(class(priorNode)=='Plate'){
                                     print('is plate i need to go deeper')
                                     stop('not implemnetd yet')
                                     next
                                   }
                                   
                                   if(priorNode$data$empty){
                                     print('thats great we can sample')
                                     ## ok we found that we need to take samples from
                                     #like = node
                                     retssample = sample(node,priorNode)
                                     nodeName = priorNode$getName()
                                     samplesList[[nodeName]] =retssample
                                   }else{
                                     print('node contains data this should be a likelihood of some kind')
                                   }
                                   
                                   ##walk up one step
                                   retslist = walkPlate(cplate)
                                   if(length(retslist)>0){
                                     samplesList = append( samplesList,retslist)
                                   }
                                 }
                                 ##ok we sampled platt go one level up
                                 
                               }else{
                                 printf('is not plate:%s',cplate$getName())
                               }
                             }
                           }
                           return(samplesList)
                         },
                         sample=function(likelihood,prior){
                            printf('taking sample from like:%s and prior:%s',likelihood$getName(),prior$getName() )
                         ###mcmc step
                         # betanew= beta + t(rmvnorm( 1,rep(0,p),diag(p)*0.1 ))
                         # oldprob = calcProb(X%*%beta,Y,sigma)
                         
                           oldprob = likelihood$logLike()+prior$logLike() ##current position
                         
                         #eta = X%*%betanew
                         #newprop = calcProb(eta,Y,sigma)
                           oldvalue = prior$cvalue
                           prior$cvalue =oldvalue  + rnorm(1,0,0.25) ##change proposal distribution somewhere
                          printf('new value:%f',prior$cvalue )
                           newprop = likelihood$logLike()+prior$logLike()
                         
                           printf('newprop:%f,oldprob:%f',newprop,oldprob )
                           
                         # MH Acceptance Ratio on Log Scale
                           ratio<-(newprop)-(oldprob)
                           printf('ratio:%f',ratio)
                           if(log(runif(1))<ratio) {
                             #accept = accept +1
                             print('accept')
                           }else{
                             print('reject')
                             prior$cvalue = oldvalue ##set back !
                           }
                            prior$cvalue
                            
                          }
                         
                       )
)

#mcmcSample = MCMCsampler(plate_root)

#samplesFromProblem = mcmcSample$takeSample(1000)

#hist(samplesFromProblem[,1])
#hist(samplesFromProblem[,2])

#colnames(samplesFromProblem)

#mean(samplesFromProblem[,1])
#mean(samplesFromProblem[,2])

#plot(samplesFromProblem[,1])
#plot(samplesFromProblem[,2])




SliceSampler <- setRefClass("SliceSampler",
                           contains = 'MCMCsampler',
                           fields = list(),
                           methods = list(
                             sample=function(likelihood,prior){
                               printf('taking sample from like:%s and prior:%s',likelihood$getName(),prior$getName() )
                             
                               x =  prior$cvalue
                               
                               height =  likelihood$logLike()+prior$logLike() 
                               y = runif(1, 0, exp(height) )    # Take a random y value
                             
                               f=function(x){ ##maybe we dont need to reset because we accept anyway
                                 tmp =  prior$cvalue
                                 prior$cvalue =matrix(x)
                                 likelihood$logLike()+prior$logLike()
                                 prior$cvalue =tmp
                               }
                               likelihood$logLike()+prior$logLike() 
                               
                               int = runif(1,0,1)
                               x.interval =c(x-int,x+int)
                       #### find end points
                               
                               prior$cvalue
                               
                             }
                             
                           )
)


#sliceSample = SliceSampler(plate_root)

#samplesFromProblem = sliceSample$takeSample(1000)
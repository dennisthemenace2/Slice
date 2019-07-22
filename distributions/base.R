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
                                if(!contains(node)){
                                .self$nodes =append(.self$nodes, node)
                                }
                              },
                              contains = function(node){
                                if(length(.self$nodes)==0){
                                  return(FALSE)
                                }
                                for(i in 1:length(.self$nodes)){
                                  if(.self$nodes[[i]]$getName() == node$getName()){
                                    return(TRUE)
                                  }
                                }
                                return(FALSE)
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
                                   slots='list',cslots='list',parents='list' ),
                     methods = list(
                       addParentNode =function(node){
                          ret = getParentNode(node$getName())
                          if(is.null(ret)){
                             .self$parents = append(.self$parents,node)
                          }
                       },
                       isLastParentNode=function(name){
                         if(length(.self$parents)==0){
                           return(FALSE)
                         }
                         return(.self$parents[[length(.self$parents)]]$getName() ==name )
                       },
                       getParentNode = function(name){
                         
                         if(length(.self$parents)==0){
                           return(NULL)
                         }
                         for(i in 1:length(.self$parents)){
                           if(.self$parents[[i]]$getName() == name){##found
                             return(.self$parents[[i]])
                           }
                         }
                         return(NULL)
                       },
                       getBounds = function(){
                         return(c(-Inf,Inf))
                       },
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
                         print('called base class')
                       },
                       setCurrentValue = function(v){
                         .self$cvalue = matrix(v)
                         return(TRUE)
                       },
                       getCurrentValue = function(v){
                         return(.self$cvalue)
                       }
                       
                       
                       )
                     )


###
ComputationHelperNode <- setRefClass("ComputationHelperNode",
                           fields = list(matrixType='character',ncol='numeric',nrow='numeric'),
                           contains = "Distribution",
                           methods = list(
                             compute = function(){
                               if(length(cslots)==0){
                                 printf('no cslosts set !')
                                 return(NULL)
                               }
                               values = c()
                               for(i in 1:length(cslots)){
                                 values = c(values, cslots[[i]]$compute())
                               }
                               if(matrixType=='col'){
                                 .self$cvalue = matrix(values, ncol = 1)
                               }else if(matrixType=='row'){
                                 .self$cvalue = matrix(values, nrow = 1)
                               }else if(matrixType=='matrix'){
                                 .self$cvalue = matrix(values, ncol=.self$ncol,nrow=.self$nrow,byrow = T) ##unsure of byrow value
                               }
                              
                               return(.self$cvalue) 
                             },
                             initialize = function(name,distList) {
                               callSuper(name)
                            #   addParentNode(parent$distrib)
                               if(length(distList)==0){
                                 printf('passed no list!')
                                 return(.self)
                               } 
                               ##figur out from name if row col or matrix
                               .self$matrixType = 'col' #setDefault
                               fullNames = c()
                               #addSlotMem = c()
                               printf('listClass:%s',class(distList[[1]]) )
                               if(class(distList[[1]]) == 'DistributionLexer' ){
                                 printf('distribution lexer')
                                 for(i in 1:length(distList)){
                                   cd = distList[[i]]
                                   .self$cslots[[i]] = cd$distrib ##set distibution
                                   fullNames = c(fullNames, cd$name)
                                   #we could also take care of slot members
                                  # addSlotMem = c(addSlotMem, unlist(cd$slotMembers) ) ## just take all ??
                                   printf('slot is node:%s',class(cd$distrib))
                                   ## i assume its storage node might we wrong
                                 }
                                # addSlotMem = unique(addSlotMem) ## okk add them
                                 
                               }
                               
                               ##get type.. now
                               lex = Lexer()
                               dimMatrix = NULL
                               for(i in 1:length(fullNames)){
                                 ret = lex$getIndex(fullNames[i])
                                 if(!is.null(ret)){
                                   if(is.null(dimMatrix)){
                                     dimMatrix = matrix(as.numeric(unlist(ret$index)) , ncol=length(ret$index))
                                   }else{
                                     dimMatrix = rbind(dimMatrix, as.numeric(unlist(ret$index)))
                                   }
                                 }
                               }
                               if(!is.null(dimMatrix) ){
                                 if(ncol(dimMatrix)>2){
                                   
                                 }else if(ncol(dimMatrix)==2){
                                   dim1 = unique(dimMatrix[,1])
                                   dim2 = unique(dimMatrix[,2])
                                   if(length(dim1)==1){
                                     if(length(dim2)>1){
                                       .self$matrixType = 'row'
                                     }
                                   }else{
                                     if( length(dim2)>1 ){
                                       ##might be matrix
                                       if(length(distList) > length(dim2)){
                                         .self$matrixType = 'matrix'
                                         .self$ncol = length(dim2) 
                                         .self$nrow = length(dim1)
                                         printf('set matrix type for:%s',.self$getName())
                                       }
                                     }
                                   }
                                 }else{
                                   printf('on dimension we use col vector')
                                 }
                                 printf('helperNode %s id type %s ',.self$getName(),.self$matrixType)
                               }###end dim matrix check 
                               
                             }
                           )
)
##

StorageNode <- setRefClass("StorageNode",
                        fields = list(),
                        contains = "Distribution",
                        methods = list(
                      #    addParentNode =function(node){
                       #     callSuper(name)
                        #  },
                          compute = function(){
                            .self$cvalue =  as.matrix(cslots[[1]]$compute())
                            return(.self$cvalue) 
                          },
                          initialize = function(name) {
                            callSuper(name)
                          }
                        )
)
Constant <- setRefClass("Constant",
                                  fields = list(),
                                  contains = "Distribution",
                                  methods = list(
                                    getBounds = function(){
                                      return(c(cvalue,cvalue))
                                    },
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
                                 # printf("%s pred, %f var, name:%s",pred,var,getName())
                                  if(!.self$data$empty){ ##obsevred likelyhood
                                    singlelikelihoods = dnorm(.self$data$data, mean = pred, sd = var, log = T)  
                                  }else{##unobseverd take sample at current position
                                    singlelikelihoods = dnorm(.self$cvalue, mean = pred, sd = var, log = T)  
                                #    printf("singlelikelihoods:%f,.self$cvalue:%f",singlelikelihoods,.self$cvalue)
                                  }
                                  
                                  sumll = sum(singlelikelihoods)
                                #  printf("sumll:%f",sumll)
                                  return(sumll)    
                                }#,
                                #sample=function(position){
                                #we have to take a sample from proceeding distr.
                                  ##  
                                  
                                #}
                                  
                              )
                       )



GammaDistribution <- setRefClass("GammaDistribution",
                                  fields = list(data='DataContainer'),
                                  contains = "Distribution",
                                  methods = list(
                                    setData = function(data){
                                      .self$data = DataContainer(data)
                                    },
                                    initialize = function(name=NULL){
                                      callSuper(name)
                                    },
                                    logLike = function(){
                                      a =   cslots[[1]]$compute()
                                      b =   cslots[[2]]$compute()
                                      #  printf("pred:%f,var:%f",pred,var)
                                      if(!.self$data$empty){ ##obsevred likelyhood
                                        singlelikelihoods = dgamma(.self$data$data, shape  = a, rate = b, log = T)  
                                      }else{##unobseverd take sample at current position
                                        singlelikelihoods = dgamma(.self$cvalue,  shape  = a, rate = b, log = T)  
                                     #   printf("singlelikelihoods:%f,.self$cvalue:%f",singlelikelihoods,.self$cvalue)
                                      }
                                      
                                      sumll = sum(singlelikelihoods)
                                      # printf("sumll:%f",sumll)
                                      return(sumll)    
                                    },
                                    getBounds = function(){
                                      return(c(0,Inf))
                                    },
                                    setCurrentValue = function(v){
                                      if(v<0){
                                        printf('Value ca not be negative for dgamma:%f',v)
                                        return(FALSE)
                                      }
                                      callSuper(v)
                                    }
                                  )
)



UniformDistribution <- setRefClass("UniformDistribution",
                                 fields = list(data='DataContainer'),
                                 contains = "Distribution",
                                 methods = list(
                                   setData = function(data){
                                     .self$data = DataContainer(data)
                                   },
                                   initialize = function(name=NULL){
                                     callSuper(name)
                                   },
                                   logLike = function(){
                                     min =   cslots[[1]]$compute()
                                     max =   cslots[[2]]$compute()
                                     #  printf("pred:%f,var:%f",pred,var)
                                     if(!.self$data$empty){ ##obsevred likelyhood
                                       singlelikelihoods = dunif(.self$data$data, min, max, log = T)  
                                     }else{##unobseverd take sample at current position
                                       singlelikelihoods = dunif(.self$cvalue, min, max, log = T)  
                                   #    printf("singlelikelihoods:%f,.self$cvalue:%f",singlelikelihoods,.self$cvalue)
                                     }
                                     
                                     sumll = sum(singlelikelihoods)
                                     # printf("sumll:%f",sumll)
                                     return(sumll)    
                                   },
                                   setCurrentValue = function(v){
                                     ##check ranges
                                     min =   cslots[[1]]$compute()
                                     max =   cslots[[2]]$compute()
                                     if(v<min | v>max){
                                       printf('value out of range:%f',v)
                                       return(FALSE)
                                     }
                                     return(callSuper(v) )
                                   },
                                   getBounds = function(){
                                     min =   cslots[[1]]$compute()
                                     max =   cslots[[2]]$compute()
                                     return(c(min,max))
                                   }
                                 )
)


require('extraDistr')


MultinomialDistribution <- setRefClass("MultinomialDistribution",
                                 fields = list(data='DataContainer'),
                                 contains = "Distribution",
                                 methods = list(
                                   setData = function(data){
                                     .self$data = DataContainer(data)
                                   },
                                   initialize = function(name=NULL){
                                     callSuper(name)
                                   },
                                   logLike = function(){
                                     probs = c()
                                     for( i in 1:length(cslots)){
                                       probs =c(probs,cslots[[i]]$compute())
                                     }
                                     probs = matrix(probs, ncol = length(cslots),byrow = F)
                                    # printf("probs:%s",paste(probs,sep='',collapse = ','  ) )
                                   #  printf('dims:%s ndata %s' ,probs,.self$data$data)
                                     if(!.self$data$empty){ ##obsevred likelyhood
                                       singlelikelihoods = dcat(.self$data$data, prob=probs, log = T)  
                                     }else{##unobseverd take sample at current position
                                       singlelikelihoods = dcat(.self$cvalue,  prob=probs, log = T)  
                                       #   printf("singlelikelihoods:%f,.self$cvalue:%f",singlelikelihoods,.self$cvalue)
                                     }
                                     
                                     sumll = sum(singlelikelihoods)
                                     # printf("sumll:%f",sumll)
                                     return(sumll)    
                                   },
                                   getBounds = function(){
                                     #return(c(0,Inf))
                                   },
                                   setCurrentValue = function(v){
                                     #if(v<0){
                                    #   printf('Value ca not be negative for dgamma:%f',v)
                                    #   return(FALSE)
                                    # }
                                     callSuper(v)
                                   }
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
                           ###remove doubel links
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
                           if(nSamples ==1){
                             return(ret)
                           }
                           for( ns in 1:(nSamples-1) ){
                             s1 = walkPlate(.self$model)
                             ret = rbind(ret,unlist(s1))
                           }
                           ret
                         },
                         walkPlate=function(root,parentName=''){
                           samplesList = list()
                           
                           if(class(root)!='Plate'){
                             printf('%s is not a Plate',root$getName()) #what should i do , just return ot what
                           }
                           rootList = root$getNodeList()
                           if(length(rootList)==0){
                             printf('No nodes in%s',root$getName())
                           }
                           for(r in 1:length(rootList)){
                             printf("%d nodes in root",length(rootList))
                             node = rootList[[r]] # this is the likelyhood we can take samples now.
                             
                             ### check if parent is legit to call me
                             if(parentName !=''){
                               if(!node$isLastParentNode(parentName) ){
                                 next
                               }
                             }
                             
                             # we shoudl check for data before taking sample...
                             if(class(node)=='Plate'){
                               printf('node is plate:%s',node$getName())
                             }else{
                               #if(node$data$empty){
                              #   printf('node %s has no data but ok...',node$getName())
                              # }
                             }
                             
                             
                             if(class(node)=='StorageNode'){
                               printf("%s is storage node",node$getName())
                             #  samplesList[[node$getName()]] = node$compute()
                               node$compute()
                               next;
                             }
                             
                             ##for each slot this has to be a distibution
                             if(length(node$slots)==0){
                               printf('No slots in node in %s',node$getName())
                               return(samplesList)
                             }
                             #need to sample from each one
                             printf('going through slots for node:%s',node$getName())
                             for(s in 1:length(node$slots)){
                               #get plate ...
                               printf('testing slot:%d',s)
                               cplate = node$slots[[s]]
                               if(class(cplate)=="Plate"){ #but it doenst need to be, but i could so if it wants evaluation
                                 printf('ok %s is plate!',cplate$getName())
                                 ## we have to sample for each element

                                 cnodes = cplate$getNodeList()
                                 if(length(cnodes)==0 ){
                                   print('empty plate !')
                                   return(samplesList)
                                 }
                                 
                                printf('got %d nodes in this plate:%s',length(cnodes),cplate$getName())
                             #   if(length(cnodes)>0){
                            #      if(class(cnodes[[1]])=='StorageNode' ){
                            #         cnodes = cnodes[[1]]$slots[[1]]$getNodeList()
                            #      }
                            #    }
                                
                                
                                 for(e in 1:length(cnodes)){
                                   ##ok this is the prior node finally. shoudl we check for values ? of if its constand and doesnt contain data
                                   priorNode = cnodes[[e]]
                                 

                                   ##echeck for type palte
                                   if(class(priorNode)=='Plate'){
                                     print('is plate i need to go deeper')
                                     #just call walk plate with this ?
                                     stop('not implemented yet')
                                  #   retslist = walkPlate(cplate,node$getName())
                                     #  printf('returning from walk with samples:%s',retslist)
                                   #  if(length(retslist)>0){
                                  #     samplesList = append( samplesList,retslist)
                                  #   }
                                     next
                                   }
                                 
                                   if(priorNode$data$empty){
                                     printf('thats great we can sample:',priorNode$getName())
                                     ## ok we found that we need to take samples from
                                     
                                     ##not so quick we need to check if we are last parent (or if hasnt been sampled already)
                                     #if more parents than one, we need to give a list of all parents to 
                                     #calculate the likelihood correctly, and avoid sampling twice
                                     if(length(priorNode$parents)>1){
                                       if(priorNode$isLastParentNode(node$getName() ) ){
                                          ##is last
                                         ## call with list of nodes,actually since its double linked list I could also only call with prior
                                         #this would make it less strange
                                         printf('node: %s is last parent of node:%s',node$getName(),priorNode$getName())
                                         retssample = sample(priorNode$parents,priorNode) 
                                       }else{
                                         printf('node: %s is not last parent of node:%s',node$getName(),priorNode$getName())
                                         next;
                                       }
                                     }else{
                                       printf('sample prior: %s prior:%s',node$getName(),priorNode$getName())
                                       retssample = sample(node,priorNode)  
                                     }
                                     
                                     nodeName = priorNode$getName()
                                     samplesList = append( samplesList,retssample)
                                     names(samplesList)[length(samplesList)] =nodeName

                             
                                   }else{
                                     print('node contains data this should be a likelihood of some kind')
                                   }
                                   
                                 }
                                ##walk up one step
                                ##only walk up with last parent
                              
                              
                                retslist = walkPlate(cplate,node$getName())
                                
                              #  printf('returning from walk with samples:%s',retslist)
                                if(length(retslist)>0){
                                  samplesList = append( samplesList,retslist)
                                }
                                 ##ok we sampled platt go one level up
                                 
                               }else{
                                 printf('is not plate:%s',class(cplate) )
                               }
                             }
                           }
                           return(samplesList)
                         },
                         #helper to consider list objects
                         getLikelihood = function(likelihood){
                           likesum = 0
                           if(is.list(likelihood)){
                             for(i in 1:length(likelihood)){
                               likesum = likesum +likelihood[[i]]$logLike()
                             }
                           }else{
                             likesum =likelihood$logLike()
                           }
                           return(likesum)
                         },
                         #### ok now likelihood can be list, which is strange since prior already contains all information needed
                         sample=function(likelihood,prior){
                           if(class(likelihood)=='list' ){
                             printf('taking sample from likelhoods list:prior:%s',prior$getName() )
                             for(i in 1:length(likelihood)){
                               printf("likelihood:%d %s",i,likelihood[[i]]$getName())
                             }
                           }else{
                            printf('taking sample from like:%s and prior:%s',likelihood$getName(),prior$getName() )
                         }
                         ###mcmc step
                         # betanew= beta + t(rmvnorm( 1,rep(0,p),diag(p)*0.1 ))
                         # oldprob = calcProb(X%*%beta,Y,sigma)
                         
                          
                           
                           ###quick hack to test
                           ##
                           
                           ##use this little function to consider type list 
                          oldprob = getLikelihood(likelihood)+prior$logLike() ##current position
                           
                        #  printf('oldprob like:%f',oldprob)
                         
                        #  ll = prior$logLike()
                        #  printf('old prior:%f',ll)
                        #  oldprob = oldprob+ll ##current position
                          
                         #eta = X%*%betanew
                         #newprop = calcProb(eta,Y,sigma)
                           oldvalue = prior$cvalue
                           
                           repeat{
                             newvalue =oldvalue  + rnorm(1,0,0.2)
                             if(prior$setCurrentValue(newvalue)){
                               break
                             }
                           }
                           
                         # printf('old value: %f new value:%f',oldvalue,prior$cvalue )
                          newprop = getLikelihood(likelihood) + prior$logLike()
                        #  printf('like:%f',newprop)
                        #  ll = prior$logLike()
                        #  printf('prior:%f',ll)
                        #  newprop = newprop + ll
                         
                         #  printf('newprop:%f,oldprob:%f',newprop,oldprob )
                           
                         # MH Acceptance Ratio on Log Scale
                           ratio<-(newprop)-(oldprob)
                         #  printf('ratio:%f',ratio)
                           if(log(runif(1))<ratio) {
                             #accept = accept +1
                             #print('accept')
                           }else{
                            # print('reject')
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
                               if(!is.list(likelihood)){
                                 printf('taking sample from like:%s and prior:%s',likelihood$getName(),prior$getName() )
                               }else{
                                 printf('taking sample from for prior:%s with a list of parents',prior$getName() )
                                 for( i in 1:length(likelihood)){
                                   print(likelihood[[i]]$getName())
                                 }
                               }
                             
                               x0 = prior$cvalue
                               
                               f=function(){ ##maybe we dont need to reset because we accept anyway
                                 return(getLikelihood(likelihood)+prior$logLike() )
                               }
                               
                               height =  f()
                               
                               #y = runif(1, 0, exp(height) )    # Take a random y value
                               #y = log(y)
                               y = log(runif(1, 0, 1 )) + height
                               
                               
                             
                               w = 0.1#abs(rnorm(1,0,1))#0.1 # typical slice size
                               m = 10 # integer limiting the size
                               
                               int = estimateIntervalSteppingOut(likelihood,prior,x0,y,w,m)  
                               ###get next value
                               repeat{
                                 printf('bounds: [%f,%f]',int[1],int[2])
                                 r = runif(1, int[1],int[2])
                                 print(r)
                                 if(prior$setCurrentValue(matrix(r)) ){
                                   y_value = f()
                                   if(y_value>y){ ##is ok
                                     printf('%f>%f accept point:%f',y_value,y,r)
                                     break
                                   }
                                 }
                                 printf('we need to adjust:%s',r)
                                 ##point is not OK we have to adjust the slice
                                 if(r <x0){ #more to the left
                                   int[1] = r
                                 }else{
                                   int[2] = r
                                 }
                                 printf('repeat [%s,%s]',int[1],int[2])
                                # if(any(is.na(r))){
                                #   stop()
                                # }
                               }
                               prior$setCurrentValue(r)
                               
                               r
                             },
                              estimateIntervalSteppingOut= function(likelihood,prior,x0,y,w,m){
                                f=function(){ ##maybe we dont need to reset because we accept anyway
                                  return(getLikelihood(likelihood)+prior$logLike())
                                }
                                
                                U = runif(1,0,1)
                                L = x0-w*U
                                R= L+w
                                V = runif(1,0,1)
                                J = floor(m*V)
                                K = (m-1)-J
                                
                                printf('x0:%f starting bound:[%s,%s],J:%f,K:%f',x0,L,R,J,K)
                                while(J>0){
                                  if(! prior$setCurrentValue(L) ){
                                    printf('could not set L value, we get bound ?%f',L)
                                    bounds = prior$getBounds()
                                    print(bounds)
                                    L = bounds[1]
                                    break;
                                  }
                                   fl = f()
                                   print(fl)
                                  
                                   if(fl <y){
                                     break;
                                   }
                                   L = L -w 
                                   J = J-1
                                }
                                
                                while(K>0){
                                  if(! prior$setCurrentValue(R) ){
                                    printf('could not set R value, we get bound ?%f',R)
                                    bounds = prior$getBounds()
                                    R = bounds[2]
                                    break;
                                  }
                                  fr = f()
                                  print(fr)
                                  if(fr <y){
                                    break;
                                  }
                                  R = R +w 
                                  K = K-1
                                }
                                printf('get bounds: [%f,%f]',L,R)
                                  
                                return(c(L,R))
                              }
                             
                           )
)


#sliceSample = SliceSampler(plate_root)

#samplesFromProblem = sliceSample$takeSample(1000)

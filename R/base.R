
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
                                   slots='list',cslots='list',parents='list',type='character' ),
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
                         .self$type = 'continuous'
                         
                         if(is.null(name)){
                           .self$dist_name ='NoName'
                         }else{
                           .self$dist_name = name
                         }
                         .self$cvalue= matrix(runif(1)) ##think about initial values
                       },
                       #returns log likelihood 
                       logLike = function(){
                         print('called base class')
                       },
                       #sets current state, can return false, if you didnt meet the boundaries.
                       setCurrentValue = function(v){
                         .self$cvalue = matrix(v)
                         return(TRUE)
                       },
                       ## returns current value, state of the sampler. you should use ise
                       getCurrentValue = function(v){
                         return(.self$cvalue)
                       }
                       
                       
                       )
                     )


StorageNode <- setRefClass("StorageNode",
                        fields = list(),
                        contains = "Distribution",
                        methods = list(
                          compute = function(){
                            res = .self$cslots[[1]]$compute()
                          #  print(ncol(res))
                            .self$cvalue =  res
                          #  printf('cvalue cols :%d',ncol(.self$cvalue))
                            return(res) 
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
                                      .self$type = 'constant'
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

###

TruncNormalDistribution <- setRefClass("TruncNormalDistribution",
                                  fields = list(data='DataContainer'),
                                  contains = "Distribution",
                                  methods = list(
                                    setData = function(data){
                                      .self$data = DataContainer(data)
                                    },
                                    initialize = function(name=NULL){
                                      callSuper(name)
                                    },
                                    dtruncnorm= function(x,a=-Inf,b=Inf,mean=0,sd=1,log=F){
                                      ret = NA
                                      if(log==TRUE){
                                        ret = dnorm ((x-mean)/sd, log = T) - log( pnorm(b ) - pnorm(a))
                                      }else{
                                        ret = dnorm ((x-mean)/sd) / (pnorm(b) - pnorm(a))
                                      }
                                      ret
                                    },
                                    logLike = function(){
                                      a =   cslots[[1]]$compute()
                                      b =   cslots[[2]]$compute()
                                      pred =   cslots[[3]]$compute()
                                      var =   cslots[[4]]$compute()
                                      if(!.self$data$empty){ ##obsevred likelyhood
                                        singlelikelihoods = .self$dtruncnorm(.self$data$data,a=a,b=b ,mean = pred, sd = var, log = T)  
                                      }else{##unobseverd take sample at current position
                                        singlelikelihoods = .self$dtruncnorm(.self$cvalue,a=a,b=b, mean = pred, sd = var, log = T)  
                                      }
                                      
                                      sumll = sum(singlelikelihoods)
                                      #  printf("sumll:%f",sumll)
                                      return(sumll)    
                                    },
                                    setCurrentValue = function(v){
                                      ##check ranges
                                      min =   cslots[[1]]$compute()
                                      max =   cslots[[2]]$compute()
                                      if(v<min || v>max){
                                       # printf('<%d/%d>value out of range:%f',min,max,v)
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


###

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
                                      if(v<=0){
                                        printf('Value ca not be negative for dgamma:%f',v)
                                        return(FALSE)
                                      }
                                      callSuper(v)
                                    }
                                  )
)


BernoulliDistribution <- setRefClass("BernoulliDistribution",
                                 fields = list(data='DataContainer'),
                                 contains = "Distribution",
                                 methods = list(
                                   setData = function(data){
                                     .self$data = DataContainer(data)
                                   },
                                   initialize = function(name=NULL){
                                     callSuper(name)
                                     .self$type = 'discrete'
                                     .self$cvalue = matrix(sample(c(0,1),1))
                                   },
                                   logLike = function(){
                                     p =   cslots[[1]]$compute()
                                     #  printf("pred:%f,var:%f",pred,var)
                                     if(!.self$data$empty){ ##obsevred likelyhood
                                       singlelikelihoods = dbern( .self$data$data,prob=p,log=T)  
                                     }else{##unobseverd take sample at current position
                                       singlelikelihoods = dbern(.self$cvalue,  prob=p,log=T)  
                                       #   printf("singlelikelihoods:%f,.self$cvalue:%f",singlelikelihoods,.self$cvalue)
                                     }
                                     
                                     sumll = sum(singlelikelihoods)
                                     # printf("sumll:%f",sumll)
                                     return(sumll)    
                                   },
                                   getBounds = function(){
                                     return(c(0,1))
                                   },
                                   setCurrentValue = function(v){
                                     if(v !=1 && v != 0 ){
                                       printf('Value can only be 0/1 for dbern:%f',v)
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
                                     if(v<min || v>max){
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


#require('extraDistr')


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
                                    # printf("length of slots probs:%d",length(cslots))
                                    # if( length(cslots)==0 ){
                                     #  printf('no slots set!')
                                    # }
                                     if(length(cslots)==1 ){
                                       probs = cslots[[1]]$compute()
                                     #  print('ncol(probs)=%d',ncol(probs))
                                      # if(ncol(probs)==1){
                                      #   probs = t(probs)
                                      # }
                                       if(!.self$data$empty){
                                         probs= matrix( probs,nrow= nrow(.self$data$data) , byrow = F)
                                       }else{
                                         probs= matrix( probs,nrow=1 )
                                       }
                                    #   printf("probs:%s",paste(probs,sep='',collapse = ','  ) )
                                     }else{
                                       for( i in 1:length(cslots)){
                                         probs =c(probs,cslots[[i]]$compute())
                                       }
                                       probs = matrix(probs, ncol = length(cslots),byrow = F)
                                     }
                                  #   printf("probs:%s",paste(probs,sep='',collapse = ','  ) )
                                  #   printf('dims:%s ndata %s' ,probs,.self$data$data)
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


###sampler

###containing approximation methods

##
#' Base class for samplers
#'
#' @export Sampler
#' @exportClass Sampler
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


##
#' Implements a basic MCMC sampler
#'
#' @export MCMCsampler
#' @exportClass MCMCsampler
MCMCsampler <- setRefClass("MCMCsampler",
                           contains = 'Sampler',
                           fields = list(),
                           methods = list(
                             initialize = function(model) {
                               callSuper( model)
                               ###remove doubel links
                             },
                             sampleWithNodesList = function(nodesList,additionalList=list()){
                               
                               ret = rep(NA, length(nodesList))
                               # print('sample now!')
                               for(i in 1:length(nodesList)){
                                 node = nodesList[[i]] 
                                 #  print('call sample')
                                 ret[i] =  sample(node$parents,node)
                                 names(ret)[i] =  node$getName()
                               }
                               # print('return now!')
                               if(length(additionalList)>0 ){
                                 for(i in 1:length(additionalList)){
                                   node = additionalList[[i]]
                                   res =  node$compute()
                                   if(length(res)>1){
                                     if(ncol(res)>1 ){
                                       resnrow = nrow(res)
                                       resncol = ncol(res)
                                       res = as.numeric(res)   
                                       names(res) =paste(node$getName(),'[', 1:resnrow ,',' , rep(1:resncol , each=resncol),']' ,sep='')
                                       #paste( rep(paste(node$getName(),'[',1:resnrow ,sep = '') , each=resncol),',' , 1:resncol,']' ,sep='')
                                     }else{
                                       res = res[1:length(res)]   
                                       names(res) = paste(node$getName(), '[',1:length(res),']', sep = '')
                                     }
                                   }else{
                                     names(res) = node$getName()
                                   }
                                   
                                   
                                   
                                   ret =append(ret,res)
                                 }
                               }
                               
                               ret
                             },
                             setInitialValues = function( nodesList, initialValues=list() ){
                               inames =  names(initialValues)
                               for(i in 1:length(nodesList)){
                                 node = nodesList[[i]] 
                                 idx = which(node$getName() == inames)
                                 if(length(idx)!=0){ ##found
                                   if( !node$setCurrentValue(initialValues[[idx]] ) ){
                                     printf('can not set initial value for %s',node$getName() )
                                   }
                                 }
                               }
                             },
                             getAdditionalNodes=function(nodesList,addtionalNodes){
                               isInList=function(nodesList, name){
                                 for(i in 1:length(nodesList)){
                                   node = nodesList[[i]] 
                                   if(node$getName() == name){ ##found
                                     return(TRUE)
                                   }
                                 }
                                 return(FALSE)
                               }
                               getNode=function(plate, name){
                                 rootList = plate$getNodeList()
                                 for(i in 1:length(rootList)){
                                   if(rootList[[i]]$getName()==name){
                                     return(rootList[[i]])
                                   }
                                 }
                                 return(NULL)
                               }
                               retnodesList =list()
                               for(i in 1:length(addtionalNodes)){
                                 if(!isInList(nodesList,addtionalNodes[[i]])){
                                   node = getNode(.self$model,addtionalNodes[[i]])
                                   if(is.null(node)){
                                     printf('node %s not found to watch.',addtionalNodes[[i]])
                                   }else{
                                     printf('node %s found  add to watch.',addtionalNodes[[i]])
                                     retnodesList = append(retnodesList, node)
                                   }
                                 }else{
                                   printf("Node %s is already watched.",addtionalNodes[[i]])
                                 }
                               }
                               retnodesList
                             },
                             takeSample = function(nSamples,initialValues = list(),addtionalNodes=list()){
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
                               
                               
                               ## get a list with likelihood and priors
                               nodesList = walkPlate(.self$model)
                               
                               printf('%s nodes in the model.', length(nodesList))
                               if(length(nodesList)==0){
                                 return(NULL)
                               }
                               
                               if(length(initialValues)>0){
                                 printf('setting initial Values')
                                 setInitialValues(nodesList,initialValues)
                               }
                               if(length(addtionalNodes)>0){
                                 printf('adding additional nodes to watch')
                                 addtionalNodes = getAdditionalNodes(nodesList,addtionalNodes)
                               }
                               
                               # create progress bar
                               total <- nSamples
                               pb <- txtProgressBar(min = 0, max = total, style = 3)
                               
                               start_time <- Sys.time()
                               ##
                               
                               ##go recurrent throut the model.
                               ret = sampleWithNodesList(nodesList,addtionalNodes)      #unlist( walkPlate(.self$model)  )
                               setTxtProgressBar(pb, 1)
                               if(nSamples ==1){
                                 close(pb)
                                 return(ret)
                               }
                               
                               end_time <- Sys.time()
                               
                               timeEstimate = (as.numeric(end_time-start_time) *(nSamples-1 )) / 60
                               if(timeEstimate >60){
                                 printf('Time estimate: %f hours', timeEstimate/60)
                               }else{
                                 printf('Time estimate: %f minutes',timeEstimate)
                               }
                               
                               ##prepare larger matrix
                               ret2 = matrix(, nrow=nSamples,ncol=length(ret))
                               colnames(ret2) = names(ret)
                               ret2[1,] = ret
                               
                               for( ns in 2:(nSamples) ){
                                 s1 = sampleWithNodesList(nodesList,addtionalNodes)  #walkPlate(.self$model)
                                 ret2[ns,] = unlist(s1)
                                 setTxtProgressBar(pb, ns)
                               }
                               close(pb)
                               ret2
                             },
                             walkPlate=function(root,parentName=''){
                               # samplesList = list()
                               nodesList = list()
                               
                               if(class(root)!='Plate'){
                                 printf('%s is not a Plate',root$getName()) #what should i do , just return ot what
                               }
                               rootList = root$getNodeList()
                               if(length(rootList)==0){
                                 printf('No nodes in%s',root$getName())
                               }
                               for(r in 1:length(rootList)){
                                 #   printf("%d nodes in root",length(rootList))
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
                                   #   printf("%s is storage node",node$getName())
                                   #  samplesList[[node$getName()]] = node$compute()
                                   # node$compute()
                                   next;
                                 }
                                 
                                 ##for each slot this has to be a distibution
                                 if(length(node$slots)==0){
                                   printf('No slots in node in %s',node$getName())
                                   # return(samplesList)
                                   return(nodesList)
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
                                       printf('empty plate !')
                                       return(samplesList)
                                     }
                                     
                                     printf('got %d nodes in this plate:%s',length(cnodes),cplate$getName())
                                     #   if(length(cnodes)>0){
                                     #      if(class(cnodes[[1]])=='StorageNode' ){
                                     #         cnodes = cnodes[[1]]$slots[[1]]$getNodeList()
                                     #      }
                                     #    }
                                     
                                     checkHERE= function(priorNode,node,nodesList) {
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
                                             #retssample = sample(priorNode$parents,priorNode) 
                                             nodesList = append(nodesList, priorNode )
                                             
                                           }else{
                                             printf('node: %s is not last parent of node:%s',node$getName(),priorNode$getName())
                                             return(nodesList)
                                             # next;
                                           }
                                         }else{
                                           printf('sample prior: %s prior:%s',node$getName(),priorNode$getName())
                                           #retssample = sample(node,priorNode)
                                           nodesList = append(nodesList, priorNode )
                                           
                                         }
                                         
                                       }else{
                                         printf('node contains data this should be a likelihood of some kind')
                                       }
                                       nodesList
                                     }
                                     
                                     
                                     for(e in 1:length(cnodes)){
                                       ##ok this is the prior node finally. shoudl we check for values ? of if its constand and doesnt contain data
                                       priorNode = cnodes[[e]]
                                       
                                       
                                       ##echeck for type palte
                                       if(class(priorNode)=='Plate'){
                                         printf('is plate i need to go deeper')
                                         stop('not implemented yet')
                                         
                                         next
                                       }
                                       if(class(priorNode)=='ComputationHelperNode'){
                                         for(tz in 1:length(priorNode$cslots)){
                                           nodesList = checkHERE(priorNode$cslots[[tz]],node,nodesList)
                                           # priorNode$cslots[[tz]]$addParentNode(priorNode$parent)
                                         }
                                         next;
                                       }          
                                       
                                       nodesList = checkHERE(priorNode,node,nodesList)
                                     }
                                     ##walk up one step
                                     ##only walk up with last parent
                                     
                                     
                                     retslist = walkPlate(cplate,node$getName())
                                     
                                     
                                     #  printf('returning from walk with samples:%s',retslist)
                                     if(length(retslist)>0){
                                       # samplesList = append( samplesList,retslist)
                                       nodesList = append(nodesList, retslist)
                                     }
                                     ##ok we sampled platt go one level up
                                     
                                   }else{
                                     printf('is not plate:%s',class(cplate) )
                                   }
                                 }
                               }
                               # return(samplesList)
                               return(nodesList)
                             },
                             #helper to consider list objects
                             getLikelihood = function(likelihood){
                               likesum = 0
                               if(is.list(likelihood)){
                                 for(i in 1:length(likelihood)){
                                   likesum = likesum +likelihood[[i]]$logLike()
                                  # print(likelihood[[i]]$logLike())
                                  # print(likelihood[[i]]$getName())
                                 }
                               }else{
                                 likesum =likelihood$logLike()
                               }
                               return(likesum)
                             },
                             #### ok now likelihood can be list, which is strange since prior already contains all information needed
                             sample=function(likelihood,prior){
                               # printf('called once more %s %s', class(prior), class(likelihood))
                               # if(class(likelihood)=='list' ){
                               #   printf('taking sample from likelhoods list:prior:%s',prior$getName() )
                               #   for(i in 1:length(likelihood)){
                               #     printf("likelihood:%d %s",i,likelihood[[i]]$getName())
                               #   }
                               # }else{
                               #  printf('taking sample from like:%s and prior:%s',likelihood$getName(),prior$getName() )
                               # }
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
                                 if(prior$type=='discrete'){
                                   
                                   vals = prior$getBounds()
                                   newvalue = base::sample(vals,1)
                                   #    printf('discrete function, %f',newvalue)
                                 }else{
                                   newvalue =oldvalue  + rnorm(1,0,0.2)
                                 }
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

#' Implements a stepping out slice Sampler
#'
#' @export SliceSampler
#' @exportClass SliceSampler
SliceSampler <- setRefClass("SliceSampler",
                            contains = 'MCMCsampler',
                            fields = list(),
                            methods = list(
                              sample=function(likelihood,prior){
                                #if(!is.list(likelihood)){
                                #   printf('taking sample from like:%s and prior:%s',likelihood$getName(),prior$getName() )
                                # }else{
                                #   printf('taking sample from for prior:%s with a list of parents',prior$getName() )
                                #   for( i in 1:length(likelihood)){
                                #     printf(likelihood[[i]]$getName())
                                #   }
                                # }
                                
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
                                  # printf('bounds: [%f,%f]',int[1],int[2])
                                  r = runif(1, int[1],int[2])
                                  # print(r)
                                  if(prior$setCurrentValue(matrix(r)) ){
                                    y_value = f()
                                    if(y_value>y){ ##is ok
                                      #     printf('%f>%f accept point:%f',y_value,y,r)
                                      break
                                    }
                                  }
                                  #   printf('we need to adjust:%s',r)
                                  ##point is not OK we have to adjust the slice
                                  if(r <x0){ #more to the left
                                    int[1] = r
                                  }else{
                                    int[2] = r
                                  }
                                  #  printf('repeat [%s,%s]',int[1],int[2])
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
                                
                                #    printf('x0:%f starting bound:[%s,%s],J:%f,K:%f',x0,L,R,J,K)
                                while(J>0){
                                  if(! prior$setCurrentValue(L) ){
                                    #     printf('could not set L value, we get bound ?%f',L)
                                    bounds = prior$getBounds()
                                    L = bounds[1]
                                    break;
                                  }
                                  fl = f()
                                  
                                  if(fl <y){
                                    break;
                                  }
                                  L = L -w 
                                  J = J-1
                                }
                                
                                while(K>0){
                                  if(! prior$setCurrentValue(R) ){
                                    #       printf('could not set R value, we get bound ?%f',R)
                                    bounds = prior$getBounds()
                                    R = bounds[2]
                                    break;
                                  }
                                  fr = f()
                                  
                                  if(fr <y){
                                    break;
                                  }
                                  R = R +w 
                                  K = K-1
                                }
                                #  printf('get bounds: [%f,%f]',L,R)
                                
                                return(c(L,R))
                              }
                              
                            )
)


#' Implements a basic maximum likelihood estimation method
#'
#' @export LaplaceApproximation
#' @exportClass LaplaceApproximation
LaplaceApproximation <- setRefClass("LaplaceApproximation",
                            contains = 'MCMCsampler',
                            fields = list(optimIterations = 'numeric'),
                            methods = list(
                              initialize=function(model){
                                callSuper( model)
                                .self$optimIterations <<- 100
                              },
                              sample=function(likelihood,prior){
                                
                                ###setup the 
                                initial = prior$getCurrentValue()
                              
                                  
                                minfunc =function(min){
                                #  printf("set prior:%f",min)
                                  if(!prior$setCurrentValue(min)){
                                    print('error setting value')
                                    return(-10e100)
                                  }
                                
                                  res= getLikelihood(likelihood) + prior$logLike()
                                 
                                }
                                
                                
                                if(prior$type=='discrete'){
                                  
                                  vals = prior$getBounds()
                                  #newvalue = base::sample(vals,1)
                                  if(any(is.infinite(vals)) ){
                                      print("infinite discrete numbers need to implement somthing for this")
                                     stop()
                                    
                                  }
                                   size = vals[2]- vals[1]
                                   ntests =50
                                   if(size<50){
                                     ntests = size
                                     testvals = base::sample(vals[1]:vals[2],ntests,replace = F)
                                   }else{
                                     testvals = vals[1]:vals[2]
                                   }
                                  
                                   resmat = matrix(0,ncol=1,nrow=ntests)
                                   for( k in 1:length(testvals)){
                                     resmat[k] = minfunc(testvals[k])
                                   }
                                   idx = which.max(resmat)
                                   res = testvals[idx]
                                }else{
                                  bounds = prior$getBounds()
                                #print(bounds)
                                
                                  optimres = optim(par =initial, fn= minfunc,method = "L-BFGS-B",
                                                       lower = bounds[1], upper = bounds[2],
                                                       control = list(fnscale = -1,maxit=.self$optimIterations))
                                 if(optimres$convergence!=0){
                                   ##some error occured 
                                    print(optimres$message)
                                 }
                                  res = optimres$par
                                }
                               # printf('set param %f',optimres$par)
                                ##set current values
                                prior$setCurrentValue(res)
                                res
                                
                              }
                              
                            )
)




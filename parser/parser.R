# lexer and parser


DistributionLexer <- setRefClass("DistributionLexer",
                     fields = list(name='character',plainslots='list',slotMembers='list',type='character',hasData='logical',data='matrix',
                                   distrib='Distribution'),
                     methods = list(
                       initialize = function(name="",type="",hasData=FALSE) {
                         .self$name=name
                         .self$type=type
                         .self$hasData = hasData
                       },
                       addPlainSlot=function(p){
                         .self$plainslots = append(.self$plainslots,p)
                       },
                       addSlotMembers=function(p){
                         .self$slotMembers = append(.self$slotMembers,p)
                       },
                       getName =function(){
                         return(.self$name)
                       },
                       setData =function(data){
                         .self$hasData  = T
                         .self$data  = data
                       },
                       createDistribution= function(){
                         if(type=='dnorm'){
                           .self$distrib = NormalDistribution(name=.self$name) #create empty one
                         }else if(type =='dgamma'){
                           .self$distrib = GammaDistribution(name=.self$name)
                         }else if(type =='dunif'){
                           .self$distrib = UniformDistribution(name=.self$name)
                         }else if(type=='Constant'){
                           .self$distrib = Constant(.self$name) #create empty one
                           return()
                         }else{
                           printf('Distribution type:%s unknown !',type)
                           return()
                         }
                         if(hasData){
                           .self$distrib$setData(data)
                         }
                       }
                     )
)

ModelLexer <- setRefClass("ModelLexer",
                                 fields = list(name='character',dists = 'list'),
                                 methods = list(
                                   initialize = function(model_name="") {
                                     .self$name = model_name 
                                   },
                                   appendDistribution = function(dist){
                                     .self$dists = append(.self$dists, dist)
                                   },
                                   findDist = function(name){
                                     if(length(dists)==0){
                                       return(NULL)
                                     }
                                     for(i in 1:length(dists)){
                                       if(dists[[i]]$name==name){
                                         return(dists[[i]])
                                       }
                                     }
                                     return(NULL)
                                   },
                                   chkDistName = function(name){
                                     ret = findDist(name)
                                     return(is.null(ret) )
                                   }
                                 )
)

Lexer <- setRefClass("Lexer",
                             fields = list(model_str='character',data_list='list',model_list='list'),
                             methods = list(
                               getNextToken=function(str,pos){
                                 
                                 delim = c('{','}','(',')','-','*','+','\\','~','=',',')
                                 commentChar = '#'
                                 commentMode = F
                                 cword=''
                                 type='No'
                                 while(pos <= length(str) ){
                                   #  printf('char:%s',str[pos])
                                   #  print(cword)
                                   if(str[pos]=='\n'){ #check new line
                                     #   print('new line')
                                     if(nchar(cword)>0 ){
                                       break;
                                     }
                                     cword = ''
                                     commentMode=F
                                     pos = pos+1 
                                     next()
                                   }
                                   
                                   if(commentMode){
                                     pos= pos+1 
                                     next;
                                   }
                                   if(str[pos]== commentChar){
                                     commentMode=T
                                   }else if(str[pos]== ' '){
                                     ##hit hard delim
                                     if(nchar(cword)>0){
                                       pos=pos+1
                                       break;
                                     }
                                   }else if(any(str[pos]== delim)){
                                     #  print('delim')
                                     #  print(nchar(cword))
                                     #  print(cword)
                                     if(nchar(cword)>0){
                                       #   print('return word')
                                       break;
                                     }
                                     #print(cword)
                                     cword=str[pos] #return character
                                     type= 'DELIM'
                                     pos = pos+1
                                     break
                                   }else{
                                     cword= paste(cword,str[pos], sep = "")
                                   }
                                   pos = pos+1
                                   
                                 }
                                 list('str'=cword,'pos'=pos,'type'=type)
                               },
                               findData =function(name){
                                 if(length(data_list)==0){
                                   print('No data list set. Please set a list with data first')
                                   return(NULL)
                                 }
                                 dnames = names(data_list)
                                 for(i in 1:length(dnames)){
                                   if(name == dnames[i]){
                                     #found
                                     return( data_list[[i]] )
                                   }
                                 }
                                 return(NULL)
                               },
                               initialize = function(model_str="") {
                                   .self$setModelString(model_str) 
                                   .self$model_list = list()
                               },
                               setModelData=function(data_list){
                                 .self$data_list = data_list
                               },
                               setModelString= function(model_str=""){
                                 .self$model_str=model_str
                               },
                               lexModel =function(){
                                 if(nchar(model_str)==0){
                                   print('No model loaded')
                                   return(-1)
                                 }
                                 #
                                 allCharacter = strsplit(model_str, "")[[1]]
                                 cword = ''
                                 i = 1
                                 while(i < length(allCharacter) ){
                                   ret_token = getNextToken(allCharacter,i)
                                   i = ret_token$pos
                                   cword = ret_token$str
                                    if(cword=='model'){
                                       print('found model')
                                       ##extract model and call different lexer for model
                                       start=i-6
                                       end = findNextBlock(allCharacter,start)
                                       printf('start:%d,end:%d',start,end)
                                       mod = createModel(allCharacter[start:end])
                                       if(!is.null(mod)){
                                         ##append
                                         .self$model_list= append(.self$model_list,mod)
                                       }
                                       i = end ##skip this text
                                     }else{
                                       printf('unknown token%s',cword)
                                     }
                                   
                                   i = i+1
                                 }
                                 
                               },
                               #lex some model
                               createModel=function(str){
                                 newmodel =NULL
                                 print(str)
                                 ret_token = getNextToken(str,1)
                                 token = ret_token$str
                                 if(token !='model'){
                                   printf('first token is not model! check:%s',token)
                                 }
                                 ret_token = getNextToken(str,ret_token$pos)
                                 token = ret_token$str
                                 if(token!='{'){
                                   modelName = token #set model name
                                   ret_token = getNextToken(str,ret_token$pos)
                                   print(ret_token$pos)
                                   token = ret_token$str
                                   if(token!='{'){
                                     printf('Where is model openning {! check:%s',token)
                                   }
                                 }else{
                                   modelName = 'NoModelName'
                                 }
                                 printf('Parsing model:%s',modelName)
                                 i = 100
                                 newmodel = ModelLexer(modelName)
                                 
                                 state = 0 #
                                 while(nchar(token)>0){
                                    ret_token = getNextToken(str,ret_token$pos)
                                    token = ret_token$str
                                    printf('token:%s pos%d',token,ret_token$pos)
                                    if(state ==0){
                                      tokenName = token;
                                      state = 1
                                    }else if(state ==1){
                                      whatType = token
                                      if(token=='~'){ #oh its a distibution
                                        printf("%s is distib",tokenName)
                                        state = 2
                                      }else if(token=='='){ # assignemnt some compvariable.
                                        printf("%s is cn",tokenName)
                                        state = 3
                                      }else{
                                        printf('unknown type:%s for:%s in model%s',token,tokenName,modelName)
                                      }
                                      
                                    }else if(state ==2){ ##get type of distrib
                                      if(any(token==c('dnorm','dgamma','dunif') )){
                                        print('type dnowm but could be independen of that')
                                        #normal dist.
                                        #now get included terms and slots.
                                        #have to check if already exist with the same name !
                                         if(!newmodel$chkDistName(tokenName)){
                                           printf('There aready is a distribution with the name:%s',token)
                                         }
                                         nd =  DistributionLexer(tokenName,token)
                                         ret_token = getNextToken(str,ret_token$pos)
                                         token = ret_token$str
                                         if(token !='('){
                                           printf('expecting openen (, got:%s',token)
                                         }
                                         ##read each slot
                                         plainText = ''
                                         variableNames = c()
                                         while(nchar(token)>0 & token!=')'){
                                           ret_token = getNextToken(str,ret_token$pos)
                                           token = ret_token$str
                                           printf('reading slots token:%s pos%d',token,ret_token$pos)
                                           if(token!=')' & token!=','){
                                             plainText = paste(plainText,token,sep='')
                                           }
                                          # printf('plainText%s',plainText)
                                           if(ret_token$type!='DELIM'){ # variable
                                             variableNames = c(variableNames,token )
                                           }
                                           if(token==','| token==')'){ #next slot
                                             print('new slot')
                                             variableNames = unique(variableNames)
                                             printf('add plaintext:%s',plainText)
                                             nd$addPlainSlot(plainText)
                                             nd$addSlotMembers(list(variableNames) )
                                             plainText = ''
                                             variableNames = c()
                                           }
                                         }
                                         #add all slots or so and reset state
                                         newmodel$dists= append(newmodel$dists,nd)
                                        # .self$model_list = append(.self$model_list,newmodel)
                                         state = 0
                                         print('reset state')
                                      }else{
                                        printf('unknown distribution:%s',token)
                                      }
                                      
                                    }
                                    
                                    i = i-1
                                    if(i==0){
                                      print('emergency break')
                                      break
                                    }
                                 }
                                 newmodel
                               },
                               parseModel = function(){
                                 if(length(model_list)==0){
                                   printf('No model found. Call lexModel() first')
                                   return(FALSE)
                                 }
                                 ## fist check for data field and things and build up a tree.
                                 
                                 ## check and set all data values.

                                 for(k in 1:length(model_list)){
                                   cntObserved = 0
                                   rootNode = NULL
                                   cmodel = .self$model_list[[k]]
                                   dists = cmodel$dists
                                   printf('start with model:%s',cmodel$name)
                                   for(i in 1:length(dists)){
                                     cd = dists[[i]]
                                     #check for data
                                     datalink = findData(cd$getName())
                                     if(!is.null(datalink)){ #found data
                                       cd$setData(datalink)
                                       rootNode = cd
                                       cntObserved= cntObserved +1
                                     }
                                     ##create all distributions
                                     cd$createDistribution()
                                     ##and do more ?
                                   }
                                   if(cntObserved==0){
                                     print('No node with observed data thats odd !')
                                   }else if(cntObserved==1){
                                     print('One observed Node, this will be used as root of the model')
                                   }else{
                                     print('Multible nodes with data observed, choosing first for root')
                                   }
                                   
                                   ###ok create model
                                   
                                   plate_root = Plate(cmodel$name) ##create root plate
                                 
                                   plate_root$addNode(rootNode$distrib) ##add root node
                                   
                                   for(i in 1:length(dists)){
                                     print(length(dists))
                                     cd = dists[[i]]
                                     if(cd$type == 'Constant'){
                                       next;
                                     }
                                     printf('processing:%s',cd$name)
                                     ## for each node create the links to child nodes and cnodes.
                                     #print(class(cd))
                                     for(x in 1:length(cd$slotMembers)){
                                       print(length(cd$slotMembers))
                                       printf('slotmembers in total:%s',cd$slotMembers[[x]])
                                       step_plate = Plate(paste('slot_', cd$name,as.character(x),sep=''))
                                       
                                       for(s in 1:length(cd$slotMembers[[x]])){
                                         printf('slotmember:%s',cd$slotMembers[[x]][s])
                                         cslotName = cd$slotMembers[[x]][s]
                                         
                                         #check for data
                                         isData = findData(cslotName)
                                         if(!is.null(isData)){
                                           printf('is data:%s',cslotName)
                                           next;
                                         }
                                         
                                         #check for constant
                                        #is a constant.....
                                        modelPtr =  cmodel$findDist(cslotName)
                                        if(is.null(modelPtr)){
                                          res = as.numeric(cslotName)
                                          if(!is.na(res)){## is constant
                                            printf('Create constant dist:%s',cslotName)
                                            #modelPtr = Constant(cslotName)
                                            modelPtr= DistributionLexer(cslotName,'Constant')
                                            modelPtr$createDistribution()
                                            cmodel$appendDistribution(modelPtr)
                                          }else{
                                            printf("Problem can not resolve:%s",cslotName)
                                          }
                                          
                                        }else{
                                           printf("%s is distrib",cslotName)
                                         }
                                         modelPtr = modelPtr$distrib
                                         
                                         if(class(modelPtr)!='Constant'){
                                           print('add !!!!!!')
                                           step_plate$addNode( modelPtr) 
                                         }else{
                                           if(length(cd$slotMembers[[x]])==1 ){ #only 1 constant
                                             print('only one constant')
                                             printf('set compu slot for: %s , %d',cd$distrib$dist_name, x)
                                             cd$distrib$slots[[x]]=modelPtr
                                             cd$distrib$cslots[[x]]= ComputationNodeRef(modelPtr)
                                           }
                                         }
                                         
                                       }##nd all memver slows
                                       #
                                       #set slot if plate
                                       if(step_plate$getNumberOfNodes()>0){
                                         cd$distrib$slots[[x]] = step_plate
                                       }
                                       
                                     }
                                     
                                     
                                   }
                                   
                                   ###next we have to do the computation step
                                   ##
                                   printf('get computation step')
                                   
                                   cparser = ParseComputation(.self,k)
                                   
                                   for(i in 1:length(dists)){
                                     cd = dists[[i]]
                                     if(cd$type == 'Constant'){
                                       next;
                                     }
                                     printf('processing:%s',cd$name)
                                     for(x in 1:length(cd$slotMembers)){
                                       
                                     
                                       printf('slotmembers in total:%s',cd$slotMembers[[x]])
                                       print(length(cd$distrib$cslots))
                                       add=FALSE
                                       if(length(cd$distrib$cslots)<x ){
                                         add = TRUE
                                       }else if(is.null(cd$distrib$cslots[[x]])){
                                         add = TRUE
                                       }
                                       if(add){
                                         printf('need to caculcate computation.%s',cd$plainslots[[x]])
                                         printf('need compu slot for: %s , %d',cd$distrib$dist_name, x)
                                         cnode = cparser$parse(cd$plainslots[[x]])
                                         cd$distrib$cslots[[x]] = cnode
                                      #   print(is.null(cd$distrib$cslots[[x]]))
                                       #  print(length(cd$distrib$cslots))
                                       }
                                       
                                       
                                     }
                                   }
                          
                                   ##next model now
                                   
                                   }###all models are done
                                 return(plate_root)
                               },
                               findNextBlock = function(str,pos){
                                 active = F
                                 open = 0
                                 for(pos in 1:length(str)){
                                   cc = str[pos]
                                   if(cc=='{'){
                                     open = open+1
                                     active = T
                                   }else if(cc=='}'){
                                     open = open-1
                                   }
                                   if(active & open==0){
                                     return(pos)
                                   }
                                 }
                                 return(-1)
                               }
                               
                             )
)


#data_list = list('y'=y,'x'=x)

#lex = Lexer()
#lex$setModelString(my_model_str)
#lex$setModelData(data_list)

#lex$lexModel()

#root_plate = lex$parseModel()

#lex$dists
                
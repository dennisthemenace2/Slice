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
                         }else if(type == 'dcat'){
                           .self$distrib = MultinomialDistribution(name=.self$name)
                         }else if(type=='Constant'){
                           .self$distrib = Constant(.self$name) #create empty one
                           return()
                         }else if(type == 'storage'){
                           .self$distrib = StorageNode(.self$name) #create empty one
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
                                      # ret = Lexer()$getIndex(name)
                                       #if(!is.null(ret)){
                                        # if(ret$name == dists[[i]]$name){
                                           ##append somwhat
                                         #  printf('givig you %s instead ',dists[[i]]$name)
                                          
                                      #      #return(dists[[i]])
                                       #  }
                                       #}
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
                                 
                                 delim = c('{','}','(',')','-','*','+','/','~','=',',')
                                 commentChar = '#'
                                 commentMode = F
                                 openBracket = F
                                 cword=''
                                 type='No'
                                 while(pos <= length(str) ){
                                    # printf('char:%s',str[pos])
                                    # print(cword)
                                   if(str[pos]=='['){
                                     openBracket = T
                                   }else if(str[pos]==']'){
                                     openBracket = F
                                   }
                                     
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
                                   }else if(any(str[pos]== delim)& !openBracket ){
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
                                 ##cheack for array
                                 index = getIndex(name)
                                 if(!is.null(index)) {
                                  # printf('need to get index:%s',name)
                                   name = index$name
                                 }
                                 
                                 dnames = names(data_list)
                                 for(i in 1:length(dnames)){
                                   if(name == dnames[i]){
                                     #found
                                     if(!is.null(index)) {
                                       if(!is.list(data_list[[i]])){
                                         printf('%s is not of type list! type list is indicated by the array!',name)
                                       }
                                       data = data_list[[i]]
                                       for(k in 1:length(index$index)){
                                         data = data[[as.numeric(index$index[k])]]
                                       }
                                       return(data)
                                     }else{
                                       return( data_list[[i]] )
                                     }
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
                               hasSubstring=function(str,text){
                                 
                                 ret_token = c()
                                 ret_token$pos = 1
                                 token = 'dummy'
                                # printf('text:%s',text)
                                 text = strsplit(text,"")[[1]]
                                 while(nchar(token)>0){
                                   ret_token = getNextToken(text,ret_token$pos)
                                   token = ret_token$str
                                   if(token==str){
                                     return(TRUE)
                                   }
                                   ##check for speacial case
                                  # printf('token:%s',token)
                                   ret = getIndex(token)
                                   if(!is.null(ret)){ ##ok token has index
                                     if(ret$name == str){
                                        ####ok we found it we replace this stuff
                                        return(TRUE)
                                     }
                                   }
                                 }
                                 return(FALSE)
                               },
                               replaceSubstring=function(str,replace,text){
                                 ret_token = c()
                                 ret_token$pos = 1
                                 token = 'dummy'
                                 #printf('text:%s',text)
                                 text = strsplit(text,"")[[1]]
                                 return = c()
                                 while(nchar(token)>0){
                                   ret_token = getNextToken(text,ret_token$pos)
                                   token = ret_token$str
                                   if(token==str){
                                     return =c(return,replace)
                                   }else{
                                     ###special case again
                                     ret = getIndex(token)
                                     if(!is.null(ret)){ ##ok token has index
                                       if(ret$name == str){
                                         ####ok we found it we replace this stuff
                                         return =c(return,replace) ##butt keep the index
                                         return =c(return,paste('[' , 
                                                                paste(unlist(ret$index) ,sep = '',collapse = ',' ) 
                                                                ,']',sep='', collapse = '' )
                                                   )
                                         next;
                                       }
                                       
                                     }
                                     
                                      return = c(return,token)
                                     
                                   }
                                 }
                                 return(paste(return,sep='',collapse = ''))
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
                               addParsedistribtuion =function(newmodel,tokenName,token,ret_token,str,forStates=NULL,isStorageNode=F){
                                 
                                 if(!newmodel$chkDistName(tokenName)){
                                   printf('There aready is a distribution with the name:%s',token)
                                 }
                                 if(!isStorageNode){
                                   nd =  DistributionLexer(tokenName,token)
                                   ret_token = getNextToken(str,ret_token$pos)
                                   token = ret_token$str
                                   if(token !='('){
                                     printf('expecting openen (, got:%s',token)
                                   }
                                 }else{
                                   nd =  DistributionLexer(tokenName,'storage')
                                   ##only till end of line and revert last token 
                                   newlines = which(str=='\n') 
                                   str=str[1:(newlines[which(newlines >= ret_token$pos)[1]] )  ]
                                   str[length(str)] = ')'
                                   
                                 
                                   prevAssigment = which(str=='=')
                                  prevToken = which( prevAssigment < ret_token$pos  )
                                   ret_token$pos =prevAssigment[length(prevToken)]+1
                                 #    print(prevAssigment[length(prevToken)]+1)
                                   printf('str:%s',paste(str,collapse = '',sep='' ) )
                              #     ret_token$pos = ret_token$pos - nchar(token)
                                   printf("pos:%d ,prevToken=%s", ret_token$pos , token)
                                #   print
                                   
                                 }
                                 
                                 ##read each slot
                                 openingCnt = 1
                                 plainText = ''
                                 variableNames = c()
                                 prevToken = ''
                                 while(nchar(token)>0 & openingCnt>0 ){
                                   ret_token = getNextToken(str,ret_token$pos)
                                   token = ret_token$str
                                   printf('reading slots token:%s pos%d',token,ret_token$pos)
                                   ### need to check for [] so that we can take care of loops and stuff
                                   ## i handle arrayies in a strange way
                                   index = getIndex(token)
                                   if(!is.null(index) ){
                                     ##it is an array i need to handle that
                                     ##check variable.
                                     printf('processing index for for:%s',index$name)
                                     
                                     currentValue= c()
                                     for(idx in 1:length(index$index)){
                                       cvalue = NA
                                       if(!is.null(forStates) & length(forStates) > 0){
                                         printf('serachign for the for here')
                                         ###we have to take care that these could be equations
                                         
                                         tmp_pos = 1
                                         tmp_tokenstr= 'dummy'
                                         nextString = ''
                                         tmp_str = strsplit(index$index[idx], "")[[1]]
                                         while(nchar(tmp_tokenstr)>0){
                                           tmp_token  = getNextToken(tmp_str,tmp_pos)
                                           
                                           tmp_pos = tmp_token$pos
                                           tmp_tokenstr = tmp_token$str
                                           cvalue = tmp_tokenstr
                                          printf('tmp_tokenstr:%s',tmp_tokenstr)
                                           for(z in 1:length(forStates)){
                                             if(forStates[[z]]$name==tmp_tokenstr ){
                                               cvalue = forStates[[z]]$current
                                               break;
                                             }
                                           }
                                           nextString = append(nextString, cvalue)
                                         }
                                         index$index[idx] = paste(nextString, sep='',collapse = '')
                                         cvalue=  index$index[idx]
                                         printf('new cvalue:%s',cvalue)
                                       }
                                       
                                      # if(is.na(cvalue)){
                                         cvalue = as.numeric(index$index[idx])
                                       #}
                                       
                                       currentValue = append(currentValue, cvalue)
                                       printf('found:%s value:%s',index[idx],cvalue)
                                       #if(length(index$index) >1 & idx<length(index$index)){
                                      #   currentValue = append(currentValue, ',')
                                      # }
                                     }
                                     if(any(is.na(currentValue) ) ){
                                       printf('couldnt find current value for array:%s',index$index)
                                       #is numeric ?
                                       idx = which(is.na(currentValue))
                                       for(i in idx){
                                         currentValue[i] = resolveIndex(index$index[i])
                                         if(is.null(currentValue[i])){
                                           ##its not numeric, is it data ?
                                           printf('could not reslove the variable:%s',index$index[i])
                                         }
                                       }
                                     }
                                     if(length(currentValue)>1){
                                       tmp = currentValue
                                       currentValue =c()
                                       for(i in 1:length(tmp)){
                                         currentValue = append(currentValue,tmp[i] )
                                         if(i<length(tmp)){
                                           currentValue = append(currentValue,',')
                                         }                     
                                       }
                                     }
                                     #print(index)
                                     token = paste(index$name,'[', currentValue, ']',sep='',collapse = '' )
                                     
                                     printf('new token:%s',token)
                                   }
                                   
                                   # printf('plainText%s',plainText)
                                   if(ret_token$type!='DELIM'){ # variable
                                     variableNames = c(variableNames,token )
                                   }
                                   ##try to resolve if its somehting to fo with for indices
                                   if(!is.null(forStates) & length(forStates)>0){
                                     printf('seaching for for in lst')
                                     for(z in 1:length(forStates)){
                                       if(forStates[[z]]$name==token ){
                                         printf('%s is for state and constant:%d',token, forStates[[z]]$current)
                                         token = as.character(forStates[[z]]$current)
                                         break;
                                       }
                                     }
                                   }
                                   
                                   if(token==')' ){
                                     openingCnt = openingCnt -1
                                   }else if(token=='(' ){
                                     openingCnt= openingCnt +1
                                     sprintf('%s opening ( found need to check for function',prevToken)
                                     ##check if previous was funtionname if so remove
                                     if(exists(prevToken)){
                                       if(is.function(eval(base::parse(text=prevToken))) ){
                                         sprintf('%s is function name lets remove this from slots',prevToken)
                                         if(length(variableNames)>0){
                                           variableNames = variableNames[-length(variableNames)]
                                         }
                                       }
                                     }
                                     
                                   }
                                   if(nchar(token)>0 & token!=',' & openingCnt>0){
                                     plainText = paste(plainText,token,sep='')
                                   }
                                   
                                   if(token==',' | openingCnt==0 ){ #next slot
                                     if(openingCnt >1){
                                       plainText = paste(plainText,token,sep='')
                                       next;
                                     }
                                     print('new slot')
                                     variableNames = unique(variableNames)
                                     printf('add plaintext:%s',plainText)
                                     printf('add slotmembers:%s',variableNames)
                                     
                                     nd$addPlainSlot(plainText)
                                     nd$addSlotMembers(list(variableNames) )
                                     plainText = ''
                                     variableNames = c()
                                   }
                                   prevToken = token
                                 }
                                 
                                 #add all slots or so and reset state
                                 newmodel$dists= append(newmodel$dists,nd)
                                 
                                 return(ret_token)
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
                                 
                                 ##used to model for loops
                                 forCnter = 0
                                 forStates = list() 
                                 forListElem = list()
                                 ########################
                                 
                                 state = 0 #
                                 while(nchar(token)>0){
                                    ret_token = getNextToken(str,ret_token$pos)
                                    token = ret_token$str
                                    printf('token:%s pos%d ,state:%d',token,ret_token$pos,state)
                                    if(state ==0){
                                      if(token=='}'){ #something is closed
                                        if(forCnter>0){
                                          print('closing for')
                                          forStates = forStates[-forCnter]
                                          forCnter = forCnter-1
                                          next;
                                        }else{
                                          print('closing model !')
                                          next;
                                        }
                                      }
                                      tokenName = token;
                                      
                                      state = 1
                                    }else if(state ==1){
                                      #whatType = token
                                      if(token=='~'){ #oh its a distibution
                                        printf("%s is distib",tokenName)
                                        state = 2
                                      }else if(token=='='){ # assignemnt some compvariable.
                                        printf("%s is cn",tokenName)
                                        state = 3
                                      }else if(token=='('){
                                        ##check for for
                                        if(tokenName == 'for'){ ##found a for statement
                                          print('initt for loop')
                                          state = 4 #for state 
                                          forCnter = forCnter+1 ##open for
                                          forListElem = list('name'='','numbers'='','current'=NA)
                                        }else{
                                          printf('%s unknown ctr seq',tokenName)
                                        }
                                        
                                      }else{
                                        printf('unknown type:%s for:%s in model%s',token,tokenName,modelName)
                                        if(state ==1){ ##might be something like an multi
                                          tokenName = paste(tokenName, token,sep = '')
                                          printf('append to tokenName:%s',tokenName)
                                        }
                                      }
                                      
                                    }else if(state ==2 | state ==3){ ##get type of distrib, lets handle comput nodes same way
                                      ##consider compute nodes here too
                                      if(state==3){
                                        printf('is computation node')
                                      }
                                      
                                      if(any(token==c('dnorm','dgamma','dunif','dcat') | state ==3 )){
                                        print('parse distribution...')
                                        #normal dist.
                                        #now get included terms and slots.
                                        #have to check if already exist with the same name !
                                        printf('tokenName %s',tokenName)
                                        index = getIndex(tokenName)
                                        if(!is.null(index)){
                                          
                                          name = index$name
                                          index_all = index$index
                                          print(index_all)
                                          numbersList = list()
                                          allForIdx = c()
                                          for(idex in 1:length(index_all) ){
                                            numbers = NULL
                                            index = index_all[idex]
                                            print(index)
                                            forIdx = 0
                                            if(!is.null(forStates) & length(forStates)>0){ ##might be for index
                                              printf('for is not null:%d',length(forStates))
                                              for(z in 1:length(forStates)){
                                                if(forStates[[z]]$name==index ){
                                                  printf('found:%s',index)
                                                  numbers =forStates[[z]]$numbers
                                                  forIdx = z
                                                  break;
                                                }
                                              }
                                            }
                                            
                                            if(is.null(numbers)){
                                              printf('variable is not index, try to resolve: %s',index)
                                              numbers =resolveIndex(index)
                                              if(is.null(numbers)){
                                                printf('could not resolce index:%s',index)
                                              }else{
                                                if(state==3){
                                                 # numbers = numbers
                                                }else{
                                                  numbers = 1:numbers
                                                }
                                              }
                                            }
                                            numbersList = append(numbersList,list(numbers) )
                                            allForIdx = c(allForIdx,forIdx)
                                          } ## all numbers resolved
                                          
                                          printf('numbersList%s',numbersList)
                                          ##init all numbers
                                          theseNumbers = c()
                                          for(nl in 1:length(numbersList)){
                                            numbers = numbersList[[nl]]
                                            theseNumbers =c(theseNumbers,numbers[1] )
                                          }
                                          posNumbers = rep(1,length(theseNumbers))
                                          increase = T
                                          while(increase ==T){
                                            ##set for states
                                            for(nl in 1:length(numbersList)){
                                              numbers = numbersList[[nl]]
                                              forIdx = allForIdx[nl]
                                              if(forIdx>0){
                                                forStates[[forIdx]]$current = theseNumbers[nl]
                                              }
                                            }
                                            num = paste(theseNumbers, sep='',collapse = ',')
                                            tokenName = paste(name, '[', num,']', sep='' , collapse = '')
                                            printf('create:%s token:%s tokenPos:%s',tokenName, token,as.character(ret_token$pos) )
                                            tt = addParsedistribtuion(newmodel,tokenName,token,ret_token,str,forStates,state==3)
                                            ##increase
                                            increase=F
                                            for(nl in 1:length(numbersList)){
                                              if(posNumbers[nl]<length(numbersList[[nl]])){
                                                posNumbers[nl] = posNumbers[nl]+1
                                                theseNumbers[nl] = numbersList[[nl]][ posNumbers[nl]]
                                                increase=T
                                                printf('is smaller increase%d',nl)
                                                break;
                                              }else{ ##trans
                                                printf('transition %d',nl)
                                                posNumbers[nl] = 1
                                                theseNumbers[nl] = numbersList[[nl]][1]
                                              }
                                            }
                                            printf('theseNumbers:%s',paste(theseNumbers, sep='',collapse = ','))
                                          }
                                          
                                    #      for(nl in 1:length(numbersList)){
                                     #       printf('length(numbersListl):%d',length(numbersList) )
                                          #  for(nlrun in 1:length(numbersList)){
                                           # }
                                              
                                      #      numbers = numbersList[[nl]]
                                      #      forIdx = allForIdx[nl]
                                      #      for(icx in 1:length(numbers)){
                                      #        num = numbers[icx]
                                      #        if(forIdx>0){
                                      #          forStates[[forIdx]]$current = icx
                                      #        }
                                             
                                      #        tokenName = paste(name, '[', num,']', sep='' , collapse = '')
                                      #        printf('create:%s',tokenName)
                                      #        tt = addParsedistribtuion(newmodel,tokenName,token,ret_token,str,forStates,state==3)
                                      #      }
                                      #    }
                                          ret_token = tt
                                        }else{
                                         ret_token = addParsedistribtuion(newmodel,tokenName,token,ret_token,str,isStorageNode=state==3)
                                        }
                                        # .self$model_list = append(.self$model_list,newmodel)
                                         state = 0
                                         print('reset state')
                                         
                                      }else{
                                        printf('unknown distribution:%s',token)
                                        
                                      }
                                      
                                    }else if(state == 4){ ##read bound and variables for 'for'
                                      printf('for loop read token: %s',token)
                                      
                                      if(token==')'){
                                        print('for loop ends and initialized')
                                        forListElem$numbers = evalTokenBound(forListElem$numbers)
                                        printf('for loop bounds:%s',forListElem$numbers )
                                        if(length(forStates)==0){
                                          forStates = list(forListElem)
                                        }else{
                                          forStates = append(forStates,list(forListElem) )
                                          
                                        }
                                        state = 5 ##expect opening
                                        next;
                                      }
                                      
                                      if(token != 'in' ){
                                        if(forListElem$name==''){
                                          printf('for loop variable:%s',token)
                                          forListElem$name = token ##set name
                                        }else{
                                          forListElem$numbers = paste(forListElem$numbers ,token,sep = '')
                                        }
                                      }
                                    }else if(state ==5){
                                      if(token == '{'){
                                        print('got opening bracket continue')
                                        state = 0
                                      }else{
                                        printf('expecting { got:%s',token)
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
                               isStorageNode = function(node){
                                 return(class(node) == 'StorageNode')
                               },
                               getIndex=function(token){
                                 chars =strsplit(token, "")[[1]]
                                 if(length(chars)==0){
                                   return(NULL)
                                 }
                                 start = 0
                                 end = 0
                                 for(i in 1:length(chars)){
                                   if(chars[i] =='['){
                                     start = i+1
                                     next
                                   }
                                   if(chars[i]==']'){
                                    end = i-1    
                                   }
                                 }
                                 if(start==0 & end ==0){
                                   ##its not an array
                                   return(NULL)
                                 }
                                 #check for commas
                                 index = paste(chars[start:end], sep='',collapse='')
                                 index =strsplit(index, ",")[[1]]
                               #  printf('get index%s',index)
                                 list('index'=index,
                                      'name' = paste(chars[1:(start-2)],sep='' ,collapse = '')  )
                               },
                               getDataOrNumeric =function(str){
                                 res1 =suppressWarnings(  as.numeric(str) )
                                 if(is.na(res1) ){
                                   res1=findData(str)
                                   if(is.null(res1)){
                                     printf('data %s not found',str)
                                     return(NULL)
                                   }
                                 }
                                 res1
                               },
                               evalTokenBound =function(token){ ##function
                                 #check for ':'
                                 printf('evalTokenBound:%s',token)
                                 split = strsplit(token,':')[[1]]
                                 if(length(split)==2){
                                  # printf('split:%s',split)
                                   start = getDataOrNumeric(split[1])
                                   end = getDataOrNumeric(split[2])
                                   return(start:end)
                                 }else{
                                   printf('problem processing token:%s',token)
                                 }
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
                                   rootNode = list()
                                   cmodel = .self$model_list[[k]]
                                   dists = cmodel$dists
                                   printf('start with model:%s',cmodel$name)
                                   for(i in 1:length(dists)){
                                     cd = dists[[i]]
                                     #check for data
                                     datalink = findData(cd$getName())
                                     if(!is.null(datalink)){ #found data
                                       printf('%s has data',cd$getName())
                                      # print(datalink)
                                       cd$setData(datalink)
                                       rootNode = append(rootNode,cd)
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
                                    # rootNode = 
                                   }else{
                                     print('Multible nodes with data observed, choosing first for root')
                                   }
                                   ###ok create model
                                   
                                   plate_root = Plate(cmodel$name) ##create root plate
                                   ##do i have to add all to root
                                   if( length(rootNode) > 0){
                                     for(i in 1:length(rootNode)){
                                       plate_root$addNode(rootNode[[i]]$distrib) ##add root node
                                     }
                                   }
                                   
                                   ###reaplce storage node names
                                   for(i in 1:length(dists)){
                                     cd = dists[[i]]
                                     storageName =  cd$distrib$getName()
                                     if(class( cd$distrib) =='StorageNode'){
                                       plate_root$addNode(cd$distrib)
                                       strToReplace = cd$plainslots[[1]]
                                       strToReplace = paste('(',strToReplace,')',sep='') ##use breakets to keep order
                                       
                                       for(z in 1:length(dists)){
                                         if(i==z){
                                           next;
                                         }
                                         cd2 = dists[[z]]
                                         for(x in 1:length(cd2$plainslots) ){
                                           printf("storageName:%s , name: %s,plainslot:%s",storageName, cd2$getName(),cd2$plainslots[[x]])
                                           if(hasSubstring(storageName, cd2$plainslots[[x]]) ){
                                           #  cd2$plainslots[[x]] = gsub(storageName, strToReplace, cd2$plainslots[[x]],fixed = T)
                                             cd2$plainslots[[x]] =  replaceSubstring(storageName, strToReplace, cd2$plainslots[[x]])
                                             printf('replacing %s in %s with %s',storageName,cd2$getName(), strToReplace)
                                             printf('plainslot now:%s',cd2$plainslots[[x]])
                                             printf('new slots to add:%s',cd$slotMembers[[1]])
                                             for( r in 1:length(cd2$slotMembers[[x]] ) ){
                                               
                                               if(cd2$slotMembers[[x]][r]==storageName ){
                                                 printf('removing for%s',cd2$getName())
                                                 print(cd2$slotMembers[[x]])
                                                 cd2$slotMembers[[x]]= append(cd2$slotMembers[[x]],cd$slotMembers[[1]] )
                                                 cd2$slotMembers[[x]] = unique( cd2$slotMembers[[x]][-r] ) 
                                                 print(cd2$slotMembers[[x]])
                                                 
                                               }else{
                                                  ##always these speacial cases
                                                 ret = getIndex(cd2$slotMembers[[x]][r])
                                                 if(!is.null(ret)){
                                                   if(ret$name ==storageName ){
                                                     newName = paste(cd$slotMembers[[1]], 
                                                                     '[', ret$index,']' ,sep='',collapse = '')
                                                     
                                                     cd2$slotMembers[[x]]= append(cd2$slotMembers[[x]], newName )
                                                     cd2$slotMembers[[x]] = unique( cd2$slotMembers[[x]][-r] ) 
                                                   }
                                                 }
                                               }
                                             }
                                           }
                                           
                                         }
                                         
                                       }   
                                     } 
                                   }
                                   
                               
                                   
                                   for(i in 1:length(dists)){
                                     print(length(dists))
                                     cd = dists[[i]]
                                     if(cd$type == 'Constant'){
                                       next;
                                     }
                                     printf('processing:%s',cd$name)
                                     ## for each node create the links to child nodes and cnodes.
                                     #print(class(cd))
                                     ###remove double slormembers
                                     if(length(cd$slotMembers)>1){
                                       print('checking slots')
                                       already = cd$slotMembers[[1]]
                                       printf('members first 1:%s',paste(cd$slotMembers[[1]] ,sep='',collapse = ',' ) )
                                       
                                       for(x in 2:length(cd$slotMembers) ){
                                         printf('members first %d:%s',x,paste(cd$slotMembers[[1]] ,sep='',collapse = ',' ) )
                                         
                                         inter =  intersect(already,cd$slotMembers[[x]])
                                         if(length(inter)>0){
                                           for(ks in 1:length(inter)){
                                             cd$slotMembers[[x]] = cd$slotMembers[[x]][-which(cd$slotMembers[[x]]==inter[ks] ) ]
                                           }
                                           printf('removing double:%s',paste(inter,sep='',collapse = ',' ) )
                                         }
                                         printf('members now:%s',paste(cd$slotMembers[[x]] ,sep='',collapse = ',' ) )
                                         already = c(already, cd$slotMembers[[x]])
                                         
                                       }
                                     }
                                     
                                     
                                     for(x in 1:length(cd$slotMembers)){
                                       print(length(cd$slotMembers))
                                       printf('slotmembers in total:%s',cd$slotMembers[[x]])
                                       step_plate = Plate(paste('slot_', cd$name,as.character(x),sep=''))
                                       if(length(cd$slotMembers[[x]])==0){
                                         next;
                                       }
                                       
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
                                          res = suppressWarnings( as.numeric(cslotName) )
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
                                         lexDistPtr = modelPtr
                                         modelPtr = modelPtr$distrib
                                         
                                         if(class(modelPtr)!='Constant' ){
                                           print('add dists do the plate!!!!!!')
                                           step_plate$addNode( modelPtr) 
                                           ##also set parent here for double linked list
                                           
                                           ##check for storage
                                           if(isStorageNode(cd$distrib)){
                                             printf("%s is storage node",cd$distrib$getName())

                                          }else{
                                             printf('add parent %s - %s',cd$distrib$getName(),modelPtr$getName())
                                             modelPtr$addParentNode(cd$distrib)
                                          }
                                         }#else{
                                           #if(length(cd$slotMembers[[x]])==1 ){ #only 1 constant
                                          #   print('only one constant')
                                          #   printf('set compu slot for: %s , %d',cd$distrib$dist_name, x)
                                          #   cd$distrib$slots[[x]]=modelPtr
                                          #   cd$distrib$cslots[[x]]= ComputationNodeRef(modelPtr)
                                          # }
                                        # }
                                         
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
                                      # print(length(cd$distrib$cslots))
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
                               resolveIndex=function(index){
                                 currentValue =  getDataOrNumeric(index)
                                 if(is.null(currentValue)){
                                   ##its not numeric, is it data ?
                                   printf('could not reslove the variable:%s',index)
                                   #it might be an expression to resolve
                                   pc = ParseComputation(lex=.self) #this will break with different models
                                   res = pc$parse(index)
                                   currentValue = res$compute()
                                   if(is.null(currentValue) | is.na(currentValue)){
                                     printf('definitely could not reslove this variable:%s',index)
                                     return(NULL)
                                   }
                                 }
                                 return(currentValue)
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
                

Node <- setRefClass("Node",
                    methods = list(
                      compute=function(){print("Base class called")},
                      getSlotsList = function(txt){
                        allChars = strsplit(txt,'')[[1]]
                        slots = c()
                        start = 1
                        braketOpen = 0
                        
                        for(i in 1:length(allChars)){
                          if(allChars[i]==',' & braketOpen==0){ ##indicated slot 
                            slots = append(slots,substr(txt, start,i-1) )
                            start = i+1                
                          }else if(allChars[i]=='(' | allChars[i]=='['){
                            braketOpen = braketOpen+1
                          }else if(allChars[i]==')' | allChars[i]==']'){
                            braketOpen = braketOpen-1
                          }
                        }
                        slots = append(slots,substr(txt, start,length(allChars)) )
                        return(slots)
                      }
                    )
)

ComputationNode <- setRefClass("ComputationNode",
                               contains='Node',
                               fields = list(a='Node',b='Node' ),
                               methods = list(
                                 compute=function(){##do computation return result
                                   rleft =a$compute()
                                   rright= b$compute()
                                #   printf('a:%d,b:%d',rleft,rright)
                                   list('a'=rleft,'b'=rright)
                                 },
                                initialize = function(a=NULL,b=NULL) {
                                   if(is.null(a)){
                                     .self$a = Node()
                                   }else{
                                     .self$a=a #set
                                   }
                                  if(is.null(b)){
                                    .self$b=Node()
                                  }else{
                                    .self$b=b
                                  }
                                 }
                               )
)
ComputationNodeAdd <- setRefClass("ComputationNodeAdd",
                                  contains='ComputationNode',
                                  fields = list(),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      res = callSuper()
                                      #return(as.vector(res$a) + as.vector(res$b) )
                                     # printf('dims for add: %dx%d %dx%d',
                                      #       ncol(res$a),nrow(res$a),ncol(res$b),nrow(res$b) )
                                      if(ncol(res$a)==1 && nrow(res$a)==1){
                                        return(as.numeric(res$a) + res$b)
                                      }
                                      if(ncol(res$b)==1 && nrow(res$b)==1){
                                        return(res$a + as.numeric(res$b) )
                                      }
                                      
                                      return(res$a + res$b )
                                    }
                                  )
)

ComputationNodeSub <- setRefClass("ComputationNodeSub",
                                  contains='ComputationNode',
                                  fields = list(),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      res = callSuper()
                                #      printf('dims: %dx%d %dx%d',
                                 #            ncol(res$a),nrow(res$a),ncol(res$b),nrow(res$b) )
                                      if(ncol(res$a)==1 && nrow(res$a)==1){
                                        return(as.numeric(res$a) - res$b)
                                      }
                                      if(ncol(res$b)==1 && nrow(res$b)==1){
                                        return(res$a - as.numeric(res$b) )
                                      }
                                      
                                      return(res$a - res$b )
                                     # return(as.vector(res$a) - as.vector(res$b) )
                                    }
                                  )
)

ComputationNodeDiv <- setRefClass("ComputationNodeDiv",
                                  contains='ComputationNode',
                                  fields = list(),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      res = callSuper()
                                     # printf('dims: %dx%d %dx%d',
                                      #       ncol(res$a),nrow(res$a),ncol(res$b),nrow(res$b) )
                                      if(ncol(res$a)==1 && nrow(res$a)==1){
                                        return(as.numeric(res$a) / res$b)
                                      }
                                      if(ncol(res$b)==1 && nrow(res$b)==1){
                                        return(res$a / as.numeric(res$b) )
                                      }
                                      
                                      return(res$a / res$b )
                                      #return(as.vector(res$a) / as.vector(res$b) )
                                    }
                                  )
)
ComputationNodeMultiply <- setRefClass("ComputationNodeMultiply",
                                       contains='ComputationNode',
                                       fields = list(),
                                       methods = list(
                                         compute=function(){##do compuation return result
                                           res = callSuper()
                                          # if(is.numeric(res$b)){
                                          #   printf('numeric b:%f',res$b)
                                          # }
                                         #  printf('a:%s b:%s',as.character(class(res$a)) ,as.character( class(res$b)) )
                                        #   printf('dims: %dx%d %dx%d',
                                         #         ncol(res$a),nrow(res$a),ncol(res$b),nrow(res$b) )
                                           
                                          
                                           return(res$a %*% res$b)
                                         }
                                       )
)


ComputationNodeMultiplyElement <- setRefClass("ComputationNodeMultiplyElement",
                                       contains='ComputationNode',
                                       fields = list(),
                                       methods = list(
                                         compute=function(){##do compuation return result
                                           res = callSuper()
                                           if(nrow(res$a)!= ncol(res$b) ||
                                              ncol(res$a)!= nrow(res$b)){
                                             # printf('this will not be valid: %dx%d %dx%d',
                                             #       ncol(res$a),nrow(res$a),ncol(res$b),nrow(res$b) )
                                             # printf('%f',res$b)
                                             if( ncol(res$b)==1 && nrow(res$b)==1 ){
                                               return(res$a * as.numeric( res$b) )
                                             }
                                            # if( ncol(res$a)==1 && nrow(res$a)==1 ){
                                               return(as.numeric(res$a) *  res$b )
                                            # }
                                             
                                           }
                                           return(res$a * res$b)
                                         }
                                       )
)

#change this so matrix or so
ComputationNodeValue <- setRefClass("ComputationNodeValue",
                                    contains='Node',
                                    fields = list(a='matrix',name='character'),
                                    methods = list(
                                      compute=function(){##do compuation return result
                                        return(.self$a)
                                      },
                                      initialize = function(a,name='') {
                                        if(is.character(a)){
                                          a = as.numeric(a)
                                        }
                                        if(is.numeric(a)){
                                          a = as.matrix(a)
                                        }
                                        .self$a=a
                                        .self$name = as.character(name)
                                      }
                                    )
)
#require(pryr) #for address
ComputationNodeRef <- setRefClass("ComputationNodeRef",
                                    contains='Node',
                                    fields = list(a='Node'),
                                    methods = list(
                                      compute=function(){##do compuation return result
                                      # print(address(.self$a))
                                      #  printf('node:%s',.self$a$getName())
                                        return(.self$a$compute())
                                      },
                                      initialize = function(a) {
                                        .self$a=a
                                        #print(address(.self$a))
                                      }
                                    )
)

ComputationNodeIndex <- setRefClass("ComputationNodeIndex",
                                  contains='Node',
                                  fields = list(a='Node',slots='list'),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      # print(address(.self$a))
                                    #  val = matrix(.self$a$compute()[as.numeric(index)]) 
                                    #  printf('values: %s returning index:%s', paste(.self$a$compute(),sep='',collapse = ',') ,as.character(index))
                                      if(length(.self$slots)==0){
                                        printf('thre are no indexes')
                                      }
                                      idx = c()
                                      for(i in 1:length(slots)){
                                        idx = c(idx, slots[[i]]$compute())
                                      }
                                      val = matrix(.self$a$compute()[as.numeric(idx)]) 
                                      return(val)
                                    },
                                    initialize = function(a,idx,parser) {
                                      .self$a=a
                                      sl = getSlotsList(idx)
                                      for(i in 1:length(sl)){
                                        .self$slots[[i]]=parser$parse(sl[i])  
                                      }
                                     # .self$index = idx ##could be equations
                                      #print(address(.self$a))
                                    }
                                  )
)

ComputationNodeRefList <- setRefClass("ComputationNodeRefList",
                                  contains='Node',
                                  fields = list(nodes='list'),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      # print(address(.self$a))
                                      res = c()
                                      for(i in 1:length(nodes)){
                                        tmp = nodes[[i]]$compute()
                                        res = c(res, tmp )
                                      }
                                   #   printf('retunring result:%s',res)
                                      return(res)
                                    },
                                    initialize = function(a) {
                                      .self$nodes=a
                                      #print(address(.self$a))
                                    }
                                  )
)

#comp = ComputationNodeAdd(ComputationNodeMultiply(ComputationNodeValue(5),ComputationNodeValue(5)),ComputationNodeValue(5))
#comp$compute()

ParseComputation <- setRefClass("ParseComputation",
                                fields = list(lexer ='Lexer',modelIdx = 'numeric'),
                    methods = list(
                      initialize = function(lex = NULL,currentModel=1){
                        if(is.null(lex)){
                          .self$lexer = Lexer() ##empty only for test... 
                        }else{
                          .self$lexer = lex
                        }
                        .self$modelIdx = currentModel
                      },
                      isNumeric=function(char){
                        ret = suppressWarnings( as.numeric(char) )
                        return(!is.na(ret))
                      },
                      createNode=function(l1){
                        printf('create node:%s',l1)
                        
                        if(isNumeric(l1)){
                          printf('node is value:%s',l1)
                          return(ComputationNodeValue(l1,l1))
                        }
                        #is data ?
                        
                        ##check if first is -
                        doSub = F
                        if(substr(l1,1,1) == '-'){
                          ##ok
                          l1 = substr(l1,2,nchar(l1))
                          doSub = T
                        }
                        res = lexer$findData(l1)
                        if(!is.null(res)){
                          printf('is data node:%s',l1)
                          if(doSub){
                            node = ComputationNodeValue(-res,l1)
                          }else{
                            node = ComputationNodeValue(res,l1)
                          }
                          return(node)
                        }
                        ##check dist
                        if(length(lexer$model_list)>0){
                          printf('modellist length:%d',length(lexer$model_list))
                          res = lexer$model_list[[modelIdx]]$findDist(l1)
                          if(!is.null(res)){
                            printf('is distribution:%s',l1)
                            node =ComputationNodeRef(res$distrib)
                            if(doSub){
                              mn = ComputationNodeMultiplyElement()
                              mn$a = ComputationNodeValue(-1,'-1')
                              mn$b = node
                              node = mn
                              print('return times minus for you')
                            }
                            return( node)
                          }
                          
                          nodesList = list()
                          ###check 
                          for(i in 1:length(lexer$model_list[[modelIdx]]$dists)){
                              ddd = lexer$model_list[[modelIdx]]$dists[[i]]
                              ret = lexer$getIndex(ddd$getName())
                              if(!is.null(ret)){
                                 if(ret$name == l1){
                                   nodesList = append(nodesList, ddd$distrib)
                                 }
                              }
                              
                            }
                          if(length(nodesList)>0){
                            printf('found some nodes that we can use:%d',length(nodesList))
                            node = ComputationNodeRefList(nodesList)
                            if(doSub){
                              mn = ComputationNodeMultiplyElement()
                              mn$a = ComputationNodeValue(-1,'-1')
                              mn$b = node
                              node = mn
                            }
                            return(node)
                          }
                        }
                        
                        ###check if is function
                       if(exists(l1)){
                         if(is.function(eval(base::parse(text=l1))) ){
                           printf('%s is function',l1)
                          #  stop()
                            return( ComputationNodeFunction(l1,.self) )
                            
                         }
                       }
                        
                        if(length(lexer$model_list)>=modelIdx){
                          printf('Cannot resolve node:%s for model:%s',l1,lexer$model_list[[modelIdx]]$name)
                        }else{
                          printf('Cannot resolve node:%s',l1)
                        }
                        return(NULL)
                      },
                      createOpNode=function(op){
                        printf('create OP node:%s',op)
                        if(op=='+'){
                          return(ComputationNodeAdd())
                        }else if(op=='-'){
                          return(ComputationNodeSub())
                        }else if(op=='*'){
                          return(ComputationNodeMultiplyElement())
                        }else if(op=='/'){
                          return(ComputationNodeDiv())
                        }else if(op=='%*%'){
                          return(ComputationNodeMultiply())
                        }else{
                          printf('unkonwn op:%s',op)
                        }
                      },
                      parse=function(text){
                        printf('parse text %s',text)
                        allChars = strsplit(text, "")[[1]]
                        root = NULL
                        lastNode = NULL
                        
                        
                        pos = 1
                        op = ""
                        p1 = NULL
                       #p2 = ""
                        while( pos<=length(allChars)){
                          tok=  lexer$getNextToken(allChars,pos)
                          token = tok$str
                          pos = tok$pos
                          printf('token:%s',token)
                          if(nchar(token) ==0){
                            next;
                          }
                          
                          
                          if(token=='('){
                          #  printf("parse this first:%s",allChars)
                            cnt = 1
                            end = 0
                            for(c in pos:length(allChars)){
                              if(allChars[c]==')'){
                                cnt = cnt-1
                              }else if(allChars[c]=='('){
                                cnt = cnt +1
                              }
                              if(cnt==0){
                                printf('set end:%s',c)
                                end = c
                                break;
                                
                              }
                            }
                            if(cnt!=0){
                              printf('Cant find closing braket parsing:%s',text)
                            }
                            printf('parse this:%s',substr(text,pos,end-1))
                          #  printf('refer to all cahrs parse this:%s',paste(allChars[pos:(end-1)],sep='',collapse = '' ))
                            
                           # subParse =  paste(allChars[pos:(end-1) ],sep='',collapse = '' )
                            
                            if(class(p1)=='ComputationNodeFunction'){
                              p1$setText(substr(text,pos,end-1) )
                            }else{
                              p1 = parse(substr(text,pos,end-1))  
                            }
                            
                            pos = end+1
                            next;
                          }
                          
                          
                          if(is.null(p1)){
                            if(tok$type == 'DELIM'){
                              ##lefts hope is some minus
                              tok=  lexer$getNextToken(allChars,pos)
                              token = paste(token,tok$str,sep='',collapse = '')
                              pos = tok$pos
                              
                            }
                            p1=createNode(token)
                            if(is.null(p1)){
                              printf('node could not be create i better leave!')
                              return(NULL)
                            }
                            next;
                          }
                          
                          chars = strsplit( token,'')[[1]]
                          if(chars[1]=='[' ){
                            printf('its some index')
                            ###have to check last one
                            while( chars[length(chars)]!=']' ){
                              printf('please removes spaces..need to grep till end...')
                              tok=  lexer$getNextToken(allChars,pos)
                              token = tok$str
                              pos = tok$pos
                              printf('token:%s',token)
                              chars = append(chars, strsplit( token,'')[[1]])
                              
                            }
                            
                            if(!is.null(p1) ){
                              printf('we can apply it to p1')
                              if(class(p1)=='ComputationNodeFunction'){
                                printf('is funtion thats good')
                              }
                              p1= ComputationNodeIndex(p1,
                                                   paste(chars[2:(length(chars)-1)],sep='',collapse = '' )
                                                 ,.self)
                              next;
                            }else{
                              printf('p1 is empty we dont know where to apply index[] !')
                            }
                            
                          }
                          
                         # if(nchar(op)==0){
                          #check for dominance
                          if( any(op==c('*','/' , '%*%') ) & any(token==c('+','-') ) ){
                            ## number has to be in node and this node and the new node has to be root
                            lastNode$b= p1#createNode(p1)
                            newnode  = createOpNode(token)
                            newnode$a = root
                            root = newnode
                            lastNode = newnode
                            op = token
                            p1=NULL
                            next
                          }
                          
                            op = token
                            newnode  = createOpNode(op)
                            #p1Node = p1 #createNode(p1)
                            newnode$a = p1
                            if(is.null(root)){
                              root = newnode
                             # lastNode=newnode
                            }else{
                              lastNode$b=newnode
                            }
                            lastNode=newnode
                            p1=NULL
                        #    next
                        }
                        last = p1#createNode(p1);
                        if(is.null(root)){
                          root = last  
                        }else{
                            newnode$b= last
                        } 
                          
                            root
                        }
                       
                    )
)



ComputationNodeFunction <- setRefClass("ComputationNodeFunction",
                                       contains='Node',
                                       fields = list(fcnt='character',a='Node',slots='list',parser='ParseComputation'),
                                       methods = list(
                                         compute=function(){##do compuation return result
                                           # printf('call some function:%s with %d slots class:'
                                          #         ,fcnt , length(.self$slots))
                                          
                                           params = c()
                                           if(length(.self$slots)==0 ){
                                             printf('no computation to be done')
                                             return(NULL)
                                           }
                                           if(length(.self$slots)==1){
                                       #         printf("has only 1 slot:%s",  class(.self$slots[[1]]))
                                        #     if(class(.self$slots[[1]])=='ComputationNodeValue' ||
                                         #       class(.self$slots[[1]])=='ComputationHelperNode'||
                                          #      class(.self$slots[[1]])== 'ComputationNodeRef'){
                                               fcn = eval(base::parse(text=fcnt) )
                                        #       printf(class(.self$slots[[1]]))
                                            # printf('call:%s with class %s ncol:%d',fcnt ,class(.self$slots[[1]]$compute()),ncol(.self$slots[[1]]$compute()) )
                                               ret = fcn(.self$slots[[1]]$compute())
                                               if(!is.matrix(ret)){
                                                 ret = matrix(ret)
                                               }
                                               return( ret)
                                           #  }
                                           }
                                           
                                           
                                           for(i in 1:length(.self$slots)){
                                         #    printf('%d class:%s',i,class(.self$slots[[i]]))
                                       #      tmp = paste(.self$slots[[i]]$compute(),sep='',collapse = ',')
                                             #  printf(tmp)
                                        #     params =c(params,paste('c(',tmp,')',sep = '',collapse = '') )
                                             params = c(params, paste('.self$slots[[',i, ']]$compute()',sep='',collapse = '' ))  
                                           }
                                           
                                           params = paste(params,sep='',collapse = ',')
                                           fnCall = paste(fcnt,'(',params,')',sep='',collapse = '')
                                       #     printf('function Call:%s',fnCall)
                                           ret = eval(base::parse(text=fnCall) )
                                           if(!is.matrix(ret)){
                                             ret = matrix(ret)
                                           }
                                           return(ret )
                                         },
                                         setText= function(txt){
                                           printf('settext:%s',txt)
                                           #sl = strsplit(txt,',')[[1]]###
                                           ###needs a function that realiable extractrs the slots
                                           sl = getSlotsList(txt)
                                           for(i in 1:length(sl)){
                                             .self$slots[[i]]=parser$parse(sl[i])  
                                           }
                                         },
                                         initialize = function(name,parser) {
                                           .self$fcnt =name
                                           .self$parser=parser
                                           #print(address(.self$a))
                                         }
                                       )
)



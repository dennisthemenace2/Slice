Node <- setRefClass("Node",
                    methods = list(
                      compute=function(){print("Base class called")}
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
                                      return(as.vector(res$a) + as.vector(res$b) )
                                    }
                                  )
)

ComputationNodeSub <- setRefClass("ComputationNodeSub",
                                  contains='ComputationNode',
                                  fields = list(),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      res = callSuper()
                                      return(as.vector(res$a) - as.vector(res$b) )
                                    }
                                  )
)

ComputationNodeDiv <- setRefClass("ComputationNodeDiv",
                                  contains='ComputationNode',
                                  fields = list(),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      res = callSuper()
                                      return(as.vector(res$a) / as.vector(res$b) )
                                    }
                                  )
)
ComputationNodeMultiply <- setRefClass("ComputationNodeMultiply",
                                       contains='ComputationNode',
                                       fields = list(),
                                       methods = list(
                                         compute=function(){##do compuation return result
                                           res = callSuper()
                                         #  print(res)
                                           return(res$a %*% res$b)
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
                                  fields = list(a='Node',index='character'),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      # print(address(.self$a))
                                      return(matrix(.self$a$compute()[as.numeric(index)]) )
                                    },
                                    initialize = function(a,idx) {
                                      .self$a=a
                                      .self$index = idx ##could be equations
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

ComputationNodeFunction <- setRefClass("ComputationNodeFunction",
                                  contains='Node',
                                  fields = list(fcnt='character',a='Node',slots='list',parser='ParseComputation'),
                                  methods = list(
                                    compute=function(){##do compuation return result
                                      # print(address(.self$a))
                                 #     return(.self$a$compute())
                                      params = c()
                                      for(i in 1:length(.self$slots)){
                                     #   printf('%d class:%s',i,class(.self$slots[[i]]))
                                        tmp = paste(.self$slots[[i]]$compute(),sep='',collapse = ',')
                                      #  printf(tmp)
                                        params =c(params,paste('c(',tmp,')',sep = '',collapse = '') )
                                      }
                                      
                                      params = paste(params,sep='',collapse = ',')
                                      fnCall = paste(fcnt,'(',params,')',sep='',collapse = '')
                                     # printf('function Call:%s',fnCall)
                                      return(eval(base::parse(text=fnCall) ) )
                                    },
                                    setText= function(txt){
                                   #   printf('settext:%s',txt)
                                      sl = strsplit(txt,',')[[1]]
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
                        res = lexer$findData(l1)
                        if(!is.null(res)){
                          printf('is data node:%s',l1)
                          
                          return(ComputationNodeValue(res,l1))
                        }
                        ##check dist
                        if(length(lexer$model_list) >0){
                          res = lexer$model_list[[modelIdx]]$findDist(l1)
                          if(!is.null(res)){
                            printf('is distribution:%s',l1)
                            
                            return( ComputationNodeRef(res$distrib))
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
                            return(ComputationNodeRefList(nodesList))
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
                        
                        printf('Cannot resolve node:%s for model:%s',l1,lexer$model_list[[modelIdx]]$name)
                        
                      },
                      createOpNode=function(op){
                        printf('create OP node:%s',op)
                        if(op=='+'){
                          return(ComputationNodeAdd())
                        }else if(op=='-'){
                          return(ComputationNodeSub())
                        }else if(op=='*'){
                          return(ComputationNodeMultiply())
                        }else if(op=='/'){
                          return(ComputationNodeDiv())
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
                            #printf('parse this:%s',substr(text,pos,end-1))
                            if(class(p1)=='ComputationNodeFunction'){
                              p1$setText(substr(text,pos,end-1))
                            }else{
                              p1 = parse(substr(text,pos,end-1))  
                            }
                            
                            pos = end+1
                            next;
                          }
                          
                          
                          if(is.null(p1)){
                            p1=createNode(token)
                            next;
                          }
                          
                          chars = strsplit( token,'')[[1]]
                          if(chars[1]=='[' ){
                            printf('its some index')
                            if(!is.null(p1) ){
                              printf('we can apply it to p1')
                              if(class(p1)=='ComputationNodeFunction'){
                                printf('is funtion thats good')
                              }
                              p1= ComputationNodeIndex(p1,
                                                   paste(chars[2:(length(chars)-1)],sep='',collapse = '' )
                                                  )
                              next;
                            }
                            
                          }
                          
                         # if(nchar(op)==0){
                          #check for dominance
                          if( any(op==c('*','/') ) & any(token==c('+','-') ) ){
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

text = "3+1*5*2"
pc = ParseComputation()
res = pc$parse(text)
res$compute()

text = "4*1+3"
res = pc$parse(text)
res$compute()

text = "1*4+2+1*2*1+10"
res = pc$parse(text)
res$compute()

text = "c(1,2,3)[1]*2"
res = pc$parse(text)
res$compute()


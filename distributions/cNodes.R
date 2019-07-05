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
                                    fields = list(a='matrix'),
                                    methods = list(
                                      compute=function(){##do compuation return result
                                        return(.self$a)
                                      },
                                      initialize = function(a) {
                                        if(is.character(a)){
                                          a = as.numeric(a)
                                        }
                                        if(is.numeric(a)){
                                          a = as.matrix(a)
                                        }
                                        .self$a=a
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
                          return(ComputationNodeValue(l1))
                        }
                        #is data ?
                        res = lexer$findData(l1)
                        if(!is.null(res)){
                          printf('is data node:%s',l1)
                          
                          return(ComputationNodeValue(res))
                        }
                        ##check dist
                        res = lexer$model_list[[modelIdx]]$findDist(l1)
                        if(!is.null(res)){
                          printf('is distribution:%s',l1)
                          
                          return( ComputationNodeRef(res$distrib))
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
                            print("parse this first")
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
                            p1 = parse(substr(text,pos,end-1))
                            pos = end+1
                            next;
                          }
                          
                          if(is.null(p1)){
                            p1=createNode(token)
                            next;
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

text = "(1*4+3*(1*1))+2+1*2*1+10-1"
res = pc$parse(text)
res$compute()

text = "(1*4)/5"
res = pc$parse(text)
res$compute()

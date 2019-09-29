### basic node that is used for lots of things


printf <- function(...) invisible(print(sprintf(...)))


##
#' Base class for Node
#'
Node <- setRefClass("Node",
                    fields = list(),
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

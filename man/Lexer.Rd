\name{Lexer}
\alias{Lexer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slice Lexer class
}
\description{

This class contains the lexer and parser to create a model tree from the model string. 
To use this class, create an instance of it first. Then set the model string and data using the functions setModelString and setModelData.
Afterwards use lexModel to create all object required to build the model.
Than use parseModel to build the mode. This function will return the model tree that can be used in with sampler classed to sample from the model.
}
\usage{

lex = new('Lexer')
lex$setModelString(model_str)
lex$setModelData(data_list)

lex$lexModel()

root_plate = lex$parseModel()

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mode_str}{
	Contains the model string
  }
  \item{data_list}{
    Is a named list with the data used to build the model.
  }


}
\details{
The language is very similar to the bugs language
}
\value{
Will return the build model tree if successful.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
 Package Overview:
\code{\link[Slice:Slice-package]{Slice-Package}}

Base Sampler class: 
\code{\link[Slice:Sampler]{Sampler}}

}
\examples{

require('Slice')

#define a regression model 
my_model_str= "
model regModel{
  #this is a comment
  y~ dnorm(x*beta1 +beta0,sigma)
  sigma~dgamma(0.01,0.01)
  beta0 ~ dnorm(0,s)
  s ~ dunif(0,1)
  beta1 ~ dnorm(0,1) #one more comment
}
"

#generate some data

set.seed(1234)
n <- 100
beta   <- c(1.4,-0.8)
sigma2 <- 1
X = cbind(rnorm(n,2,1),1)
val = X %*% beta  + rnorm(n,0,sigma2) 
y = matrix( val,ncol = 1)
x = matrix(X[,1],ncol = 1)
data_list = list('y'=y,'x'=x)

lex = new('Lexer') ##create lexer obj
lex$setModelString(my_model_str) #set model
lex$setModelData(data_list) # set data

lex$lexModel() ##lexx the syntax 
 
root_plate = lex$parseModel() ##create the model

}



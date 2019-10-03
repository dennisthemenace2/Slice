\name{Sampler}
\alias{Sampler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sampler class
}
\description{
This is the sampler base class that defines the functions that have to be implemented by a sampler.
}
\usage{
 sampler = new("Sampler",model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	Constructor takes the model object create by the lexer
  }


}
\details{
 Expects the model in the constructor and defines the takeSample function that has to be overwritten by inheriting sampler class.
}
\value{
Returns a valid sampler object.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
 Package Overview:
\code{\link[Slice:Slice-package]{Slice-Package}}

Simple MCMC sampler class: 
\code{\link[Slice:MCMCsampler]{MCMCsampler}}

Slice sampler class: 
\code{\link[Slice:SliceSampler]{SliceSampler}}

Maximum likelihood based sampler class: 


}
\examples{
# you should inherit from this model. It does not provide functionality.
}



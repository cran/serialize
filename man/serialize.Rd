\name{serialize}
\title{Simple Serialization Interface}
\usage{
serialize(object, connection, ascii = FALSE, refhook = NULL)
unserialize(connection, refhook = NULL)
}
\alias{serialize}
\alias{unserialize}
\arguments{
  \item{object}{object to serialize.}
  \item{connection}{an open connection.}
  \item{ascii}{logical; specifies whether to use ascii or binary format}
  \item{refhook}{hook function for handling reference objects}
}
\description{
  A simple interface for serializing to connections.
}
\details{
  The function \code{serialize} writes \code{object} to the specified
  connection.  Sharing of reference objects is preserved within the
  object but not across separate calls to serialize.  If
  \code{connection} is \code{NULL} then \code{object} is serialized to a
  string, which is returned as the result of \code{serialize}.
  For a text mode connection, the default value of code{ascii} is set
  to \code{TRUE}

  \code{unserialize} reads an object from \code{connection}.
  \code{connection} may also be a string.

  The \code{refhook} functions can be used to customize handling of
  non-system reference objects (all external pointers and weak
  references, and all environments other than name space and package
  environments and \code{.GlobalEnv}).  The hook function for
  \code{serialize} should return a character vector for references it
  wants to handle; otherwise it should return NULL.  The hook for
  \code{unserialize} will be called with character vectors supplied to
  \code{serialize} and should return an appropriate object.
}
\examples{
x<-serialize(list(1,2,3),NULL)
unserialize(x)
}
\keyword{programming}
\author{Luke Tierney}

assert <- function(expr)
    if (! expr) stop(paste("assertion failed:", deparse(substitute(expr))))

library(serialize)

runTests <- function(useAscii = FALSE, dataFile = "tmp.RData") {
    testEnv <- environment()

    mysave <- function(list, file = dataFile, ascii=useAscii, version=2)
        save(list=list, file=file, ascii=ascii, version=version,
             envir=testEnv)

    myload <- function(file = dataFile)
        load(file, envir=testEnv)

    x<-list(1,2,3)
    f<-file(dataFile, open="wb")
    serialize(x,f, ascii = useAscii)
    close(f)
    f<-file(dataFile, open="rb")
    y <- unserialize(f)
    close(f)
    assert(identical(x,y))

    f<-file(dataFile, open="w")
    serialize(x,f)
    close(f)
    f<-file(dataFile, open="r")
    y <- unserialize(f)
    close(f)
    assert(identical(x,y))

    f<-textConnection("mytext","w")
    serialize(x,f)
    close(f)
    f<-textConnection("mytext","r")
    f<-textConnection(mytext,"r")
    y<-unserialize(f)
    close(f)
    assert(identical(x,y))

    z <- list(x=1,y="2")
    f<-file(dataFile,"wb")
    writeChar("RDX2\n", f, eos=NULL)
    serialize(z,f)
    close(f)
    load(dataFile)
    assert(identical(x, z$x) && identical(y, z$y))

    x<-list(1,2,3)
    mysave("x")
    f<-file(dataFile, "rb")
    readChar(f,5)
    y<-unserialize(f)
    close(f)
    assert(identical(x,y$x))

    x<-list(1,2,3)
    y<-unserialize(serialize(list(1,2,3),NULL, ascii = useAscii))
    assert(identical(x, y))

    y<-"bar"
    e<-new.env()
    e1<-e
    attr(e,"foo")<-y
    mysave("e")
    myload()
    assert(! identical(e,e1) && identical(y, attr(e,"foo")))

    e1 <- new.env()
    attr(e1,"name") <- "e1"
    e2 <- new.env()
    attr(e2,"name") <- "e2"
    x<-serialize(list(e1,e2,e1,e2),NULL, refhook=function(e) attr(e,"name"))
    y <- unserialize(x,refhook=function(x) x)
    assert(identical(y, list("e1", "e2", "e1", "e2")))
}

runTests(TRUE)
runTests(FALSE)

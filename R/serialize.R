serialize <- function(object, connection, ascii = FALSE, refhook = NULL) {
    if (! is.null(connection)) {
        if (missing(ascii))
            if (summary(connection)$text == "text")
                ascii <- TRUE
            else
                ascii <- FALSE
        if (!inherits(connection, "connection")) 
            stop("`connection' must be a connection")
    }
    if (! ascii && inherits(connection, "sockconn"))
        .Call("serializeb", object, connection, refhook, PACKAGE="serialize")
    else
        .Call("serialize", object, connection, ascii, refhook,
              PACKAGE="serialize")
}

unserialize <- function(connection, refhook = NULL) {
    if (! is.character(connection) && !inherits(connection, "connection")) 
        stop("`connection' must be a connection")
    .Call("unserialize", connection, refhook, PACKAGE="serialize")
}

.First.lib <- function(lib, pkg) {
    library.dynam( "serialize", pkg, lib )
}

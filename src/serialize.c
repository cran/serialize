#define NEED_CONNECTION_PSTREAMS
#include "Rinternals.h"
#include "R_ext/Rdynload.h"

/**** should eventually come from a public header file */
Rconnection getConnection(int n);
size_t R_WriteConnection(Rconnection con, void *buf, size_t n);

typedef struct membuf_st {
    int size;
    int count;
    unsigned char *buf;
} *membuf_t;

#define BCONBUFSIZ 4096

typedef struct bconbuf_st {
    Rconnection con;
    int count;
    unsigned char buf[BCONBUFSIZ];
} *bconbuf_t;

static void resize_buffer(membuf_t mb, int needed)
{
    int newsize = 2 * needed;
    mb->buf = realloc(mb->buf, newsize);
    if (mb->buf == NULL)
	error("cannot allocate buffer");
}

static void flush_buffer(bconbuf_t bb)
{
    if (R_WriteConnection(bb->con, bb->buf, bb->count) != bb->count)
	error("error writing to connection");
    bb->count = 0;
}

static void OutChar(R_outpstream_t stream, int c)
{
    membuf_t mb = stream->data;
    if (mb->count >= mb->size)
	resize_buffer(mb, mb->count + 1);
    mb->buf[mb->count++] = c;
}

static void OutCharBB(R_outpstream_t stream, int c)
{
    bconbuf_t bb = stream->data;
    if (bb->count >= BCONBUFSIZ)
	flush_buffer(bb);
    bb->buf[bb->count++] = c;
}

static void OutBytes(R_outpstream_t stream, void *buf, int length)
{
    membuf_t mb = stream->data;
    if (mb->count + length > mb->size)
	resize_buffer(mb, mb->count + length);
    memcpy(mb->buf + mb->count, buf, length);
    mb->count += length;
}

static void OutBytesBB(R_outpstream_t stream, void *buf, int length)
{
    bconbuf_t bb = stream->data;
    if (bb->count + length > BCONBUFSIZ)
	flush_buffer(bb);
    if (length <= BCONBUFSIZ) {
	memcpy(bb->buf + bb->count, buf, length);
	bb->count += length;
    }
    else if (R_WriteConnection(bb->con, buf, length) != length)
	error("error writing to connection");
}

static int InChar(R_inpstream_t stream)
{
    membuf_t mb = stream->data;
    if (mb->count >= mb->size)
	error("read error");
    return mb->buf[mb->count++];
}

static void InBytes(R_inpstream_t stream, void *buf, int length)
{
    membuf_t mb = stream->data;
    if (mb->count + length > mb->size)
	error("read error");
    memcpy(buf, mb->buf + mb->count, length);
    mb->count += length;
}

static void InitMemInPStream(R_inpstream_t stream, membuf_t mb,
			     void *buf, int length,
			     SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    mb->count = 0;
    mb->size = length;
    mb->buf = buf;
    R_InitInPStream(stream, (R_pstream_data_t) mb, R_pstream_any_format,
		    InChar, InBytes, phook, pdata);
}

static void InitMemOutPStream(R_outpstream_t stream, membuf_t mb,
			      R_pstream_format_t type, int version,
			      SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    mb->count = 0;
    mb->size = 0;
    mb->buf = NULL;
    R_InitOutPStream(stream, (R_pstream_data_t) mb, type, version,
		     OutChar, OutBytes, phook, pdata);
}

static SEXP CloseMemOutPStream(R_outpstream_t stream)
{
    SEXP val;
    membuf_t mb = stream->data;
    PROTECT(val = allocVector(CHARSXP, mb->count));
    memcpy(CHAR(val), mb->buf, mb->count);
    val = ScalarString(val);
    if (mb->buf != NULL) free(mb->buf);
    UNPROTECT(1);
    return val;
}

static void InitBConOutPStream(R_outpstream_t stream, bconbuf_t bb,
			       Rconnection con,
			       R_pstream_format_t type, int version,
			       SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    bb->count = 0;
    bb->con = con;
    R_InitOutPStream(stream, (R_pstream_data_t) bb, type, version,
		     OutCharBB, OutBytesBB, phook, pdata);
}

/* ought to quote the argument, but it should only be an ENVSXP or STRSXP */
static SEXP CallHook(SEXP x, SEXP fun)
{
    SEXP val, call;
    PROTECT(call = LCONS(fun, LCONS(x, R_NilValue)));
    val = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return val;
}

static SEXP serialize(SEXP object, SEXP icon, SEXP ascii, SEXP fun)
{
    struct R_outpstream_st out;
    R_pstream_format_t type;
    SEXP (*hook)(SEXP, SEXP);

    hook = fun != R_NilValue ? CallHook : NULL;

    if (asLogical(ascii)) type = R_pstream_ascii_format;
    else type = R_pstream_xdr_format; /**** binary or ascii if no XDR? */

    if (icon == R_NilValue) {
	struct membuf_st mbs;
	InitMemOutPStream(&out, &mbs, type, 0, hook, fun);
        /**** Need to make sure the buffer is released on error.  This
	      will be easier to do once this code is included in base.
	      But maybe having a oublic unwind-protect mechanism would
	      be useful too. */
	R_Serialize(object, &out);
	return CloseMemOutPStream(&out);
    }
    else {
	Rconnection con = getConnection(asInteger(icon));
	R_InitConnOutPStream(&out, con, type, 0, hook, fun);
	R_Serialize(object, &out);
	return R_NilValue;
    }
}

static SEXP serializeb(SEXP object, SEXP icon, SEXP fun)
{
    struct R_outpstream_st out;
    SEXP (*hook)(SEXP, SEXP);
    struct bconbuf_st bbs;
    Rconnection con = getConnection(asInteger(icon));

    hook = fun != R_NilValue ? CallHook : NULL;

    InitBConOutPStream(&out, &bbs, con, R_pstream_xdr_format, 0, hook, fun);
    R_Serialize(object, &out);
    flush_buffer(&bbs);
    return R_NilValue;
}

static SEXP unserialize(SEXP icon, SEXP fun)
{
    struct R_inpstream_st in;
    SEXP (*hook)(SEXP, SEXP);

    hook = fun != R_NilValue ? CallHook : NULL;

    if (TYPEOF(icon) == STRSXP && LENGTH(icon) > 0) {
	struct membuf_st mbs;
	void *data = CHAR(STRING_ELT(icon, 0));
	int length = LENGTH(STRING_ELT(icon, 0));
	InitMemInPStream(&in, &mbs, data,  length, hook, fun);
	return R_Unserialize(&in);
    }
    else {
	Rconnection con = getConnection(asInteger(icon));
	R_InitConnInPStream(&in, con, R_pstream_any_format, hook, fun);
	return R_Unserialize(&in);
    }
}

static R_CallMethodDef CallDefs[] = {
    {"serialize", (DL_FUNC) serialize, 4},
    {"serializeb", (DL_FUNC) serializeb, 3},
    {"unserialize", (DL_FUNC) unserialize, 2},
    {NULL}
};

void R_init_serialize(DllInfo *info)
{
    R_registerRoutines(info, NULL, CallDefs, NULL, 0);
}


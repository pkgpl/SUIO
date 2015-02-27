#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "segy_mod.h"
#include "su_xdr_mod.h"

#define TRACE_HEADER_BYTES 240
#define SIZE_OF_FLOAT 4

// STD IO open
void c_std_open_read(FILE **file_p)
{
	*file_p=stdin;
}
void c_std_open_write(FILE **file_p)
{
	*file_p=stdout;
}

// FILE IO
void c_file_open_read(const char *filename, FILE **file_p)
{
	*file_p=fopen(filename,"rb");
}
void c_file_open_write(const char *filename, FILE **file_p)
{
	*file_p=fopen(filename,"wb");
}
void c_file_close(FILE **file_p)
{
        fclose(*file_p);
}

// XDR initialize
void c_xdr_init_read(XDR *xdr_p,FILE **file_p)
{
	xdrstdio_create(xdr_p,*file_p,XDR_DECODE);
}
void c_xdr_init_write(XDR *xdr_p,FILE **file_p)
{
	xdrstdio_create(xdr_p,*file_p,XDR_ENCODE);
}

// XDR finalize
void c_xdr_finalize(XDR *xdr_p)
{
	xdr_destroy(xdr_p);
}

// trace IO
int c_read_trc(segy *trace,const int *Nsize,FILE **file_p)
{
	return fread(trace,*Nsize,1,*file_p);
}
int c_write_trc(segy *trace,const int *Nsize,FILE **file_p)
{
	return fwrite(trace,*Nsize,1,*file_p);
}
int c_xdr_read_trc(XDR *xdr_p,segy *trace)
{
	int val=xdrhdrsub(xdr_p,trace);
	if(val)
		val=xdr_vector(xdr_p,(char*)(trace->data),(unsigned int)(*trace).ns,SIZE_OF_FLOAT,(xdrproc_t) xdr_float);
	return val;
}
int c_xdr_write_trc(XDR *xdr_p,segy *trace)
{
	int val=xdrhdrsub(xdr_p,trace);
	if(val)
		val=xdr_vector(xdr_p,(char*)(trace->data),(unsigned int)(*trace).ns,SIZE_OF_FLOAT,(xdrproc_t) xdr_float);
	return val;
}

// header only
int c_read_trch(segy *trace,FILE **file_p)
{
	return fread(trace,TRACE_HEADER_BYTES,1,*file_p);
}
int c_xdr_read_trch(XDR *xdr_p,segy *trace)
{
	return xdrhdrsub(xdr_p,trace);
}

// UTIL move file pointer
void c_fseek_trc(const int *Itr,const int *Nsize,FILE **file_p)
{
	fseek(*file_p, *Nsize * (*Itr-1), SEEK_SET);
}
void c_xdr_fseek_trc(const int *Itr,const int *Nsize,XDR *xdr_p)
{
	xdr_setpos(xdr_p, *Nsize * (*Itr-1));
}

// UTIL get ns from the 1st trace header
short c_get_ns(FILE **file_p)
{
	segy trace;
	fseek(*file_p,(off_t) 0,SEEK_SET);
	fread(&trace,TRACE_HEADER_BYTES,1,*file_p);
	fseek(*file_p,(off_t) 0,SEEK_SET);
	return (short) trace.ns;
}
short c_xdr_get_ns(XDR *xdr_p)
{
	segy trace;
	xdr_setpos(xdr_p,(unsigned int) 0);
	xdrhdrsub(xdr_p,&trace);
	xdr_setpos(xdr_p,(unsigned int) 0);
	return (short) trace.ns;
}

// UTIL get file size to calculate ntr (file IO only)
off_t c_filesize(const char *filename)
{
	struct stat buf;
	if(stat(filename,&buf)==-1){
		perror("stat");
		return -1;
	}
	return (off_t) buf.st_size;
}


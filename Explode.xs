/*
 * Explode.xs
 * Last Modification: Mon May 24 17:12:39 WEST 2004
 *
 * Copyright (c) 2004 Henrique Dias <hdias@aesbuc.pt>. All rights reserved.
 * This module is free software; you can redistribute it and/or modify
 * it under the same terms as Perl itself.
 *
 */

#ifndef WIN32
#include <unistd.h>
#endif

#ifdef OP_PROTOTYPE
#undef OP_PROTOTYPE
#endif

#define PERL_POLLUTE

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#define BUFFLEN 256
#define TMPBUFFLEN 2*256
#define ISTEXT 70
#define MBXHDRLEN 39

#define JNK 0177
#define PAD 0100

#define DEC(Char) (((Char) - ' ') & 077)

#ifndef isGRAPH  
#define isGRAPH(c) (isgraph(c))
#endif

bool ismailbox(unsigned char *line) {
	int i = 5, p = 0;
	if(strlen(line) < MBXHDRLEN ||
		line[0] != 'F' || line[1] != 'r' || line[2] != 'o' ||
			line[3] != 'm' || line[4] != 0x20) return FALSE;
	while(line[i] == ' ') i++;
	p = i;
	while(line[i] != '@') {
		if(!isGRAPH(line[i])) return FALSE;
		i++;
	}
	if(i-p < 1) return FALSE;
	i += 1;
	p = i;
	while(line[i] != 0x20) {
		if(!(isALNUM(line[i]) || line[i] == '_' || line[i] == '.'  ||
			line[i] == '-')) return FALSE;
		i++;
	}
	if(i-p < 4) return FALSE;
	i += 1;
	while(line[i] == 0x20) i++;
	return((isALPHA(line[i]) && isALPHA(line[i+1]) && isALPHA(line[i+2]) &&
		line[i+3] == 0x20 && isALPHA(line[i+4]) && isALPHA(line[i+5]) &&
		isALPHA(line[i+6]) && line[i+7] == 0x20 &&
		(line[i+8] == 0x20 || isDIGIT(line[i+8])) && isDIGIT(line[i+9]) &&
		line[i+10] == 0x20 && isDIGIT(line[i+11]) && isDIGIT(line[i+12]) &&
		line[i+13] == ':' && isDIGIT(line[i+14]) && isDIGIT(line[i+15]) &&
		line[i+16] == ':' && isDIGIT(line[i+17]) && isDIGIT(line[i+18]) &&
		line[i+19] == 0x20 && isDIGIT(line[i+20]) && isDIGIT(line[i+21]) &&
		isDIGIT(line[i+22]) && isDIGIT(line[i+23]) && (line[i+24] == 0x0A ||
		(line[i+24] == 0x20 && (line[i+25] == '+' || line[i+25] == '-')&&
		isDIGIT(line[i+26]) && isDIGIT(line[i+27]) &&
		isDIGIT(line[i+28]) && isDIGIT(line[i+29]) &&
		line[i+30] == 0x0A))) ? TRUE : FALSE);
}

bool istext(unsigned char *buff, unsigned long l) {
	unsigned long i = 0, n = 0;
	for(i = 0; i < l; i++)
		if(isPRINT(buff[i])) n++;
	return(((int)(100*((float)n/(float)l)) > ISTEXT) ? TRUE : FALSE);
}

unsigned char *str_to_lower(unsigned char *string) {
	unsigned char *p = string;
	while(*p = toLOWER(*p)) *p++;
	return string;
}

static char *set_mime_type(unsigned char *buff, unsigned long len, char *base) {
	if(!len) return base;
	if(istext(buff, len)) {
		if(len > 4 && buff[0] == '%' &&
				!strnNE((char *)buff+1, "PDF-", 4))
			return("application/pdf");
		str_to_lower(buff);
		if(len > 5) {
			if(instr(buff, "<?xml ")) return("text/xml");
			if(instr(buff, "<html>")) return("text/html");
		}
		return((!strnNE(base, "text/", 5)) ? base : "text/plain");
	} else {
		if(len > 1 && buff[0] == 0x4d && buff[1] == 0x5a)
			return("application/octet-stream");
		if(len > 2 && buff[0] == 0x47 && buff[1] == 0x49 && buff[2] == 0x46)
			return("image/gif");
		if(len > 3) {
			if(buff[0] == 0x89 && buff[1] == 0x50 &&
					buff[2] == 0x4e && buff[3] == 0x47)
				return("image/png");
			if(buff[0] == 0x50 && buff[1] == 0x4b &&
					buff[2] == 0x03 && buff[3] == 0x04)
				return("application/x-zip-compressed");
		}
		if(len > 4 && buff[0] == '%' &&
				strnEQ((char *)buff+1, "PDF-", 4))
			return("application/pdf");
		if(len > 7 && buff[0] == 0xd0 && buff[1] == 0xcf &&
			buff[2] == 0x11 && buff[3] == 0xe0 &&
				buff[4] == 0xa1 && buff[5] == 0xb1 &&
					buff[6] == 0x1a && buff[7] == 0xe1)
			return("application/msword");
		if(len > 9 && buff[0] == 0xff && buff[1] == 0xd8 &&
			buff[2] == 0xff && buff[3] == 0xe0 &&
				strnEQ((char *)&buff[6], "JFIF", 4))
			return("image/jpeg");
		if(len > 15 && buff[0] == 0x42 && buff[1] == 0x4d &&
				buff[5] == 0x00 && buff[10] == 0x36 &&
					buff[15] == 0x28)
			return("image/bmp");
		return(base ? base : "");
	}
}

unsigned char *_rfc822_qprint(unsigned char *src,
			unsigned long srcl, unsigned long *len) {
	unsigned char *ret = NULL;
	unsigned char *d = NULL;
	unsigned char *t = NULL;
	unsigned char *s = src;
	unsigned char c, e;

	New(1, ret, (size_t) srcl + 1, unsigned char);
	d = ret;
	t = d;
	*len = 0;
	while(((unsigned long) (s - src)) < srcl) {
		switch(c = *s++) {
			case '=':
				if(((unsigned long) (s - src)) < srcl)
					switch (c = *s++) {
					case '\0':
						*d++ = '=';
						s--;
						break;
					case '\015':
						if((((unsigned long) (s - src)) < srcl) && (*s == '\012')) s++;
					case '\012':
						t = d;
						break;
					default:
						if(!(isxdigit(c) && (((unsigned long) (s - src)) < srcl) &&
								(e = *s++) && isxdigit(e))) {
						//	Safefree(ret);
						//	return NULL;
							*d++ = '=';
							s -= 2;
							if(*s == '=') s++;
							t = d;
							break;
						}
						if(isDIGIT(c)) c -= '0';
						else c -= (isUPPER(c) ? 'A' - 10 : 'a' - 10);
						if(isDIGIT(e)) e -= '0';
						else e -= (isUPPER(e) ? 'A' - 10 : 'a' - 10);
						*d++ = e + (c << 4);
						t = d;
						break;
				} else {
					*d++ = '=';
					t = d;
				}
				break;
			case ' ':
				*d++ = c;
				break;
			case '\015':
			case '\012':
				d = t;
			default:
				*d++ = c;
				t = d;
		}
	}
	*d = '\0';
	*len = d - ret;
	return ret;
}


void *_rfc822_base64(unsigned char *src, unsigned long srcl,
						unsigned long *len) {
	char c;
	char *d;
	int e;
	void *ret;
	static unsigned char decode[256] = {
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,076,JNK,JNK,JNK,077,
		064,065,066,067,070,071,072,073,074,075,JNK,JNK,JNK,PAD,JNK,JNK,
		JNK,000,001,002,003,004,005,006,007,010,011,012,013,014,015,016,
		017,020,021,022,023,024,025,026,027,030,031,JNK,JNK,JNK,JNK,JNK,
		JNK,032,033,034,035,036,037,040,041,042,043,044,045,046,047,050,
		051,052,053,054,055,056,057,060,061,062,063,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,
		JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK,JNK 
	};

	New(0, ret, (size_t) (*len = 4 + ((srcl * 3) / 4)), void);
	d = (char *)ret;

	memset(ret,0,(size_t)*len);
	*len = 0;
	for(e = 0; srcl--; )
		switch(c = decode[*src++]) {
		default:
			switch (e++) {
				case 0:
					*d = c << 2;
					break;
				case 1:
					*d++ |= c >> 4;
					*d = c << 4;
					break;
				case 2:
					*d++ |= c >> 2;
					*d = c << 6;
					break;
				case 3:
					*d++ |= c;
					e = 0;
					break;
			}
			break;
		case JNK:
			break;
		case PAD:
			switch (e++) {
				case 3:
					for(; srcl; --srcl)
						if(!(decode[*src++] & 0300)) {
							warn("Possible data truncation in _rfc822_base64(): %.80s", (char *)src-1);
						}
					break;
				case 2:
					if(srcl && (*src == '=')) break;
				default:
					Safefree(ret);
					return NULL;
			}
			break;
	}
	*len = d - (char *) ret;
	return ret;
}


void *uu_decode(char *buff, unsigned long srcl, unsigned long *len) {
	char *p;
	int n;
	void *ret = NULL;
	char *d = NULL;

	p = buff;
	n = DEC(*p);

	ret = New(2, ret, (size_t) (*len = n + ((3*srcl-6)/4) + 1), void);
	d = (char *)ret;
	memset(ret,0,(size_t)*len);
	*len = 0;
	if(n <= 0) return("");
	for(++p; n > 0; p += 4, n -= 3) {
		if(n >= 3) {
			*d++ = DEC(p[0]) << 2 | DEC (p[1]) >> 4;
			*d++ = DEC(p[1]) << 4 | DEC (p[2]) >> 2;
			*d++ = DEC(p[2]) << 6 | DEC (p[3]);
		} else {
			if(n >= 1)
				*d++ = DEC(p[0]) << 2 | DEC (p[1]) >> 4;
			if(n >= 2)
				*d++ = DEC(p[1]) << 4 | DEC (p[2]) >> 2;
		}
	}
	*d = '\0';
	*len = d - (char *)ret;
	return(ret);
}


void data_cat(char* tmp, char* data,
		unsigned long *tmplen, unsigned long len) {
	unsigned int i = 0;
	for(i = 0; i < len; i++) {
		if(*tmplen > TMPBUFFLEN-1) break;
		tmp[*tmplen] = data[i];
		(*tmplen)++;
	}
	tmp[*tmplen] = '\0';
}

MODULE = MIME::Explode	PACKAGE = MIME::Explode	PREFIX = exp_

PROTOTYPES: DISABLE

void
exp_rfc822_qprint(source)
		SV	*source
	PREINIT:
		STRLEN srcl;
		unsigned long len;
		unsigned char *s;
	PPCODE:
		s = (unsigned char*)SvPV(source, srcl);
		if(s = _rfc822_qprint(s, (unsigned long)srcl, &len))
			XPUSHs(sv_2mortal(newSVpv((char*)s, (STRLEN)len)));

void
exp_rfc822_base64(source)
		SV	*source
	PREINIT:
		STRLEN srcl;
		unsigned long len;
		unsigned char *s;
	PPCODE:
		s = (unsigned char*)SvPV(source, srcl);
		if(s = _rfc822_base64(s, (unsigned long)srcl, &len))
			XPUSHs(sv_2mortal(newSVpv((char*)s, (STRLEN)len)));		


void
exp_set_content_type(source, ...)
		SV	*source
	PREINIT:
		STRLEN srcl;
		unsigned char *s;
		char *base = NULL;
		char *mt;
	PPCODE:
		if(items == 2) base = SvPV(ST(1), na);
		s = (unsigned char*)SvPV(source, srcl);
		mt = set_mime_type(s, (unsigned long)srcl, base);
		XPUSHs(sv_2mortal(newSVpv(mt, (STRLEN)strlen(mt))));


void
exp_uu_file(fhs, filename, mode, ...)
		SV	*fhs;
		char	*filename;
		char	*mode;
	PREINIT:
		PerlIO *fpin = NULL;
		PerlIO *fptmp = NULL;
		PerlIO *fpout = NULL;
		I32 avlen = 0;
		AV *av_fhs = (AV*)SvRV(fhs);
		HV *hvtypes;
		SV *buff_sv = newSV(BUFFLEN);
		unsigned long len = 0;
		unsigned char *decoded = NULL;
		bool verify = TRUE;
		bool exclude = FALSE;
		char mimetype[BUFFLEN] = "";
		AV *av_ret = newAV();
		char tmp[TMPBUFFLEN];
		unsigned long tmplen = 0;
		I32 action = 1;
		I32 checktype = 0;
	PPCODE:
		if((avlen = av_len(av_fhs)) != -1) {
			fpin = IoIFP(sv_2io(*av_fetch(av_fhs, 0, 0)));
			if(avlen == 1)
				fptmp = IoIFP(sv_2io(*av_fetch(av_fhs, 1, 0)));
		} else
			croak("Null Array Reference");

		if(items == 5) {
			HV *hv = (HV*)SvRV(ST(4));
			if(hv_exists(hv, "action", 6)) {
				SV **value = hv_fetch(hv, "action", 6, 0);
				action = SvIVx(*value);
			}
			if(hv_exists(hv, "mimetypes", 9)) {
				SV **value = hv_fetch(hv, "mimetypes", 9, 0);
				hvtypes = (HV*)SvRV(*value);
			}
		}
		if((fpout = PerlIO_open(filename, "wb")) == NULL)
			croak("Failed to open file \"%s\"", filename);

		while(sv_gets(buff_sv, fpin, 0)) {
			STRLEN l = SvCUR(buff_sv);
			char *line = SvGROW(buff_sv, l);
			if(line[l-1] != 0x0a) break;
			if(fptmp != NULL) PerlIO_write(fptmp, line, l);
			if(instr(line, "end\n") || line[0] == 0x0a) break;
			if(!exclude) {
				decoded = uu_decode(line, l, &len);
				PerlIO_write(fpout, decoded, len);
			}
			if(verify) {
				if(line[0] == 0x20 || line[0] == 0x0a || line[0] == 0x0d) continue;
				data_cat(tmp, decoded, &tmplen, len);
				if(tmplen < TMPBUFFLEN) continue;
				strcpy(mimetype, set_mime_type(tmp, tmplen, mimetype));
				exclude = hv_exists(hvtypes, mimetype, strlen(mimetype)) ? (action ? FALSE : TRUE) :
					hv_iterinit(hvtypes) ? (action ? TRUE : FALSE) : (action ? FALSE : TRUE);
				verify = FALSE;
			}
		}
		PerlIO_close(fpout);
		if(verify) {
			strcpy(mimetype, set_mime_type(tmp, tmplen, mimetype));
			exclude = hv_exists(hvtypes, mimetype, strlen(mimetype)) ? (action ? FALSE : TRUE) :
				hv_iterinit(hvtypes) ? (action ? TRUE : FALSE) : (action ? FALSE : TRUE);
		}
		if(exclude)
			if(unlink(filename))
				croak("Failed to delete file \"%s\"", filename);

		av_push(av_ret, mimetype ? newSVpv(mimetype, 0) : newSVsv(&sv_undef));
		av_push(av_ret, newSViv(exclude ? 1 : 0));
		XPUSHs(sv_2mortal(newRV_noinc((SV*)av_ret)));


void
exp_decode_content(fhs, encoding="base64", filename, boundary="", ...)
		SV	*fhs;
		char 	*encoding;
		char	*filename;
		char 	*boundary;
	PREINIT:
		PerlIO *fpin = NULL;
		PerlIO *fptmp = NULL;
		PerlIO *fpout = NULL;
		unsigned char *decoded = NULL;
		unsigned char *rest = NULL;
		SV *buff_sv = newSV(BUFFLEN);
		SV *part = NULL;
		char mt[BUFFLEN] = "";
		bool last = FALSE;
		bool exclude = FALSE;
		bool verify = TRUE;
		I32 avlen = 0;
		HV *hvtypes;
		AV *av_ret = newAV();
		AV *av_fhs = (AV*)SvRV(fhs);
		unsigned long len = 0;
		char tmp[TMPBUFFLEN];
		unsigned long tmplen = 0;
		I32 action = 1;
		I32 checktype = 0;
		char *mimetype;
	PPCODE:
		if((avlen = av_len(av_fhs)) != -1) {
			fpin = IoIFP(sv_2io(*av_fetch(av_fhs, 0, 0)));
			if(avlen == 1)
				fptmp = IoIFP(sv_2io(*av_fetch(av_fhs, 1, 0)));
		} else
			croak("Null Array Reference");
		if(items == 5) {
			HV *hv = (HV*)SvRV(ST(4));
			if(hv_exists(hv, "mimetype", 8)) {
				SV **value = hv_fetch(hv, "mimetype", 8, 0);
				mimetype = SvPVx(*value, na);
			}
			if(hv_exists(hv, "checktype", 9)) {
				SV **value = hv_fetch(hv, "checktype", 9, 0);
				checktype = SvIVx(*value);
			}
			if(hv_exists(hv, "action", 6)) {
				SV **value = hv_fetch(hv, "action", 6, 0);
				action = SvIVx(*value);
			}
			if(hv_exists(hv, "mimetypes", 9)) {
				SV **value = hv_fetch(hv, "mimetypes", 9, 0);
				hvtypes = (HV*)SvRV(*value);
			}
		}
		if((fpout = PerlIO_open(filename, "wb")) == NULL)
			croak("Failed to open file \"%s\"", filename);
		while(sv_gets(buff_sv, fpin, 0)) {
			STRLEN l = SvCUR(buff_sv);
			char *line = SvGROW(buff_sv, l);
			if(fptmp != NULL) PerlIO_write(fptmp, line, l);
			if(encoding[0] == 'q' && ismailbox(line)) {
				part = newSVpvn(line, l);
				break;
			}
			if(encoding[0] == 'b') {
				char *pos = NULL;
				if(line[0] == 0x0a && len > 0) break;
				if(boundary[0] != '\0' && line[l-1] != 0x0a) break;
				if(pos = strchr(line, 0x20)) {
					pos++;
					while(*pos == 0x20) pos++;
					if(*pos != 0x0a) {
						part = newSVpvn(line, l);
						break;
					}
				}
			}
			if(boundary[0] != '\0' && (rest = instr(line, boundary))) {
				part = newSVpvn(rest, strlen(rest));
				l -= SvCUR(part);
				if(l == 0) break;
				line[l] = '\0';
				last = TRUE;
			}
			if(!exclude) {
				decoded = (encoding[0] == 'q') ?
					_rfc822_qprint(line, l, &len) : _rfc822_base64(line, l, &len);
				PerlIO_write(fpout, decoded, len);
			}
			if(last) break;
			if(verify) {
				if((encoding[0] == 'b' && line[0] == 0x20) || line[0] == 0x0a || line[0] == 0x0d) continue;
				data_cat(tmp, decoded, &tmplen, len);
				if(tmplen < TMPBUFFLEN) continue;
				strcpy(mt, (checktype) ? set_mime_type(tmp, tmplen, mimetype) : mimetype);
				exclude = hv_exists(hvtypes, mt, strlen(mt)) ? (action ? FALSE : TRUE) :
					hv_iterinit(hvtypes) ? (action ? TRUE : FALSE) : (action ? FALSE : TRUE);
				verify = FALSE;
			}
		}
		PerlIO_close(fpout);
		if(verify) {
			strcpy(mt, (checktype) ? set_mime_type(tmp, tmplen, mimetype) : mimetype);
			exclude = hv_exists(hvtypes, mt, strlen(mt)) ? (action ? FALSE : TRUE) :
				hv_iterinit(hvtypes) ? (action ? TRUE : FALSE) : (action ? FALSE : TRUE);
		}
		if(exclude)
			if(unlink(filename))
				croak("Failed to delete file \"%s\"", filename);
		av_push(av_ret, part ? part : newSVsv(&sv_undef));
		av_push(av_ret, mt ? newSVpv(mt, 0) : newSVsv(&sv_undef));
		av_push(av_ret, newSViv(exclude ? 1 : 0));
		XPUSHs(sv_2mortal(newRV_noinc((SV*)av_ret)));

/*
  Copyright (c) 2009 Dave Gamble

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

/* cJSON */
/* JSON parser in C. */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include "cJSON.h"

static const char *ep;

struct cJSON_Buf{
   char * buf;
   int len;
   int offset;
};

int cJSON_Buf_Init(cJSON_Buf & buf, int size,int padding){
	buf.len = size+padding;
	buf.offset= padding;
	buf.buf = (char*)malloc(buf.len);
	if(!buf.buf)return -1;
	return 0;
}

void cJSON_Buf_Clear(cJSON_Buf & buf){
	if(buf.buf)free(buf.buf);
}

int cJSON_Buf_Check(cJSON_Buf*buf, int len){
	int new_size;char * new_ptr;
	if(buf->len - buf->offset > len)return 0;
	
	new_size = buf->len * 2;
	while(new_size - buf->offset <= len){
		new_size*=2;
	}
	
    new_ptr = (char*)realloc(buf->buf, new_size);
    if(!new_ptr){
        return -1;
    }

    buf->buf = new_ptr;
    buf->len = new_size;
	return 0;
}

char* cJSON_Buf_Copy_Str(cJSON_Buf*buf,const char * str){
	int len =strlen(str);
	if(cJSON_Buf_Check(buf,len)<0)return 0;
	strcpy(buf->buf + buf->offset,str);
	buf->offset +=len;
	return buf->buf;
}

char* cJSON_Buf_Copy_Char(cJSON_Buf*buf,char ch){
	if(cJSON_Buf_Check(buf, 1)<0)return 0;
	*(buf->buf+buf->offset++)=ch;
	return buf->buf;
}

const char *cJSON_GetErrorPtr() {return ep;}

static int BKDRHash(const char *key){
     unsigned int seed =131;  //  31 131 1313 13131 131313 etc..
     unsigned int hash =0 ;
     while(*key){
         hash=hash*seed+(*key++);
     }
     return hash&0x7fffffff;
}

static int cJSON_strcasecmp(const char *s1,const char *s2)
{
	if (!s1) return (s1==s2)?0:1;if (!s2) return 1;
	for(; tolower(*s1) == tolower(*s2); ++s1, ++s2)	if(*s1 == 0)	return 0;
	return tolower(*(const unsigned char *)s1) - tolower(*(const unsigned char *)s2);
}

static void *(*cJSON_malloc)(size_t sz) = malloc;
static void (*cJSON_free)(void *ptr) = free;

static char* cJSON_strdup(const char* str)
{
      size_t len;
      char* copy;

      len = strlen(str) + 1;
      if (!(copy = (char*)cJSON_malloc(len))) return 0;
      memcpy(copy,str,len);
      return copy;
}

void cJSON_InitHooks(cJSON_Hooks* hooks)
{
    if (!hooks) { /* Reset hooks */
        cJSON_malloc = malloc;
        cJSON_free = free;
        return;
    }

	cJSON_malloc = (hooks->malloc_fn)?hooks->malloc_fn:malloc;
	cJSON_free	 = (hooks->free_fn)?hooks->free_fn:free;
}

/* Internal constructor. */
static cJSON *cJSON_New_Item()
{
	cJSON* node = (cJSON*)cJSON_malloc(sizeof(cJSON));
	if (node) memset(node,0,sizeof(cJSON));
	node->hash_string =-1;
	return node;
}

/* Delete a cJSON structure. */
void cJSON_Delete(cJSON *c)
{
	cJSON *next;
	while (c)
	{
		next=c->next;
		if (!(c->type&cJSON_IsReference) && c->child) cJSON_Delete(c->child);
		if (!(c->type&cJSON_IsReference) && c->valuestring) cJSON_free(c->valuestring);
		if (c->string) cJSON_free(c->string);
		cJSON_free(c);
		c=next;
	}
}

/* Parse the input text to generate a number, and populate the result into item. */
static const char *parse_number(cJSON *item,const char *num)
{
	double n=0,sign=1,scale=0;int subscale=0,signsubscale=1;

	/* Could use sscanf for this? */
	if (*num=='-') sign=-1,num++;	/* Has sign? */
	if (*num=='0') num++;			/* is zero */
	if (*num>='1' && *num<='9')	do	n=(n*10.0)+(*num++ -'0');	while (*num>='0' && *num<='9');	/* Number? */
	if (*num=='.') {num++;		do	n=(n*10.0)+(*num++ -'0'),scale--; while (*num>='0' && *num<='9');}	/* Fractional part? */
	if (*num=='e' || *num=='E')		/* Exponent? */
	{	num++;if (*num=='+') num++;	else if (*num=='-') signsubscale=-1,num++;		/* With sign? */
		while (*num>='0' && *num<='9') subscale=(subscale*10)+(*num++ - '0');	/* Number? */
	}

	n=sign*n*pow(10.0,(scale+subscale*signsubscale));	/* number = +/- number.fraction * 10^+/- exponent */
	
	item->valuedouble=n;
	item->valueint=(int)n;
	item->valueuint=(uint)n;
	item->type=cJSON_Number;
	return num;
}

/* Render the number nicely from the given item into a string. */
static char *print_number(cJSON *item,cJSON_Buf* buf)
{
	char str[64];
	double d=item->valuedouble;
	if (fabs(((double)item->valueint)-d)<=DBL_EPSILON && d<=INT_MAX && d>=INT_MIN)
	{
		sprintf(str,"%d",item->valueint);
	}
	else
	{
		if (fabs(floor(d)-d)<=DBL_EPSILON)			sprintf(str,"%.0f",d);
		else if (fabs(d)<1.0e-6 || fabs(d)>1.0e9)	sprintf(str,"%e",d);
		else										sprintf(str,"%f",d);
	}
	if(!cJSON_Buf_Copy_Str(buf,str))return 0;
	return buf->buf;
}

/* Parse the input text into an unescaped cstring, and populate item. */
static const unsigned char firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
static const char *parse_string(cJSON *item,const char *str)
{
	const char *ptr=str+1;char *ptr2;char *out;int len=0;unsigned uc;
	if (*str!='\"') {ep=str;return 0;}	/* not a string! */
	
	while (*ptr!='\"' && *ptr && ++len) if (*ptr++ == '\\') ptr++;	/* Skip escaped quotes. */
	
	out=(char*)cJSON_malloc(len+1);	/* This is how long we need for the string, roughly. */
	if (!out) return 0;
	
	ptr=str+1;ptr2=out;
	while (*ptr!='\"' && *ptr)
	{
		if (*ptr!='\\') *ptr2++=*ptr++;
		else
		{
			ptr++;
			switch (*ptr)
			{
				case 'b': *ptr2++='\b';	break;
				case 'f': *ptr2++='\f';	break;
				case 'n': *ptr2++='\n';	break;
				case 'r': *ptr2++='\r';	break;
				case 't': *ptr2++='\t';	break;
				case 'u':	 /* transcode utf16 to utf8. DOES NOT SUPPORT SURROGATE PAIRS CORRECTLY. */
					sscanf(ptr+1,"%4x",&uc);	/* get the unicode char. */
					len=3;if (uc<0x80) len=1;else if (uc<0x800) len=2;ptr2+=len;
					
					switch (len) {
						case 3: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
						case 2: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
						case 1: *--ptr2 =(uc | firstByteMark[len]);
					}
					ptr2+=len;ptr+=4;
					break;
				default:  *ptr2++=*ptr; break;
			}
			ptr++;
		}
	}
	*ptr2=0;
	if (*ptr=='\"') ptr++;
	item->valuestring=out;
	item->type=cJSON_String;
	return ptr;
}

/* Render the cstring provided to an escaped version that can be printed. */
static char *print_string_ptr(const char *str,cJSON_Buf* buf)
{
	const char *ptr=str;unsigned char token;char*out=0;
	char temp[6];
	if(!cJSON_Buf_Copy_Char(buf,'\"'))return 0;
	
	while (*ptr)
	{
		if ((unsigned char)*ptr>31 && *ptr!='\"' && *ptr!='\\') out=cJSON_Buf_Copy_Char(buf,*ptr++);
		else
		{
			if(!cJSON_Buf_Copy_Char(buf,'\\'))return 0;
			switch (token=*ptr++)
			{
				case '\\':	out=cJSON_Buf_Copy_Char(buf,'\\');break;
				case '\"':	out=cJSON_Buf_Copy_Char(buf,'\"');break;
				case '\b':	out=cJSON_Buf_Copy_Char(buf,'b');break;
				case '\f':	out=cJSON_Buf_Copy_Char(buf,'f');break;
				case '\n':	out=cJSON_Buf_Copy_Char(buf,'n');break;
				case '\r':	out=cJSON_Buf_Copy_Char(buf,'r');break;
				case '\t':	out=cJSON_Buf_Copy_Char(buf,'t');break;
				default: sprintf(temp,"u%04x",token);out=cJSON_Buf_Copy_Str(buf,temp);break;	/* escape and print */
			}
		}
		if(!out)return 0;
	}
	if(!cJSON_Buf_Copy_Char(buf,'\"'))return 0;
	return buf->buf;
}
/* Invote print_string_ptr (which is useful) on an item. */
static char *print_string(cJSON *item,cJSON_Buf* buf)	{return print_string_ptr(item->valuestring,buf);}

/* Predeclare these prototypes. */
static const char *parse_value(cJSON *item,const char *value);
static char *print_value(cJSON *item,int depth,int fmt,cJSON_Buf* buf);
static const char *parse_array(cJSON *item,const char *value);
static char *print_array(cJSON *item,int depth,int fmt,cJSON_Buf* buf);
static const char *parse_object(cJSON *item,const char *value);
static char *print_object(cJSON *item,int depth,int fmt,cJSON_Buf* buf);

/* Utility to jump whitespace and cr/lf */
static const char *skip(const char *in) {while (in && *in && (unsigned char)*in<=32) in++; return in;}

/* Parse an object - create a new root, and populate. */
cJSON *cJSON_Parse(const char *value)
{
	cJSON *c=cJSON_New_Item();
	ep=0;
	if (!c) return 0;       /* memory fail */

	if (!parse_value(c,skip(value))) {cJSON_Delete(c);return 0;}
	return c;
}

char * print_json(cJSON *item,int fmt,int padding,int estimate){
	cJSON_Buf buf;
	char *out;
	if(estimate<256)estimate=256;
	if(cJSON_Buf_Init(buf,estimate,padding)<0)return 0;
	out=print_value(item,0,fmt,&buf);
	if(!out){cJSON_Buf_Clear(buf);return 0;}
	out=cJSON_Buf_Copy_Char(&buf,0);
	if(!out){cJSON_Buf_Clear(buf);return 0;}
	return out;
}
/* Render a cJSON item/entity/structure to text. */
char *cJSON_Print(cJSON *item,int padding,int estimate)				{return print_json(item,1,padding,estimate);}
char *cJSON_PrintUnformatted(cJSON *item,int padding,int estimate)	{return print_json(item,0,padding,estimate);}

/* Parser core - when encountering text, process appropriately. */
static const char *parse_value(cJSON *item,const char *value)
{
	if (!value)						return 0;	/* Fail on null. */
	if (*value=='\"')				{ return parse_string(item,value); }
	if (*value=='{')				{ return parse_object(item,value); }
	if (*value=='-' || (*value>='0' && *value<='9'))	{ return parse_number(item,value); }
	if (*value=='[')				{ return parse_array(item,value); }
	if (!strncmp(value,"null",4))	{ item->type=cJSON_NULL;  return value+4; }
	if (!strncmp(value,"false",5))	{ item->type=cJSON_False; return value+5; }
	if (!strncmp(value,"true",4))	{ item->type=cJSON_True; item->valueint=1;	return value+4; }


	ep=value;return 0;	/* failure. */
}

/* Render a value to text. */
static char *print_value(cJSON *item,int depth,int fmt,cJSON_Buf* buf)
{
	char *out=0;
	if (!item) return 0;
	switch ((item->type)&255)
	{
		case cJSON_NULL:	out=cJSON_Buf_Copy_Str(buf,"null");	break;
		case cJSON_False:	out=cJSON_Buf_Copy_Str(buf,"false");break;
		case cJSON_True:	out=cJSON_Buf_Copy_Str(buf,"true"); break;
		case cJSON_Number:	out=print_number(item,buf);break;
		case cJSON_String:	out=print_string(item,buf);break;
		case cJSON_Array:	out=print_array(item,depth,fmt,buf);break;
		case cJSON_Object:	out=print_object(item,depth,fmt,buf);break;
	}
	return out;
}

/* Build an array from input text. */
static const char *parse_array(cJSON *item,const char *value)
{
	cJSON *child;
	if (*value!='[')	{ep=value;return 0;}	/* not an array! */

	item->type=cJSON_Array;
	value=skip(value+1);
	if (*value==']') return value+1;	/* empty array. */

	item->child=child=cJSON_New_Item();
	if (!item->child) return 0;		 /* memory fail */
	value=skip(parse_value(child,skip(value)));	/* skip any spacing, get the value. */
	if (!value) return 0;

	while (*value==',')
	{
		cJSON *new_item;
		if (!(new_item=cJSON_New_Item())) return 0; 	/* memory fail */
		child->next=new_item;new_item->prev=child;child=new_item;
		value=skip(parse_value(child,skip(value+1)));
		if (!value) return 0;	/* memory fail */
	}

	if (*value==']') return value+1;	/* end of array */
	ep=value;return 0;	/* malformed. */
}

/* Render an array to text */
static char *print_array(cJSON *item,int depth,int fmt,cJSON_Buf* buf)
{
	cJSON *child=item->child;
	if(!cJSON_Buf_Copy_Char(buf,'['))return 0;
	while (child)
	{
		if(!print_value(child,depth+1,fmt,buf))return 0;
		if(child->next){if(!cJSON_Buf_Copy_Char(buf,','))return 0;if(fmt&&!cJSON_Buf_Copy_Char(buf,' '))return 0;}
		child=child->next;
	}
	if(!cJSON_Buf_Copy_Char(buf,']'))return 0;
	return buf->buf;	
}

/* Build an object from the text. */
static const char *parse_object(cJSON *item,const char *value)
{
	cJSON *child;
	if (*value!='{')	{ep=value;return 0;}	/* not an object! */
	
	item->type=cJSON_Object;
	value=skip(value+1);
	if (*value=='}') return value+1;	/* empty array. */
	
	item->child=child=cJSON_New_Item();
	if (!item->child) return 0;
	value=skip(parse_string(child,skip(value)));
	if (!value) return 0;
	child->string=child->valuestring;child->valuestring=0;
	if (*value!=':') {ep=value;return 0;}	/* fail! */
	value=skip(parse_value(child,skip(value+1)));	/* skip any spacing, get the value. */
	if (!value) return 0;
	
	while (*value==',')
	{
		cJSON *new_item;
		if (!(new_item=cJSON_New_Item()))	return 0; /* memory fail */
		child->next=new_item;new_item->prev=child;child=new_item;
		value=skip(parse_string(child,skip(value+1)));
		if (!value) return 0;
		child->string=child->valuestring;child->valuestring=0;
		if (*value!=':') {ep=value;return 0;}	/* fail! */
		value=skip(parse_value(child,skip(value+1)));	/* skip any spacing, get the value. */
		if (!value) return 0;
	}
	
	if (*value=='}') return value+1;	/* end of array */
	ep=value;return 0;	/* malformed. */
}

/* Render an object to text. */
static char *print_object(cJSON *item,int depth,int fmt,cJSON_Buf* buf)
{
	int j=0;
	cJSON *child=item->child;
	depth++;
	if(!cJSON_Buf_Copy_Char(buf,'{'))return 0;
	if(fmt&&!cJSON_Buf_Copy_Char(buf,'\n'))return 0;
	while (child)
	{
		for (j=0;fmt&&j<depth;j++) if(!cJSON_Buf_Copy_Char(buf,'\t'))return 0;
		if(!print_string_ptr(child->string,buf))return 0;
		if(!cJSON_Buf_Copy_Char(buf,':'))return 0;
		if(fmt&&!cJSON_Buf_Copy_Char(buf,'\t'))return 0;
		if(!print_value(child,depth,fmt,buf))return 0;
		if (child->next && !cJSON_Buf_Copy_Char(buf,','))return 0;
		if(fmt&&!cJSON_Buf_Copy_Char(buf,'\n'))return 0;
		
		child=child->next;
	}
	if(!cJSON_Buf_Copy_Char(buf,'}'))return 0;
	return buf->buf;	
}

/* Get Array size/item / object item. */
int    cJSON_GetArraySize(cJSON *array)							{cJSON *c=array->child;int i=0;while(c)i++,c=c->next;return i;}
cJSON *cJSON_GetArrayItem(cJSON *array,int item)				{cJSON *c=array->child;  while (c && item>0) item--,c=c->next; return c;}
cJSON *cJSON_GetObjectItemV2(cJSON *object,const char *string,int*pos=NULL){
	int hash_code=BKDRHash(string),i=0;
	cJSON *c=object->child;
	while (c){
		if(c->hash_string==-1){c->hash_string=BKDRHash(c->string);}
		if(c->hash_string==hash_code && !cJSON_strcasecmp(c->string,string)){if(pos)*pos=i;return c;}
		c=c->next;
		i++;
	}
	return c;
}
cJSON *cJSON_GetObjectItem(cJSON *object,const char *string){return cJSON_GetObjectItemV2(object,string);}

/* Utility for array list handling. */
static void suffix_object(cJSON *prev,cJSON *item) {prev->next=item;item->prev=prev;}
/* Utility for handling references. */
static cJSON *create_reference(cJSON *item) {cJSON *ref=cJSON_New_Item();if (!ref) return 0;memcpy(ref,item,sizeof(cJSON));ref->string=0;ref->type|=cJSON_IsReference;ref->next=ref->prev=0;return ref;}

/* Add item to array/object. */
void   cJSON_AddItemToArray(cJSON *array, cJSON *item)						{cJSON *c=array->child;if (!item) return; if (!c) {array->child=item;} else {while (c && c->next) c=c->next; suffix_object(c,item);}}
void   cJSON_AddItemToObject(cJSON *object,const char *string,cJSON *item)	{if (!item) return; if (item->string) cJSON_free(item->string);item->string=cJSON_strdup(string);cJSON_AddItemToArray(object,item);}
void	cJSON_AddItemReferenceToArray(cJSON *array, cJSON *item)						{cJSON_AddItemToArray(array,create_reference(item));}
void	cJSON_AddItemReferenceToObject(cJSON *object,const char *string,cJSON *item)	{cJSON_AddItemToObject(object,string,create_reference(item));}

cJSON *cJSON_DetachItemFromArray(cJSON *array,int which)			{cJSON *c=array->child;while (c && which>0) c=c->next,which--; if (c) return cJSON_DetachItemFromParent(array,c);return 0;}
void   cJSON_DeleteItemFromArray(cJSON *array,int which)			{cJSON_Delete(cJSON_DetachItemFromArray(array,which));}
cJSON *cJSON_DetachItemFromObject(cJSON *object,const char *string) {cJSON *c=cJSON_GetObjectItemV2(object,string);if (c) return cJSON_DetachItemFromParent(object,c);return 0;}
void   cJSON_DeleteItemFromObject(cJSON *object,const char *string) {cJSON_Delete(cJSON_DetachItemFromObject(object,string));}
cJSON *cJSON_DetachItemFromParent(cJSON *object,cJSON *c)           {if (c->prev) c->prev->next=c->next;if (c->next) c->next->prev=c->prev;if (c==object->child) object->child=c->next;c->prev=c->next=0;return c;}
void   cJSON_DeleteItemFromParent(cJSON *object,cJSON *c)			{cJSON_Delete(cJSON_DetachItemFromParent(object,c));}

/* Replace array/object items with new ones. */
void   cJSON_ReplaceItemInArray(cJSON *array,int which,cJSON *newitem)		{cJSON *c=array->child;while (c && which>0) c=c->next,which--;if (!c) return;
	newitem->next=c->next;newitem->prev=c->prev;if (newitem->next) newitem->next->prev=newitem;
	if (c==array->child) array->child=newitem; else newitem->prev->next=newitem;c->next=c->prev=0;cJSON_Delete(c);}
void   cJSON_ReplaceItemInObject(cJSON *object,const char *string,cJSON *newitem){int i=0;cJSON *c=cJSON_GetObjectItemV2(object,string,&i);if(c){if(newitem->string) cJSON_free(newitem->string);newitem->string=cJSON_strdup(string);cJSON_ReplaceItemInArray(object,i,newitem);}}

/* Create basic types: */
cJSON *cJSON_CreateNull()						{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_NULL;return item;}
cJSON *cJSON_CreateTrue()						{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_True;return item;}
cJSON *cJSON_CreateFalse()						{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_False;return item;}
cJSON *cJSON_CreateBool(int b)					{cJSON *item=cJSON_New_Item();if(item)item->type=b?cJSON_True:cJSON_False;return item;}
cJSON *cJSON_CreateNumber(double num)			{cJSON *item=cJSON_New_Item();if(item){item->type=cJSON_Number;item->valuedouble=num;item->valueint=(int)num;item->valueuint=(uint)num;}return item;}
cJSON *cJSON_CreateString(const char *string)	{cJSON *item=cJSON_New_Item();if(item){item->type=cJSON_String;item->valuestring=cJSON_strdup(string);}return item;}
cJSON *cJSON_CreateArray()						{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_Array;return item;}
cJSON *cJSON_CreateObject()						{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_Object;return item;}

/* Create Arrays: */
cJSON *cJSON_CreateIntArray(int *numbers,int count)				{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateNumber(numbers[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}
cJSON *cJSON_CreateFloatArray(float *numbers,int count)			{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateNumber(numbers[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}
cJSON *cJSON_CreateDoubleArray(double *numbers,int count)		{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateNumber(numbers[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}
cJSON *cJSON_CreateStringArray(const char **strings,int count)	{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateString(strings[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}

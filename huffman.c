/**********************************************************************
 * MPEG/audio 编码/解码软件              *
 * VERSION 2.10                                                       *
 *********************************************************************/	
#include "common.h"
#include "huffman.h"
#include "user.h"
#include "types.h"

HUFFBITS dmask = (HUFFBITS)1 << (sizeof(HUFFBITS)*8-1);

struct huffcodetab ht[HTN];	/* 所有 huffcodtable headers的数组	*/
				/* 0..31 Huffman code table 0..31	*/
				/* 32,33 count1-tables			*/

/* 读取 huffman 解码表 */
int read_decoder_table(int fi) 
{
  int n,i,nn,t;
  unsigned int v0,v1;
  char command[100],line[100];
  for (n=0;n<HTN;n++) {
    
    do {
      read(fi, line, 99);
    } while ((line[0] == '#') || (line[0] < ' '));
     
    sscanf(line,"%s %s %u %u %u %u",command,ht[n].tablename,
           &ht[n].treelen, &ht[n].xlen, &ht[n].ylen, &ht[n].linbits);




    if (strcmp(command,".end")==0)
      return n;
    else if (strcmp(command,".table")!=0) {
      // printf(stderr,"huffman table %u data corrupted\n",n);
      return -1;
    }
    ht[n].linmax = (1<<ht[n].linbits)-1;
   
    sscanf(ht[n].tablename,"%u",&nn);
    if (nn != n) {
      // printf(stderr,"wrong table number %u\n",n);
      return(-2);
    } 
    do {
      read(fi,line, 99);
    } while ((line[0] == '#') || (line[0] < ' '));

    sscanf(line,"%s %u",command,&t);
    if (strcmp(command,".reference")==0) {
      ht[n].ref   = t;
      ht[n].val   = ht[t].val;
      ht[n].treelen  = ht[t].treelen;
      if ( (ht[n].xlen != ht[t].xlen) ||
           (ht[n].ylen != ht[t].ylen)  ) {
        // printf(stderr,"wrong table %u reference\n",n);
        return (-3);
      };
      while ((line[0] == '#') || (line[0] < ' ') ) {
        read(fi, line,99);
      }
    }    
    else if (strcmp(command,".treedata")==0) {
      ht[n].ref  = -1;
      ht[n].val = (unsigned char (*)[2]) 
         // calloc(2*(ht[n].treelen),sizeof(unsigned char));
      	memset(ht[n].val, 0, (2*(ht[n].treelen)*sizeof(uchar)));
      if ((ht[n].val == 0) && ( ht[n].treelen != 0 )){
    	// printf(stderr, "heaperror at table %d\n",n);
    	exit();
      }
      for (i=0;i<ht[n].treelen; i++) {
      	char buf1[6];
      	read(fi, buf1, 6);
      	sscanf(buf1, "%x %x",&v0, &v1);
        // fscanf(fi,"%x %x",&v0, &v1);
        ht[n].val[i][0]=(unsigned char)v0;
        ht[n].val[i][1]=(unsigned char)v1;
      }
      read(fi,line,99); /* 读取剩余行 */
    }
    else {
      // printf(stderr,"huffman decodertable error at table %d\n",n);
    }
  }
  return n;
}


/* 进行huffman解码	*/
/* 注意! 对counta,countb - 4 bit 值 用 y返回, discard x */
int huffman_decoder(struct huffcodetab *h, int *x, int *y, int *v, int *w)
{  
  HUFFBITS level;
  int point = 0;
  int error = 1;
  level     = dmask;
  if (h->val == 0) return 2;

  /* table 0 不需要 bits */
  if ( h->treelen == 0)
  {  *x = *y = 0;
     return 0;
  }


  /* 查找 Huffman table. */

  do {
    if (h->val[point][0]==0) {   /*树的结尾*/
      *x = h->val[point][1] >> 4;
      *y = h->val[point][1] & 0xf;

      error = 0;
      break;
    } 
    if (hget1bit()) {
      while (h->val[point][1] >= MXOFF) point += h->val[point][1]; 
      point += h->val[point][1];
    }
    else {
      while (h->val[point][0] >= MXOFF) point += h->val[point][0]; 
      point += h->val[point][0];
    }
    level >>= 1;
  } while (level  || (point < ht->treelen) );
  
  /* 检查错误 */
  
  if (error) { /* 设置 x 和 y 为一中间值 */
    printf(1, "Illegal Huffman code in data.\n");
    *x = (h->xlen-1 << 1);
    *y = (h->ylen-1 << 1);
  }

  /* 处理信号编码 */

  if (h->tablename[0] == '3'
      && (h->tablename[1] == '2' || h->tablename[1] == '3')) {
     *v = (*y>>3) & 1;
     *w = (*y>>2) & 1;
     *x = (*y>>1) & 1;
     *y = *y & 1;

     /* v, w, x 和 y 在比特流中是颠倒的，交换他们 
         */
     
/*   {int i=*v; *v=*y; *y=i; i=*w; *w=*x; *x=i;}  MI */

     if (*v)
        if (hget1bit() == 1) *v = -*v;
     if (*w)
        if (hget1bit() == 1) *w = -*w;
     if (*x)
        if (hget1bit() == 1) *x = -*x;
     if (*y)
        if (hget1bit() == 1) *y = -*y;
     }
     
  /* 对dual table情况，处理信号编码 */
  
  else {
  
      /* 在测试比特流中x 和 y是颠倒的 
         这里颠倒 x 和 y 使测试比特流正常 */
    
     if (h->linbits)
       if ((h->xlen-1) == *x) 
         *x += hgetbits(h->linbits);
     if (*x)
        if (hget1bit() == 1) *x = -*x;
     if (h->linbits)	  
       if ((h->ylen-1) == *y)
         *y += hgetbits(h->linbits);
     if (*y)
        if (hget1bit() == 1) *y = -*y;
     }
	  
  return error;  
}

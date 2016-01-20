#include "../dynasm/dasm_proto.h"
#include "../dynasm/dasm_x86.h"

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <assert.h>
#include <sys/mman.h>
#include <inttypes.h>

#ifdef DUMP_ASSEMBLY
/* For dumping the assembly code out */
#include <udis86.h>
#endif /* DUMP_ASSEMBLY */


/* This calculator is more fancy than the one we build in calc2.c. It actually allows
 * you to handle floating point number calculation. Also we allow user to call a func
 * that is in a shared object which is much more flexible than before */

/* Before we start doing anything, we need to make sure that we have enough knowledge.
 *
 * 1. Number is always represented as a double instead of integer.
 * The rational behind this is that representing number directly using double precision
 * will simply makes our life easier. Because passing argument for integer/double-word
 * is different with passing arguments for floating point. They use different sets of
 * registers. Which requires type analysing (semantic checking) and it is out of the
 * scope of this code. For simplicity all the number are only supported through number.
 *
 * This also requires the function that you call from the script must have a prototype
 * for double of each number. Otherwise behavior is undefined, mostly it is a crash.
 *
 * 2. The calling convention on X64 AMD is that caller is able to freely use register
 * from xmm0 - xmm7 which will give us 8 free registers, and also it allows us to pass
 * up to 8 floating point number through these 8 registers. This increase our callable
 * argument number from 6 to 8 */

/* Just X64. Only for fun :) */
|.arch x64
|.actionlist actions
/* Setup globals since we will use local label */
|.globals CALC3
void* CALC3_GLOBALS[CALC3_MAX];

|.macro pusharg1, val
  | movsd xmm0, val
|.endmacro

|.macro pusharg2, val
  | movsd xmm1, val
|.endmacro

|.macro pusharg3, val
  | movsd xmm2, val
|.endmacro

|.macro pusharg4, val
  | movsd xmm3, val
|.endmacro

|.macro pusharg5, val
  | movsd xmm4, val
|.endmacro

|.macro pusharg6, val
  | movsd xmm5, val
|.endmacro

|.macro pusharg7, val
  | movsd xmm6, val
|.endmacro

|.macro pusharg8, val
  | movsd xmm7, val
|.endmacro

|.macro debug, tp , val
  | pushxmm xmm0
  | pushxmm xmm1
  | movsd xmm0,val
  | mov edi, tp
  | callq &Debug
  | popxmm xmm1
  | popxmm xmm0
|.endmacro

/* Since no instruction support push xmm registers onto stack, we need to roll
 * our own wheels. It basically just modify the RSP pointer and use move to move
 * the value directly to it. On thing to note, the stack is growing down, which
 * means from higher address to lower address. Allocating memory from stack is
 * really substract the pointer of stack register. Eg:
 * sub rsp,8 ; this allocates 8 bytes on top of the stack
 * movsb [rsp] xmm0  ; this move the double precision number from xmm0 register to
 *                   ; the allocated space. Because the CPU treats memory grows from
 *                   ; lower to higher so the current RSP is really the base address
 *                   ; for our temporary spaces
 */
|.macro pushxmm, reg
  | sub rsp, 8
  | movsd qword [rsp], reg
|.endmacro

|.macro popxmm, reg
  | movsd reg, qword [rsp]
  | add rsp, 8
|.endmacro

/* since we will not use EAX register anymore, so
 * we don't need to push it onto the stack here */
|.macro callq, addr
  | mov r8, addr
  | call r8
|.endmacro

/* Per tutorial , Dst must be pointed to the dasm_State* .
 * We could make it global stats or use this macro to point
 * to it as well */
#define Dst (&(comp->dstate))

/* Tokenizer */
enum {
  TK_ADD,
  TK_SUB,
  TK_MUL,
  TK_DIV,
  TK_NUMBER,
  TK_VARIABLE,
  TK_LT,
  TK_LE,
  TK_GT,
  TK_GE,
  TK_EQ,
  TK_NE,
  TK_NOT,
  TK_LPAR,
  TK_RPAR,
  TK_AND,
  TK_OR,
  TK_QUESTION,
  TK_COLON,
  TK_COMMA,
  TK_EOF
};

struct tokenizer {
  const char* src;
  int pos;
  int tk;
  int len;
  union {
    double num;
    char symbol[32];
  } val;
};

int tk_next( struct tokenizer* tk ) {
re_lex:
  switch(tk->src[tk->pos]) {
    case 0: return (tk->tk = TK_EOF); /* end of file */
    case ' ':case '\r':case '\n':
    case '\b':case '\t': ++tk->pos; goto re_lex;
    case '+': tk->len = 1; return (tk->tk = TK_ADD);
    case '-': tk->len = 1; return (tk->tk = TK_SUB);
    case '*': tk->len = 1; return (tk->tk = TK_MUL);
    case '/': tk->len = 1; return (tk->tk = TK_DIV);
    case '>':
      if(tk->src[tk->pos+1] == '=') {
        tk->len = 2;
        return (tk->tk = TK_GE);
      } else {
        tk->len = 1;
        return (tk->tk = TK_GT);
      }
    case '<':
      if(tk->src[tk->pos+1] == '=') {
        tk->len = 2;
        return (tk->tk = TK_LE);
      } else {
        tk->len = 1;
        return (tk->tk = TK_LT);
      }
    case '=':
      if(tk->src[tk->pos+1] == '=') {
        tk->len = 2;
        return (tk->tk = TK_EQ);
      } else {
        fprintf(stderr,"unknown token:\"=\".Do you mean \"==\"");
        return -1;
      }
    case '!':
      if(tk->src[tk->pos+1] == '=') {
        tk->len = 2;
        return (tk->tk = TK_NE);
      } else {
        tk->len = 1;
        return (tk->tk = TK_NOT);
      }
    case ',':
      tk->len = 1;
      return (tk->tk = TK_COMMA);
    case '(':
      tk->len = 1;
      return (tk->tk = TK_LPAR);
    case ')':
      tk->len = 1;
      return (tk->tk = TK_RPAR);
    case '&':
      if(tk->src[tk->pos+1] == '&') {
        tk->len = 2;
        return (tk->tk = TK_AND);
      } else {
        fprintf(stderr,"unknown token:\"&\".Do you mean \"&&\"");
        return -1;
      }
    case '|':
      if(tk->src[tk->pos+1] == '|') {
        tk->len = 2;
        return (tk->tk = TK_OR);
      } else {
        fprintf(stderr,"unknown token:\"|\".Do you mean \"||\"");
        return -1;
      }
    case '?':
      tk->len = 1;
      return (tk->tk = TK_QUESTION);
    case ':':
      tk->len = 1;
      return (tk->tk = TK_COLON);
    case '0':case '1':case '2':case '3':case '4':
    case '5':case '6':case '7':case '8':case '9':
      { /* parsing the number into val */
        char* end;
        errno = 0;
        tk->val.num = strtod(tk->src+tk->pos,&end);
        if(errno) {
          fprintf(stderr,"cannot parse number:%s!\n",
              strerror(errno));
          return -1;
        }
        tk->len = end-(tk->src+tk->pos);
        return (tk->tk = TK_NUMBER);
      }
    default:
      /* variable name */
      if( isalpha(tk->src[tk->pos]) || tk->src[tk->pos] == '_' ) {
        /* lexing variable name */
        int j = tk->pos+1;
        for( ; isalnum(tk->src[j]) || tk->src[j] == '_' ; ++j )
          ;
        if( j - tk->pos >= 32 ) {
          fprintf(stderr,"too long variable name,more than 32!\n");
          return -1;
        } else {
          memcpy(tk->val.symbol,tk->src+tk->pos,j-tk->pos);
          tk->val.symbol[j-tk->pos] = 0;
        }
        tk->len = j - tk->pos;
        return (tk->tk = TK_VARIABLE);
      } else {
        fprintf(stderr,"unexpected token character:%c\n",
            tk->src[tk->pos]);
        return -1;
      }
  }
}

int tk_init( struct tokenizer* tk , const char* src ) {
  tk->src = src;
  tk->pos = 0;
  return tk_next(tk);
}

void tk_move( struct tokenizer* tk ) {
  tk->pos += tk->len;
  tk_next(tk);
}

/* Compiler */

struct compiler {
  struct tokenizer tk;
  dasm_State* dstate;
  int tag;
  int tag_cap;
};

void check_compiler_tag( struct compiler* comp ) {
  if(comp->tag == comp->tag_cap) {
    if(comp->tag_cap == 0)
      comp->tag_cap = 4;
    else
      comp->tag_cap*=2;
    dasm_growpc(Dst,comp->tag_cap);
  }
}

/* Variable lookup */
static
const char* STRTABLE[100];
static
size_t STRTABLE_POS = 0;

static const char* add_string( const char* name ) {
  size_t i;
  for( i = 0 ; i < STRTABLE_POS ; ++i ) {
    if( strcmp(STRTABLE[i],name) == 0 )
      return STRTABLE[i];
  }
  if(STRTABLE_POS==100)
    return NULL;
  else {
    return STRTABLE[STRTABLE_POS++] = strdup(name); /* safe */
  }
}

/* Now our variable lookup will result int double precision number
 * instead of integer. So we gonna return double precision number */
static double lookup( const char* name ) {
  if(strcmp(name,"defined_var")==0)
    return 100;
  else
    return 0;
}

struct function {
  void* where;
  const char* name;
};

static
struct function FUNCTABLE[100];
static
size_t FUNCTABLE_POS = 0;

static void* func_lookup( const char* name ) {
  size_t i;
  for( i = 0 ; i < FUNCTABLE_POS; ++i ) {
    if(strcmp(FUNCTABLE[i].name,name)==0)
      return FUNCTABLE[i].where;
  }
  return NULL;
}

static void add_func( const char* name , void* entry ) {
  assert( FUNCTABLE_POS < 100 );
  FUNCTABLE[FUNCTABLE_POS].name = strdup(name);
  FUNCTABLE[FUNCTABLE_POS].where= entry;
  ++FUNCTABLE_POS;
}

static
double NUMTABLE[100];

static
size_t NUMTABLE_POS =0;

static const double NZERO = -0.0;
static const double DTRUE = 1.0;
static const double DFALSE= 0.0;

static void* add_number( double num ) {
  size_t i;
  for( i = 0 ; i < NUMTABLE_POS ; ++i ) {
    if( num == NUMTABLE[i] )
      return NUMTABLE + i;
  }
  if( NUMTABLE_POS == 100 )
    return NULL;
  else {
    NUMTABLE[NUMTABLE_POS++]=num;
    return NUMTABLE + NUMTABLE_POS -1;
  }
}

static void Debug( int type , double val ) {
  printf("TYPE:%d,DEBUG:%f\n",type,val);
}

/* We gonna use XMM0 which is the default return register for
 * function to return double precision number. Also we gonna
 * use XMM1 in case we want compile operand. */

/* To clarify the protocol for using register here, any register
 * indicated by REG for each compilation sub-routine, it is always
 * free to be used by that routine. No need to maintain the status
 * of it. But any registeres not mentioned in REG , the compilation
 * sub-routine _MUST_ maintain its status */
enum {
  REG_XMM0,
  REG_XMM1
};

int expr( struct compiler* comp , int REG );

/* function call */
int func_call( struct compiler* comp , int REG , const char* fn ) {
  void* addr; /* function address */
  int cnt = 0; /* function argument count */
  assert(comp->tk.tk == TK_LPAR);

  addr = func_lookup(fn);
  if(!addr) { fprintf(stderr,"no such function:%s!",fn); return -1; }
  tk_move(&(comp->tk));
  while(comp->tk.tk != TK_RPAR) {
    /* Always force the expression generate code in XMM0 registers */
    if(expr(comp,REG_XMM0))
      return -1;

    /* push the value into corresponding register for call */
    switch(cnt) {
      case 0:
        /* Because we store our temporary valu einto REG_XMM0, we cannot
         * use this register until we finish every function arguments parsing,
         * so we push it onto stack */
        | pushxmm xmm0
        break;
      case 1:
        | pusharg2 xmm0
        break;
      case 2:
        | pusharg3 xmm0
        break;
      case 3:
        | pusharg4 xmm0
        break;
      case 4:
        | pusharg5 xmm0
        break;
      case 5:
        | pusharg6 xmm0
        break;
        /* We allow up to 8 arguments to be passed */
      case 6:
        | pusharg7 xmm0
        break;
      case 7:
        | pusharg8 xmm0
        break;
      default:
        assert(0);
    }

    /* check comma or not */
    if(comp->tk.tk != TK_COMMA)
      break;
    else
      tk_move(&(comp->tk));

    ++cnt;
    if(cnt == 8) {
      fprintf(stderr,"only 6 arguments is allowed to call a function!");
      return -1;
    }
  }

  /* Now we need to patch the first argument since it is in the stack now */
  if(cnt >= 1) {
    /* If no argument is passed in, then we do nothing */
    | popxmm xmm0
  }

  /* Expect a ) */
  if(comp->tk.tk != TK_RPAR) {
    fprintf(stderr,"The function is not properly closed!");
    return -1;
  }
  /* Emit call crap */
  | callq addr

  /* Now move result accordingly */
  if( REG == REG_XMM1 ) {
    | movsd xmm1, xmm0
  }

  tk_move(&(comp->tk));
  return 0;
}

/* number value is always returned in EAX register */
int atomic( struct compiler* comp , int REG ) {
  /* lex next token from the input */
  switch(comp->tk.tk) {
    case TK_LPAR:
      tk_move(&(comp->tk));
      if(expr(comp,REG))
        return -1;
      if(comp->tk.tk != TK_RPAR) {
        fprintf(stderr,"The sub-expression is not properly closed!");
        return -1;
      }
      tk_move(&(comp->tk));
      return 0;
    case TK_VARIABLE: {
      const char* var = add_string(comp->tk.val.symbol);
      if(!var) {
        fprintf(stderr,"too much symbol name!");
        return -1;
      }
      tk_move(&(comp->tk));
      if( comp->tk.tk == TK_LPAR) {
        /* it is a function call */
        return func_call(comp,REG,var);
      } else {
        /* generate call stub. as we say, that if the REG
         * is not REG_XMM0 we cannot use it safely, so we
         * need to generate workaround here */
        if( REG ==  REG_XMM1 ) {
          | pushxmm xmm0
        }

        | mov rdi, var
        | callq &lookup

        /* the value returned from the lookup table is
         * actually in the XMM0 register */
        if( REG == REG_XMM1 ) {
          | movsd xmm1, xmm0
          | popxmm xmm0
        }

      }
      break;
    }
    case TK_NUMBER: {
      /* Push number literal is a little bit complicated
       * since machine doesn't support 64 bits immediate
       * number which forces us to push the value from
       * memory. A typical compiler will generate the corresponding
       * 64 bit number in a specific section and load it
       * with specific memory. We don't do this since we
       * are on the runtime. We will allocate number table
       * and find the address and load it. The number table
       * will not be cleared until we finish our runing */
      double num = comp->tk.val.num;
      void* ptr = add_number(num);
      if( REG == REG_XMM0 ) {
        | movsd xmm0, qword [ptr]
      } else {
        | movsd xmm1, qword [ptr]
      }
      tk_move(&(comp->tk));
      break;
    }
    default:
      return -1;
  }
  return 0;
}

/* Represent a registers use cases */
struct reg {
  int on : 1; /* If this register is used or not */
  int pos: 31;/* Position of this register relative to other */
};

/* unary */
int unary( struct compiler* comp , int REG ) {
  /* generate code for unary is kind of tricky,
   * since the generation order is not the order
   * of how code is written. Here we need to save
   * all the unary operator until we get the operand
   * , then we can do the generation */
  int op[32]; /* You will have 32 operators, are you kidding me ? */
  int sz = 0;
  int i;
  struct reg xmm, ecx;
  int pos = 0;
  xmm.on = 0;
  ecx.on = 0;

  do {
    if(comp->tk.tk == TK_NOT ||
       comp->tk.tk == TK_SUB ||
       comp->tk.tk == TK_ADD ) {
      if(sz == 32 ) {
        fprintf(stderr,"Are you kidding?You have "
            "32 consecutive unary operators!");
        return -1;
      }
      op[sz++] = comp->tk.tk;
    } else {
      break;
    }
    tk_move(&(comp->tk));
  } while(1);

  if(atomic(comp,REG)) {
    return -1;
  }

  /* now we know EAX must contain our baby */
  for( i = 0 ; i < sz ; ++i ) {
    switch(op[i]) {
      case TK_SUB:
        /* Negate a signed double precision number is not very easy. We don't
         * have a instruction doing this sort of things. The actual thing that
         * I want to do is just fliping the MSB of this 64 bits value. To do
         * this we load a negative zero which has MSB set to 1, rest to be zero.
         * And XOR it with the value we want to negate */
        if( REG == REG_XMM0 ) {
          if(!xmm.on) {
            xmm.on = 1;
            xmm.pos = pos++;
            | pushxmm xmm1
          }
          | movsd xmm1, qword [&NZERO]
          | xorpd xmm0, xmm1
        } else {
          if(!xmm.on) {
            xmm.on = 1;
            xmm.pos = pos++;
            | pushxmm xmm0
          }
          | movsd xmm0, qword [&NZERO]
          | xorpd xmm1, xmm0
        }
        break;
      case TK_ADD:
        /* positive sign is crap */
        break;
      case TK_NOT:
        /* This one is kind of direct. Just compare our value with zero , however
         * this is will ignore the negative zero. Anyway, we don't care right now.
         * If it is zero, just load true to xmm0 , otherwise false to xmm0. I don't
         * quit understand how GCC generate code for this thing, actually. But this
         * one is good for illustration here */
        if( REG == REG_XMM0 ) {
          if(!xmm.on) {
            xmm.on = 1;
            xmm.pos = pos++;
            | pushxmm xmm1
          }
          if(!ecx.on) {
            ecx.on = 1;
            ecx.pos = pos++;
            | push rcx
          }
          | xorpd xmm1,xmm1
          | xor ecx, ecx
          | comisd xmm0,xmm1
          | sete cl
          | cvtsi2sd xmm0, ecx
        } else {
          if(!xmm.on) {
            xmm.on = 1;
            xmm.pos= pos++;
            | pushxmm xmm0
          }
          if(!ecx.on) {
            ecx.on = 1;
            ecx.pos= pos++;
            | push rcx
          }
          | xorpd xmm0,xmm0
          | xor ecx,ecx
          | comisd xmm1,xmm0
          | sete cl
          | cvtsi2sd xmm1,ecx
        }
        break;
      default:
        assert(0);
    }

    /* Pop the register based on the relative order */
    if(xmm.on) {
      if(xmm.pos ==0) {
        if( REG == REG_XMM0 ) {
          | popxmm xmm1
        } else {
          | popxmm xmm0
        }
      } else {
        assert(ecx.on);
        assert(ecx.pos ==0);
        | pop rcx
        if( REG == REG_XMM0 ) {
          | popxmm xmm1
        } else {
          | popxmm xmm0
        }
      }
    }
  }
  return 0;
}

/* factor */
int factor( struct compiler* comp , int REG ) {
  if(unary(comp,REG)) {
    return -1;
  }
  else {
    int mreg = 0;
    /* now the value is already in EAX */
    do {
      int op;
      if(comp->tk.tk != TK_MUL &&
         comp->tk.tk != TK_DIV ) {
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));
      if(!mreg) {
        mreg = 1;
        if( REG == REG_XMM0 ) {
          | pushxmm xmm1
        } else {
          | pushxmm xmm0
        }
      }
      if(unary(comp,!REG))
        return -1;

      if(op == TK_MUL) {
        if( REG == REG_XMM0 ) {
          | mulsd xmm0, xmm1
        } else {
          | mulsd xmm1, xmm0
        }
      } else {
        if( REG == REG_XMM1 ) {
          | divsd xmm1, xmm0
        } else {
          | divsd xmm0, xmm1
        }
      }
    } while(1);
    if(mreg) {
      if( REG == REG_XMM0 ) {
        | popxmm xmm1
      } else {
        | popxmm xmm0
      }
    }
  }
  return 0;
}

/* term */
int term( struct compiler* comp , int REG ) {
  if(factor(comp,REG))
    return -1;
  else {
    int mreg = 0;
    do {
      int op;
      if(comp->tk.tk != TK_ADD &&
         comp->tk.tk != TK_SUB ) {
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));

      if(!mreg) {
        mreg = 1;
        if( REG == REG_XMM0 ) {
          | pushxmm xmm1
        } else {
          | pushxmm xmm0
        }
      }
      if(factor(comp,!REG))
        return -1;

      if(op == TK_ADD) {
        if( REG == REG_XMM0 ) {
          | addsd xmm0, xmm1
        } else {
          | addsd xmm1, xmm0
        }
      } else {
        if( REG == REG_XMM0 ) {
          | subsd xmm0, xmm1
        } else {
          | subsd xmm1, xmm0
        }
      }
    } while(1);
    if(mreg) {
      if( REG == REG_XMM0 ) {
        | popxmm xmm1
      } else {
        | popxmm xmm0
      }
    }
  }
  return 0;
}

/* Comparison */
int comparison( struct compiler* comp , int REG ) {
  /* generate comparison is simple just need to use
   * CMP/TEST instructions. Since we don't bother for
   * having optimization, mostly we just use CMP */
  if(term(comp,REG))
    return -1;
  else {
    int op;
    int mreg = 0; /* indicate whether we use a new register
                   * or not */
    do {
      if(comp->tk.tk != TK_LT &&
          comp->tk.tk != TK_LE &&
          comp->tk.tk != TK_GT &&
          comp->tk.tk != TK_GE &&
          comp->tk.tk != TK_NE &&
          comp->tk.tk != TK_EQ ) {
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));

      if( !mreg ) {
        mreg = 1;
        if( REG == REG_XMM0 ) {
          | pushxmm xmm1
        } else {
          | pushxmm xmm0
        }
      }

      /* Use !REG to get its conterpart register */
      if(term(comp,!REG))
        return -1;

      | push rcx
      switch(op) {
        case TK_LT:
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | setb cl
          break;
        case TK_LE:
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | setbe cl
          break;
        case TK_GT:
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | seta cl
          break;
        case TK_GE:
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | setae cl
          break;
        case TK_EQ:
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | sete cl
          break;
        case TK_NE:
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | setne cl
          break;
        default:
          assert(0);
      }
      | movzx ecx, cl
      if( REG == REG_XMM0 ) {
        | cvtsi2sd xmm0, ecx
      } else {
        | cvtsi2sd xmm1, ecx
      }
      | pop rcx
    } while(1);
    if(mreg) {
      if( REG == REG_XMM0 ) {
        | popxmm xmm1
      } else {
        | popxmm xmm0
      }
    }
  }
  return 0;
}

int logic( struct compiler* comp , int REG ) {
  if(comparison(comp,REG))
    return -1;
  else {
    int op;
    int tag = -1;

    do {
      if(comp->tk.tk != TK_AND &&
         comp->tk.tk != TK_OR ) {
        if(tag >= 0) {
          /* Right now, the last value of check_compiler_tag
           * is not in the eax but in xmm1/xmm0. We need to
           * do the comparison and set the eax accordingly */
          if( REG == REG_XMM0 ) {
            | comisd xmm0, xmm1
          } else {
            | comisd xmm1, xmm0
          }
          | setne al
          | movzx eax, al
        }
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));

      if(tag<0) {
        check_compiler_tag(comp);
        tag = comp->tag;
        ++comp->tag;
        if( REG == REG_XMM0 ) {
          | pushxmm xmm1
          | xorpd xmm1, xmm1
        } else {
          | pushxmm xmm0
          | xorpd xmm0, xmm0
        }
        | push rax
        | xor eax, eax
      }

      if( REG == REG_XMM0 ) {
        | comisd xmm0, xmm1
      } else {
        | comisd xmm1, xmm0
      }

      if(op == TK_AND) {
        | je =>tag
      } else {
        | setne al
        | jne =>tag
      }

      if(comparison(comp,REG))
        return -1;
    } while(1);

    /* normalize value */
    if( tag >= 0 ) {
      |=>tag:
      | cmp eax, 0
      | setne al
      | movzx eax, al

      if(REG == REG_XMM1) {
        | cvtsi2sd xmm1, eax
      } else {
        | cvtsi2sd xmm0, eax
      }
      | pop rax
      if( REG == REG_XMM0 ) {
        | popxmm xmm1
      } else {
        | popxmm xmm0
      }
    }
  }
  return 0;
}

int expr( struct compiler* comp , int REG ) {
  if(logic(comp,REG))
    return -1;

  if(comp->tk.tk == TK_QUESTION) {
    int tag;
    int exit;
    tk_move(&(comp->tk));

    check_compiler_tag(comp);
    tag = comp->tag;
    check_compiler_tag(comp);
    exit = comp->tag+1;

    comp->tag += 2;

    if( REG == REG_XMM1 ) {
      | xorpd xmm0, xmm0
      | comisd xmm1, xmm0
      | je => tag
    } else {
      | xorpd xmm1, xmm1
      | comisd xmm0, xmm1
      | je => tag
    }

    /* first value */
    if(logic(comp,REG))
      return -1;

    | jne =>exit

    if(comp->tk.tk == TK_COLON)
      tk_move(&(comp->tk));
    else {
      fprintf(stderr,"Tenary needs \":\"!");
      return -1;
    }

    /* second value */
    |=> tag:
    if(logic(comp,REG))
      return -1;

    /* exit location for this tenary */
    |=> exit:
  }

  return 0;
}

/* COPYWRITE for haberman.
 * github.com/haberman/jitdemo/dynasm-driver.c */
void *jitcode(dasm_State **state) {
  size_t size;
  int dasm_status = dasm_link(state, &size);
  assert(dasm_status == DASM_S_OK);

  // Allocate memory readable and writable so we can
  // write the encoded instructions there.
  char *mem = mmap(NULL, size + sizeof(size_t),
		   PROT_READ | PROT_WRITE,
                   MAP_ANON | MAP_PRIVATE, -1, 0);
  assert(mem != MAP_FAILED);

  // Store length at the beginning of the region, so we
  // can free it without additional context.
  *(size_t*)mem = size;
  void *ret = mem + sizeof(size_t);

  dasm_encode(state, ret);
  dasm_free(state);

  // Adjust the memory permissions so it is executable
  // but no longer writable.
  int success = mprotect(mem, size, PROT_EXEC | PROT_READ);
  assert(success == 0);

  return ret;
}

void* compile( const char* src ) {
  struct compiler comp;
  dasm_State* state = comp.dstate;
  comp.tag = comp.tag_cap = 0;

  /* initialize dynasm dstate */
  dasm_init(&state,1);
  dasm_setup(&state,actions);
  dasm_setupglobal(&state,CALC3_GLOBALS,CALC3_MAX);

  comp.dstate = state;

  /* This is a hack. Macro can be pushed and poped
   * but I just don't want this compiler directives */
#undef Dst
#define Dst (&(comp.dstate))

  /* initialize lexer */
  if(tk_init(&(comp.tk),src) <0)
    return NULL;

  /* generate a stack frame pointer but I don't think it
   * is useful on X86-64  */
  | push rbx
  | mov rbx,rsp
  if(expr(&comp,REG_XMM0))
    return NULL;
  | pop rbx
  | ret

  state = comp.dstate; /* For safety reason, since it is a pointer 2
                        * pointer, not sure it is modified or not */
  /* get the jited code */
  return jitcode(&state);
}

void free_jitcode(void *code) {
  void *mem = (char*)code - sizeof(size_t);
  int status = munmap(mem, *(size_t*)mem);
  assert(status == 0);
}

typedef double (*func)();

/* Add a simple function and make our code fancier */
static double my_abs( double val ) {
  return val > 0 ? val : -val;
}

static double my_mul(double a, double b) {
  return a*b;
}

static double my_div(double a,double b) {
  return a/b;
}

#ifdef DUMP_ASSEMBLY
static void dump_assemb( void* ptr ) {
    ud_t ud;
    size_t* size = (size_t*)((char*)ptr - sizeof(size_t));

    ud_init(&ud);
    ud_set_input_buffer(&ud,ptr,*size);
    ud_set_mode(&ud,64);
    ud_set_syntax(&ud,UD_SYN_INTEL);
    while(ud_disassemble(&ud)) {
        printf("\t%s\n",ud_insn_asm(&ud));
    }
}
#endif /* DUMP_ASSEMBLY */

int main( int argc , char* argv[] ) {
  add_func("abs",my_abs);
  add_func("mul",my_mul);
  add_func("div",my_div);
  if( argc != 2 ) {
    fprintf(stderr,"usage: calc 1+2+3*var\n");
    return -1;
  } else {
    void* c = compile(argv[1]);
    assert(c);
#ifdef DUMP_ASSEMBLY
    dump_assemb(c);
#endif /* DUMP_ASSEMBLY */
    func f = (func)c;
    printf("%f\n",f());
    free_jitcode(c);
    return 0;
  }
}


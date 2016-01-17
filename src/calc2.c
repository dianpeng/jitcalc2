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

/* This is a little bit fancier integer calculator it will do the comparison stuff
 * and also unary plus call a C function. But at your own risk , you could screw up
 * since we don't have a full compiler backend to check the prototype is matched or
 * not. */

/* Just X64. Only for fun :) */
|.arch x64
|.actionlist actions

/* Now things become complicated because we allow user to call functions. For simplicity
 * we *ONLY* allow user to call a function with maximum 6 arguments .Because these 6
 * arguments can be passed into register by default on X64 which is simple for us */

|.macro pusharg1, arg1
  | mov edi, arg1
|.endmacro

|.macro pusharg2, arg2
  | mov esi, arg2
|.endmacro

|.macro pusharg3, arg3
  | mov edx, arg3
|.endmacro

|.macro pusharg4, arg4
  | mov ecx, arg4
|.endmacro

|.macro pusharg5, arg5
  | mov r8d, arg5
|.endmacro

|.macro pusharg6, arg6
  | mov r9d,arg6
|.endmacro

/* Fuck linker.
 * We use rbx since it is not that 6 registers that is
 * used to pass parameter and also not use as return value
 * so it is safe to do this before we call into another
 * function */
|.macro callp, addr
   | push rbx
   | mov rbx, (uintptr_t)addr
   | call rbx
   | pop rbx
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
    int num;
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
        tk->val.num = strtol(tk->src+tk->pos,&end,10);
        if(errno) {
          fprintf(stderr,"cannot parse number:%s!",
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
          fprintf(stderr,"too long variable name,more than 32!");
          return -1;
        } else {
          memcpy(tk->val.symbol,tk->src+tk->pos,j-tk->pos);
          tk->val.symbol[j-tk->pos] = 0;
        }
        tk->len = j - tk->pos;
        return (tk->tk = TK_VARIABLE);
      } else {
        fprintf(stderr,"unexpected token character:%c",
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

/* Now the grammar is little bit complicated than before:
 * We have unary and relational operations. We don't stick
 * the precendence with C since it is too much work. We just
 * make *ALL* relation expression has one lowest precendence.
 *
 * EXPRESSION : RELATION
 *
 * RELATION := TERM
 *   RELATION '>'|'>='|'<'|'<='|'=='|'!=' TERM
 * TERM := FACTOR
 *   TERM '+'|'-' FACTOR
 * FACTOR := UNARY
 *   FACTOR '*'|'/' UNARY
 * UNARY := ATOMIC
 *   ('-'|'!'|'+')* ATOMIC
 * ATMOIC := NUMBER |
 *           VARIABLE |
 *           '(' EXPRESSION ')' |
 *           FUNCTION-CALL
 *
 * FUNCTION-CALL := VARIABLE '(' ARG-LIST ')
 *
 * ARG-LIST := // EMPTY
 *   EXPRESSION (',' EXPRESSION)*
 */

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
const char* STRTABLE[100];
size_t STRTABLE_POS = 0;
static const char* add_string( const char* name ) {
  if(STRTABLE_POS==100)
    return NULL;
  else {
    return STRTABLE[STRTABLE_POS++] = strdup(name); /* safe */
  }
}

static int lookup( const char* name ) {
  if(strcmp(name,"defined_var")==0)
    return 100;
  else
    return 0;
}

struct function {
  void* where;
  const char* name;
};

struct function FUNCTABLE[100];
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

enum {
  REG_EAX, /* callee save */
  REG_EBX  /* callee save */
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
    /* until now we haven't use EAX/RAX */
    if(expr(comp,REG_EAX))
      return -1;
    /* push the value into corresponding register for call */
    switch(cnt) {
      case 0:
        | pusharg1 eax
        break;
      case 1:
        | pusharg2 eax
        break;
      case 2:
        | pusharg3 eax
        break;
      case 3:
        | pusharg4 eax
        break;
      case 4:
        | pusharg5 eax
        break;
      case 5:
        | pusharg6 eax
        break;
      default:
        assert(0);
    }
    ++cnt;
    if(cnt == 6) {
      fprintf(stderr,"only 6 arguments is allowed to call a function!");
      return -1;
    }
    /* check comma or not */
    if(comp->tk.tk != TK_COMMA)
      break;
    else
      tk_move(&(comp->tk));
  }
  /* Expect a ) */
  if(comp->tk.tk != TK_RPAR) {
    fprintf(stderr,"The function is not properly closed!");
    return -1;
  }
  /* Emit call crap */
  | callp addr

  /* Now move result accordingly */
  if( REG = REG_EBX ) {
    | mov ebx, eax
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
        /* generate call stub */
        | mov rdi, var
        | callp &lookup
        if( REG == REG_EBX ) {
          | mov ebx, eax
        }
      }
      break;
    }
    case TK_NUMBER: {
      int num = comp->tk.val.num;
      if( REG == REG_EAX ) {
        | mov eax, dword num
      } else {
        | mov ebx, dword num
      }
      break;
    }
    default:
      return -1;
  }
  tk_move(&(comp->tk));
  return 0;
}

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

  /* now we need to call atomic function */
  /* we don't need to save rax because until now we haven't used it */
  if(atomic(comp,REG_EAX)) {
    return -1;
  }
  /* now we know EAX must contain our baby */
  for( i = 0 ; i < sz ; ++i ) {
    switch(op[i]) {
      case TK_SUB:
        | neg eax
      case TK_ADD:
        /* positive sign is crap */
        break;
      case TK_NOT:
        | cmp eax, 0
        | sete al
        | movsx eax,al
        break;
      default:
        assert(0);
    }
  }
  if( REG == REG_EBX ) {
    | mov ebx, eax
  }
  return 0;
}

/* factor */
int factor( struct compiler* comp , int REG ) {
  if(unary(comp,REG_EAX)) {
    return -1;
  }
  else {
    /* now the value is already in EAX */
    do {
      int op;
      if(comp->tk.tk != TK_MUL &&
         comp->tk.tk != TK_DIV ) {
        if( REG == REG_EBX ) {
          | mov ebx, eax
        }
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));

      | push rax
      if(unary(comp,REG_EBX))
        return -1;
      | pop rax

      if(op == TK_MUL) {
        | imul eax, ebx
      } else {
        | cdq
        | idiv ebx
      }

    } while(1);
  }
  return 0;
}

/* term */
int term( struct compiler* comp , int REG ) {
  if(factor(comp,REG_EAX))
    return -1;
  else {
    do {
      int op;
      if(comp->tk.tk != TK_ADD &&
         comp->tk.tk != TK_SUB ) {
        if( REG == REG_EBX ) {
          | mov ebx , eax
        }
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));

      | push rax
      if(factor(comp,REG_EBX))
        return -1;
      | pop rax

      if(op == TK_ADD) {
        | add eax, ebx
      } else {
        | sub eax, ebx
      }
    } while(1);
  }
  return 0;
}

/* Comparison */
int comparison( struct compiler* comp , int REG ) {
  /* generate comparison is simple just need to use
   * CMP/TEST instructions. Since we don't bother for
   * having optimization, mostly we just use CMP */
  if(term(comp,REG_EAX))
    return -1;
  else {
    int op;
    do {
      if(comp->tk.tk != TK_LT &&
          comp->tk.tk != TK_LE &&
          comp->tk.tk != TK_GT &&
          comp->tk.tk != TK_GE &&
          comp->tk.tk != TK_NE &&
          comp->tk.tk != TK_EQ ) {
        if( REG == REG_EBX ) {
          | mov ebx, eax
        }
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));

      | push rax
      if(term(comp,REG_EBX))
        return -1;
      | pop rax

      switch(op) {
        case TK_LT:
          | cmp eax, ebx
          | setl al
          | movsx eax,al
          break;
        case TK_LE:
          | cmp eax, ebx
          | setle al
          | movsx eax,al
          break;
        case TK_GT:
          | cmp eax, ebx
          | setg al
          | movsx eax, al
          break;
        case TK_GE:
          | cmp eax, ebx
          | setge al
          | movsx eax,al
          break;
        case TK_EQ:
          | cmp eax, ebx
          | sete al
          | movsx eax,al
          break;
        case TK_NE:
          | cmp eax, ebx
          | setne al
          | movsx eax,al
          break;
        default:
          assert(0);
      }
    } while(1);
  }
  return 0;
}

int logic( struct compiler* comp , int REG ) {
  if(comparison(comp,REG_EBX))
    return -1;
  else {
    int op;
    int tag = -1;

    do {
      if(comp->tk.tk != TK_AND &&
         comp->tk.tk != TK_OR ) {
        break;
      }
      op = comp->tk.tk;
      tk_move(&(comp->tk));
      check_compiler_tag(comp);

      tag = comp->tag; /* get the tag for current jump position */

      | xor eax, eax
      | cmp ebx, 0
      
      if(op == TK_AND) {
        | je =>tag
      } else {
        | setne al
        | jne =>tag
      }

      if(comparison(comp,REG_EBX))
        return -1;
    } while(1);

    /* normalize value */
    if( tag >= 0 ) {
      |=>tag:
      | cmp eax, 0
      | setne al

      if(REG == REG_EBX) {
        | movzx ebx, al
      } else {
        | movzx eax, al
      }

      ++comp->tag;
    } else {
      /* we don't have a actual logic combinator,
       * so no need to normalize the value */
      if(REG == REG_EAX) {
        | mov eax, ebx
      }
    }
  } 
  return 0;
}

int expr( struct compiler* comp , int REG ) {
  if(logic(comp,REG_EBX))
    return -1;

  if(comp->tk.tk == TK_QUESTION) {
    int tag;
    int exit;
    tk_move(&(comp->tk));

    check_compiler_tag(comp);
    tag = comp->tag;

    check_compiler_tag(comp);
    exit = comp->tag+1;

    | cmp ebx, 0
    | je => tag

    /* first value */
    if(logic(comp,REG_EAX))
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
    if(logic(comp,REG_EAX))
      return -1;

    /* exit location for this tenary */
    |=> exit:

    comp->tag += 2;
  }

  if( REG == REG_EBX ) {
    | mov ebx, eax
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

  comp.dstate = state;

  /* This is a hack. Macro can be pushed and poped
   * but I just don't want this compiler directives */
#undef Dst
#define Dst (&(comp.dstate))

  | push rbx
  /* initialize lexer */
  if(tk_init(&(comp.tk),src) <0)
    return NULL;

  /* start parsing */
  if(expr(&comp,REG_EAX))
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

typedef int (*func)();

/* Add a simple function and make our code fancier */
static int my_abs( int val ) {
  return val > 0 ? val : -val;
}

static int my_mul(int a, int b) {
  return a*b;
}

static int my_div(int a,int b) {
  return a/b;
}

static int my_min(int a,int b) {
  return a > b ? b : a;
}

static int my_max(int a,int b) {
  return a > b ? a : b;
}

int main( int argc , char* argv[] ) {
  add_func("abs",my_abs);
  add_func("mul",my_mul);
  add_func("div",my_div);
  add_func("min",my_min);
  add_func("max",my_max);
  if( argc != 2 ) {
    fprintf(stderr,"usage: calc 1+2+3*var\n");
    return -1;
  } else {
    void* c = compile(argv[1]);
    assert(c);
    func f = (func)c;
    printf("%d\n",f());
    free_jitcode(c);
    return 0;
  }
}

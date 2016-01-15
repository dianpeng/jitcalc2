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

/* Just X64. Only for fun :) */
|.arch x64
|.actionlist actions

/* Please be aware this macro is only
 * good for calling the function that
 * has only 1 argument */
|.macro pusharg, arg1
   | mov rdi, arg1
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
  TK_LPAR,
  TK_RPAR,
  TK_NUMBER,
  TK_VARIABLE,
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

/* Do Compilation
 * term := factor |
 *      := term '+'|'-' factor
 * factor:= atomic |
 *          factor '*'|'/' atomic
 * atomic := NUMBER | VARIABLE */

struct compiler {
  struct tokenizer tk;
  dasm_State* dstate;
};

static int lookup( const char* name ) {
  if(strcmp(name,"defined_var")==0)
    return 100;
  else
    return 0;
}

const char* STRTABLE[100];
size_t STRTABLE_POS = 0;

enum {
  REG_EAX, /* callee save */
  REG_EBX  /* callee save */
};

/* number value is always returned in EAX register */
int atomic( struct compiler* comp , int REG ) {
  /* lex next token from the input */
  switch(comp->tk.tk) {
    case TK_VARIABLE: {
      const char* var;
      var = STRTABLE[STRTABLE_POS++] =
        strdup(comp->tk.val.symbol);
      /* generate call stub */
      | pusharg var
      | callp &lookup
      if( REG == REG_EBX ) {
        | mov ebx, eax
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

/* factor */
int factor( struct compiler* comp , int REG ) {
  if(atomic(comp,REG_EAX)) {
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
      if(atomic(comp,REG_EBX))
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

  /* initialize dynasm dstate */
  dasm_init(&state,1);
  dasm_setup(&state,actions);

  comp.dstate = state;

  /* initialize lexer */
  if(tk_init(&(comp.tk),src) <0)
    return NULL;

  /* start parsing */
  if(term(&comp,REG_EAX))
    return NULL;

  /* This is a hack. Macro can be pushed and poped
   * but I just don't want this compiler directives */
#undef Dst
#define Dst (&(comp.dstate))

  /* emit epilog, no prolog is because on X64
   * we don't need to emit stack frame indeed and
   * inside of our function body we don't use stack
   * at all . Also the return value is already in the
   * EAX register */
  | ret

  state = comp.dstate; /* For safety reason, since it is a pointer 2
                        * pointer, not sure it is modified or not */
  /* get the jited code */
  return jitcode(&state);
}

typedef int (*func)();

int main( int argc , char* argv[] ) {
  if( argc != 2 ) {
    fprintf(stderr,"usage: calc 1+2+3*var\n");
    return -1;
  } else {
    void* c = compile(argv[1]);
    assert(c);
    func f = (func)c;
    printf("%d\n",f());
    return 0;
  }
}

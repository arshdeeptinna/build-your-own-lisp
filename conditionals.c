#include "mpc.h"

#ifdef _WIN32

static char buffer[2048];

char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strnlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

void add_history(char* unused) {}

#else
#include <editline/readline.h>
#endif

#define LASSERT(args, cond, fmt, ...) \
  if (!(cond)) { \
    lval* err = lval_err(fmt, ##__VA_ARGS__); \
    lval_del(args); \
    return err; \
  }

#define LASSERT_ARG(func, args, check, num) \
   LASSERT(args, args->count check num, \
    "Invalid number of arguments to %s. Got %i, Expected %s",\
    func, args->count,num)

#define LASSERT_TYPE(func, args, index, expected_type) \
  LASSERT(args, ltype_name(args->cell[index]->type) == ltype_name(expected_type), \
  "Incorrect type passed to %s. Got %s, Expected %s", \
  func, ltype_name(args->cell[index]->type), ltype_name(expected_type))

#define LGET_BTYPE(exp) \
  btype(exp ? 0 : 1)

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

typedef lval*(*lbuiltin)(lenv*, lval*);

typedef enum { 
  BVAL_TRUE,
  BVAL_FALSE
} BOOL;

struct lval {
  int type;
  double num;
  char* err;
  char* sym;
  BOOL bool;
  lbuiltin builtin;
  lenv* env;
  lval* formals;
  lval* body;
  int count;
  lval** cell;
};

struct lenv {
  lenv* par;
  int count;
  char** syms;
  lval** vals;
};

enum LVAL_TYPE { LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_SEXPR, 
LVAL_QEXPR, LVAL_FUN, LVAL_BOOL };

char* ltype_name(enum LVAL_TYPE type) {
  char* types[] = { "LVAL_NUM", "LVAL_ERR", "LVAL_SYM", "LVAL_SEXPR", "LVAL_QEXPR", "LVAL_FUN", "LVAL_BOOL" };

  return types[type];
}
lval* lval_num(double x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval* lval_bool(BOOL x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_BOOL;
  v->bool = x;
  return v;
}

BOOL btype(int x) {
  if (x == 0) {
    return BVAL_TRUE;
  }
  return BVAL_FALSE;
}

lval* lval_err(char* fmt, ...) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;

  va_list va;
  va_start(va, fmt);

  v->err = malloc(512);

  vsnprintf(v->err, 511, fmt, va);
  v->err = realloc(v->err, strlen(v->err)+1);
  va_end(va);
  return v;
}

lval* lval_sym(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s)+1);
  strcpy(v->sym, s);
  return v;
}

lval* lval_sexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval* lval_qexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval* lval_fun(char* s, lbuiltin func) {
  lval* v = malloc(sizeof(lval));
  v->sym = malloc(strlen(s)+1);
  strcpy(v->sym, s);
  v->type = LVAL_FUN;
  v->builtin = func;
  return v;
}

lenv* lenv_new(void) {
  lenv* e = malloc(sizeof(lenv));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

lval* lval_lambda(lval* formals, lval* body) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;

  v->builtin = NULL;
  v->env = lenv_new();
  v->formals = formals;
  v->body = body;

  return v;
}

void lenv_del(lenv* e);
void lval_del(lval* v) {
  switch (v->type)
  {
    case LVAL_NUM: break;
    case LVAL_BOOL: break;
    case LVAL_FUN: 
      if (!v->builtin) {
        lenv_del(v->env);
        lval_del(v->formals);
        lval_del(v->body);
      } else {
        free(v->sym);
      }
      break;
    case LVAL_SYM: free(v->sym); break;
    case LVAL_ERR: free(v->err); break;
    case LVAL_QEXPR:
    case LVAL_SEXPR: 
      for (int i =0; i < v->count; i++) {
        lval_del(v->cell[i]);
      }
      free(v->cell);
      break;
  }
  free(v);
}

void lenv_del(lenv* e) {
  for(int i = 0; i < e->count; i++) {
    lval_del(e->vals[i]);
    free(e->syms[i]);
  }

  free(e->syms);
  free(e->vals);
  free(e);
}

lval* lval_copy(lval* v);
lval* lenv_get(lenv* e, lval* k) {
  for(int i =0; i < e->count; i++) {
    if(strcmp(e->syms[i], k->sym)==0) {
      return lval_copy(e->vals[i]);
    }
  }
  if(e->par) {
    return lenv_get(e->par, k);
  } else {
    return lval_err("Unbound symbol %s", k->sym);
  }
}

void lenv_put(lenv* e, lval* k, lval* v) {
  for(int i = 0; i < e->count; i++) {
    if(strcmp(e->syms[i], k->sym)==0) {
      lval_del(e->vals[i]);
      e->vals[i] = lval_copy(v);
      return;
    }
  }
  e->count++;
  e->syms = realloc(e->syms, sizeof(char*) * e->count);
  e->vals = realloc(e->vals, sizeof(lval*) * e->count);
  e->vals[e->count-1] = lval_copy(v);
  e->syms[e->count-1] = malloc(strlen(k->sym)+ 1);
  strcpy(e->syms[e->count-1], k->sym);
}

void lenv_def(lenv* e, lval* k, lval* v) {
  while(e->par) { e = e->par; }
  lenv_put(e, k, v);
}

lval* lval_read_num(mpc_ast_t* t) {
  errno=0;
  double x = strtod(t->contents, NULL);
  return errno != ERANGE ?
    lval_num(x) : lval_err("Invalid number: %s", t->contents);
}

lval* lval_add(lval* v, lval* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval* ) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

lval* lval_read(mpc_ast_t* t) {
  if (strstr(t->tag, "number")) { return lval_read_num(t);}
  if (strstr(t->tag, "symbol")) { return lval_sym(t->contents);}

  lval* x = NULL;
  if (strcmp(t->tag, ">") == 0) { x = lval_sexpr(); }
  if (strstr(t->tag, "sexpr")) { x = lval_sexpr();}
  if (strstr(t->tag, "qexpr")) { x = lval_qexpr();}

  for (int i =0; i < t->children_num; i++) {
    if(strcmp(t->children[i]->contents, "(") == 0) { continue;}
    if(strcmp(t->children[i]->contents, ")") == 0) { continue;}
    if(strcmp(t->children[i]->contents, "{") == 0) { continue;}
    if(strcmp(t->children[i]->contents, "}") == 0) { continue;}
    if(strcmp(t->children[i]->tag, "regex") == 0) { continue;}
    x = lval_add(x, lval_read(t->children[i]));
  }
  return x;
}

void lval_print(lval* v);
void lval_expr_print(lval* v, char open, char close) {
  putchar(open);
  for(int i = 0; i < v->count; i++) {
    lval_print(v->cell[i]);
    if (i != v->count-1) {
      putchar(' ');
    }
  }
  putchar(close);
}

void lval_print(lval* v) {
  switch(v->type){
    case LVAL_NUM: 
      if (floor(v->num) == v->num) {
        printf("%.0lf", v->num);
      } 
      else {
        printf("%.4lf", v->num); 
      }
      break;
    case LVAL_FUN:
      if (v->builtin){
        printf("<Function: %s >", v->sym);
      } else {
        printf("(\\ ");
        lval_print(v->formals);
        putchar(' '); 
        lval_print(v->body); 
        putchar(')');
      }
      break;
    case LVAL_ERR:
      printf("Error: %s", v->err);
      break;
    case LVAL_SYM:
      printf("%s", v->sym);
      break;
    case LVAL_QEXPR:
      lval_expr_print(v, '{', '}');
      break;
    case LVAL_SEXPR:
      lval_expr_print(v, '(', ')');
      break;
    case LVAL_BOOL:
      if (v->bool == BVAL_TRUE) {
        printf("true");
      } else {
        printf("false");
      }
  }
}

lenv* lenv_copy(lenv* e);
lval* lval_copy(lval* v) {
  lval* x = malloc((sizeof(lval)));
  x->type = v->type;

  switch(v->type) {
    case LVAL_NUM:
      x->num = v->num;
      break;
    case LVAL_BOOL:
      x->bool = v->bool;
      break;
    case LVAL_FUN:
      if (v->builtin){
        x->builtin = v->builtin;
        x->sym = malloc(strlen(v->sym)+1);
        strcpy(x->sym, v->sym);
      } else {
        x->builtin = NULL;
        x->env = lenv_copy(v->env);
        x->formals = lval_copy(v->formals);
        x->body = lval_copy(v->body);
      }
      break;
    case LVAL_SYM:
      x->sym = malloc(strlen(v->sym)+1);
      strcpy(x->sym, v->sym);
      break;
    case LVAL_ERR:
      x->err = malloc(strlen(v->err)+1);
      strcpy(x->err, v->err);
      break;
    case LVAL_QEXPR:
    case LVAL_SEXPR:
      x->count = v->count;
      x->cell = malloc(sizeof(lval*)*x->count);
      for(int i = 0; i < x->count; i++) {
        x->cell[i] = lval_copy(v->cell[i]);
      }
      break;
  }
  return x;
}

lenv* lenv_copy(lenv* e) {
  lenv* n = malloc(sizeof(lenv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(lval*) * n->count);
  for(int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i])+1);
    strcpy(n->syms[i], e->syms[i]);
    n->vals[i] = lval_copy(e->vals[i]);
  }
  return n;
}

void lval_println(lval* v) {
  lval_print(v);
  putchar('\n');
}

lval* lval_pop(lval* v, int i) {

  lval* x = v->cell[i];

  memmove(&v->cell[i], &v->cell[i+1],
    sizeof(lval*) * (v->count-i-1));
  
  v->count--;

  v->cell = realloc(v->cell, sizeof(lval*) * v->count);

  return x;
}

lval* lval_take(lval* v, int i) {
  lval* x = lval_pop(v, i);

  lval_del(v);
  return x;
}

lval* builtin_op(lenv* e, lval* a, char* op) {
  for(int i = 0; i < a->count; i++) {
    LASSERT_TYPE(op, a, i, LVAL_NUM);
  }

  if (strcmp("-", op) == 0) { LASSERT_ARG(op, a, >, 0);}
  else {LASSERT_ARG(op, a, >, 1);}

  lval* x = lval_pop(a, 0);

  if ((strcmp(op, "-") == 0) && a->count == 0) {
    x->num = -x->num;
  } 

  while (a->count > 0) {
    lval* y = lval_pop(a, 0);
    if (strcmp(op, "+") == 0) { x->num += y->num;}
    if (strcmp(op, "*") == 0) { x->num *= y->num;}
    if (strcmp(op, "^") == 0) { x->num = pow(y->num, x->num);}
    if (strcmp(op, "min") == 0) { x->num = x->num < y->num ? x->num : y->num; }
    if (strcmp(op, "max") == 0) { x->num = x->num > y->num ? x->num : y->num; }
    if (strcmp(op, "/") == 0) {
      if (y->num ==0 ) {
        lval_del(y);
        lval_del(x);
        x = lval_err("Division By Zero!");
        break;
      }
      x->num /= y->num;
    }
    if ((strcmp(op, "%") == 0)) {
      if (floor(x->num) != x->num 
        || floor(y->num) != y->num) {
          lval_del(x);
          lval_del(y);
          x = lval_err("Both values should be integer!");
          break;
        }
      x->num =  (long) x->num % (long)y->num;
    }
    lval_del(y);
  }

  lval_del(a);
  return x;
}

lval* builtin_add(lenv* e, lval* v) {
  return builtin_op(e, v, "+");
}

lval* builtin_sub(lenv* e, lval* v) {
  return builtin_op(e, v, "-");
}

lval* builtin_mul(lenv* e, lval* v) {
  return builtin_op(e, v, "*");
}

lval* builtin_div(lenv* e, lval* v) {
  return builtin_op(e, v, "/");
}

lval* builtin_pow(lenv* e, lval* v) {
  return builtin_op(e, v, "^");
}

lval* builtin_min(lenv* e, lval* v) {
  return builtin_op(e, v, "min");
}

lval* builtin_max(lenv* e, lval* v) {
  return builtin_op(e, v, "max");
}

lval* builtin_rem(lenv* e, lval* v) {
  return builtin_op(e, v, "rem");
}

lval* builtin_head(lenv* e, lval* a) {
  LASSERT_ARG("head", a, ==, 1);


  LASSERT_TYPE("head", a, 0, LVAL_QEXPR);

  LASSERT(a, a->cell[0]->count != 0,
  "Function 'head' passed {}!");

  lval* v = lval_take(a, 0);

  while(v->count > 1) {lval_del(lval_pop(v, 1));}
  return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  LASSERT_ARG("tail", a, ==, 1);

  LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);

  LASSERT(a, a->cell[0]->count != 0,
  "Function 'tail' passed {}!");

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_list(lenv* e,lval *a) {
  a->type = LVAL_QEXPR;
  return a;
}


lval* lval_eval(lenv* e,lval* v);
lval* builtin_eval(lenv* e, lval* a) {
  LASSERT_ARG("eval", a, ==, 1);

  LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);
  
  lval* v = lval_take(a, 0);
  v->type = LVAL_SEXPR;
  return lval_eval(e, v);
}

lval* lval_join(lval* x, lval* y) {
  while(y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }
  lval_del(y);
  return x;
}


lval* builtin_join(lenv* e, lval* a) {

  for(int i =0; i < a->count; i++) {
    LASSERT_TYPE("join", a, i, LVAL_QEXPR);
  }

  lval* x = lval_pop(a, 0);

  while(a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }
  lval_del(a);
  return x;
}

lval* builtin_cons(lenv* e, lval* a) {

  LASSERT_ARG("cons", a, ==, 2);
  LASSERT_TYPE("cons", a, 1, LVAL_QEXPR);
  
  lval* x = lval_qexpr();
  x = lval_add(x, lval_pop(a, 0));

  lval* y = lval_pop(a, 0);

  while(y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }
  lval_del(a);
  lval_del(y);
  return x;
}

lval* builtin_init(lenv* e, lval* a) {
  LASSERT_ARG("init", a, ==, 1);
  
  LASSERT_TYPE("init", a, 0, LVAL_QEXPR);
  
  LASSERT(a, a->cell[0]->count != 0,
  "Function 'init' passed {}!");
  
  lval* x = lval_qexpr();
  lval* y = lval_pop(a, 0);
  while(y->count > 1){
    x = lval_add(x, lval_pop(y, 0));
  }
  lval_del(a);
  lval_del(y);
  return x;
}

lval* builtin_len(lenv* e, lval* a) {
  LASSERT_ARG("len", a, ==, 1);
  
  LASSERT_TYPE("len", a, 0, LVAL_QEXPR);
  
  double count = a->cell[0]->count;
  lval_del(a);
  return lval_num(count);
}

lval* lval_call(lenv* e, lval* f, lval* a) {

  if(f->builtin) { return f->builtin(e, a);}

  int given = a->count;
  int total = f->formals->count;
  while(a->count) {
    if(f->formals->count == 0) {
      lval_del(a);
      return lval_err(
        "Function passed too many arguments. "
        "Got %i, Expected %i.", given, total);
    }
    lval* sym = lval_pop(f->formals, 0);
    if(strcmp(sym->sym, "&")==0) {
      if (f->formals->count != 1) {
        lval_del(a);
        return lval_err("Function format invalid. "
        "Symbol '&' not followed by single symbol.");
      }
      lval* nsym = lval_pop(f->formals, 0);
      lenv_put(f->env, nsym, builtin_list(e, a));
      lval_del(sym);
      lval_del(nsym);
      break;
    }
    lval* val = lval_pop(a, 0);

    lenv_put(f->env, sym, val);
    lval_del(sym);
    lval_del(val);
  }

  lval_del(a);

  if(f->formals->count > 0 &&
    strcmp(f->formals->cell[0]->sym, "&")== 0) {

    if (f->formals->count != 2) {
      return lval_err("Function format invalid. "
      "Symbol '&' not followed by single symbol.");
    }
    lval_del(lval_pop(f->formals, 0));
    lval* sym = lval_pop(f->formals, 0);
    lval* val = lval_qexpr();
    lenv_put(f->env, sym, val);
    lval_del(sym); lval_del(val);
  }
  if (f->formals->count == 0) {
    f->env->par = e;
    return builtin_eval(f->env,
    lval_add(lval_sexpr(), lval_copy(f->body)));
  } else {
    return lval_copy(f);
  }
}

lval* lval_eval_sexpr(lenv* e, lval* v) {
  for(int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(e, v->cell[i]);
  }

  for(int i = 0; i < v-> count; i++) {
    if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i);}
  }

  if (v->count == 0) {return v;}

  if (v->count == 1) { return lval_take(v, 0);}

  lval* f = lval_pop(v, 0);
  if (f->type != LVAL_FUN) {
    lval* err = lval_err(
      "S-Expression starts with incorrect type. "
      "Got %s, Expected %s.",
      ltype_name(f->type), ltype_name(LVAL_FUN));
    lval_del(f);
    lval_del(v);
    return err;
  }
  lval* result = lval_call(e,f, v);
  lval_del(f);
  return result;
}

lval* lval_eval(lenv* e, lval* v) {
  if (v->type == LVAL_SYM) {
    lval* x = lenv_get(e, v);
    lval_del(v);
    return x;
  }
  if (v->type == LVAL_SEXPR){
    return lval_eval_sexpr(e, v);
  }
  return v;
}


lval* builtin_var(lenv* e, lval* a, char* func) {
  LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

  lval* syms = a->cell[0];

  for(int i = 0; i < syms->count; i++) {
    LASSERT(syms, syms->cell[i]->type==LVAL_SYM,
      "Function '%s' cannot define non-symbol. "
      "Got %s, Expected %s.", func,
      ltype_name(syms->cell[i]->type),
      ltype_name(LVAL_SYM));
  }

  LASSERT(a, syms->count == a->count-1,
    "Function '%' passed too many arguments. "
    "Got %i, Expected %i.", func,
    syms->count, a->count-1);
  

  for (int i = 0; i < syms->count; i++) {
    if(strcmp(func, "def")==0) {
      lenv_def(e, syms->cell[i], a->cell[i+1]);
    }
    if (strcmp(func, "=")==0) {
      lenv_put(e, syms->cell[i], a->cell[i+1]);
    }
  }
  lval_del(a);
  return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) {
  return builtin_var(e, a, "def");
}

lval* builtin_fun(lenv* e, lval* a) {
  LASSERT_ARG("fun", a, ==, 2);
  lval* args = lval_pop(a, 0);
  lval* n = lval_qexpr(); 
  n = lval_add(n, lval_pop(args, 0));
  lval* formals = lval_qexpr();
  while (args->count > 0) {
    formals = lval_add(formals, lval_pop(args, 0));
  } 
  lval* body = lval_pop(a,0);
  lval* lambda = lval_lambda(formals, body);
  
  lval* d = lval_qexpr();
  d = lval_add(d, n);
  d = lval_add(d, lambda);
  lval_del(a);
  return builtin_def(e, d);
}

lval* builtin_lambda(lenv* e, lval* a) {
  LASSERT_ARG("\\", a, ==, 2);
  LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
  LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

  for(int i =0; i < a->cell[0]->count; i++) {
    LASSERT(a, a->cell[0]->cell[i]->type == LVAL_SYM,
      "Cannot define non-symbol in %s. Got %s, Expected %s.", "\\",
      ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));
  }
  lval* formals = lval_pop(a, 0);
  lval* vals = lval_pop(a, 0);
  lval_del(a);

  return lval_lambda(formals, vals);

}

lval* builtin_ord(lenv* e, lval* a, char* func) {
  LASSERT_ARG(func, a, ==, 2);
  for (int i = 0; i < a->count; i++) {
    LASSERT_TYPE(func, a, i, LVAL_NUM);
  }
  BOOL bool = BVAL_FALSE;
  if (strcmp(func, ">") == 0) {
    bool = LGET_BTYPE(a->cell[0]->num > a->cell[1]->num);
  }
  else if (strcmp(func, "<") == 0){
    bool = LGET_BTYPE(a->cell[0]->num < a->cell[1]->num);
  }
  else if (strcmp(func, ">=") == 0) {
    bool = LGET_BTYPE(a->cell[0]->num >= a->cell[1]->num);
  }
  else if (strcmp(func, "<=") == 0) {
    bool = LGET_BTYPE(a->cell[0]->num <= a->cell[1]->num);
  }

  lval_del(a);
  return lval_bool(bool);
}

lval* builtin_gt(lenv* e, lval* a) {
  return builtin_ord(e, a, ">");
}

lval* builtin_lt(lenv* e, lval* a) {
  return builtin_ord(e, a, "<");
}

lval* builtin_ge(lenv* e, lval* a) {
  return builtin_ord(e, a, ">=");
}

lval* builtin_le(lenv* e, lval* a) {
  return builtin_ord(e, a, "<=");
}

BOOL lval_cmp(lval* x, lval* y) {
  BOOL val = BVAL_FALSE;
  switch(x->type){
    case LVAL_NUM:
      val = LGET_BTYPE(x->num == y->num);
      break;
    case LVAL_ERR:
      val = LGET_BTYPE(strcmp(x->err, y->err) == 0);
      break;
    case LVAL_QEXPR:
      for (int i = 0; i < x->count; i++) {
        val = LGET_BTYPE(x->cell[i]->type == y->cell[i]->type);
        if (val != BVAL_FALSE) {
          val = lval_cmp(x->cell[i], y->cell[i]);
          if (val == BVAL_FALSE) {
            break;
          }
        } else {
          break;
        }
      }
      break;
    case LVAL_SYM:
      val = LGET_BTYPE(strcmp(x->sym, y->sym) == 0);
      break;
    case LVAL_FUN:
      if (x->builtin || y->builtin){
        val = LGET_BTYPE(x->builtin == y->builtin);
      } else {
        val = lval_cmp(x->formals, y->formals) && lval_cmp(x->body, y->body);
      }
      break;
  }
  return val;
}

lval* builtin_cmp(lenv* e, lval* a, char* func) {
  LASSERT_ARG(func, a, ==, 2);
  BOOL val = BVAL_FALSE;
  if (a->cell[0]->type != a->cell[1]->type) {
    lval_del(a);
    return lval_bool(val);
  }
  switch(a->cell[0]->type) {
    case LVAL_NUM:
    case LVAL_SYM:
    case LVAL_ERR:
    case LVAL_FUN:
      val = lval_cmp(a->cell[0], a->cell[1]);
      break;
    case LVAL_QEXPR:
      if (a->cell[0]->count != a->cell[1]->count) {
        break;
      } else if (a->cell[0]->count == 0) {
        val = LGET_BTYPE(a->cell[0]->count == 0);
        break;
      }
      val = lval_cmp(a->cell[0], a->cell[1]);
      break;
  }
  if (strcmp(func, "!=") == 0) {
    val = !val;
  }
  lval_del(a);
  return lval_bool(val);
}

lval* builtin_eq(lenv* e, lval* a) {
  return builtin_cmp(e, a, "==");
}

lval* builtin_neq(lenv* e, lval* a) {
  return builtin_cmp(e, a, "!=");
}

lval* lval_bool_op(lval* a, char* func, BOOL cmp) {
  for(int i = 0; i < a->count; i++) {
    if (a->cell[i]->bool == cmp) {
      return lval_bool(cmp);
    }
  }
  return lval_bool(! cmp);
}

lval* builtin_bool_op(lenv* e, lval* a, char* func){
  if (strcmp(func, "!") == 0) {
    LASSERT_ARG("!", a, ==, 1);
  } else {
    LASSERT(a, a->count>1, "Incorrect number of arguments for %s."
    " Got %i, Expected atleast %i.", func, a->count, 2);
  }

  for(int i = 0; i < a->count; i++) {
    LASSERT_TYPE(func, a, i, LVAL_BOOL);
  }
  lval* x = NULL;
  if (strcmp(func, "!") == 0) {
    x = lval_bool(! a->cell[0]->bool);
  } else if (strcmp(func, "&&") == 0) {
      x = lval_bool_op(a, "&&", BVAL_FALSE);
  } else if (strcmp(func, "||") == 0 ) {
    x = lval_bool_op(a, "||", BVAL_TRUE);
  } 
  lval_del(a);
  return x;
}

lval* builtin_and(lenv* e, lval* a) {
  return builtin_bool_op(e, a, "&&");
} 

lval* builtin_or(lenv* e, lval* a) {
  return builtin_bool_op(e, a, "||");
} 

lval* builtin_not(lenv* e, lval* a) {
  return builtin_bool_op(e, a, "!");
} 
lval* builtin_if(lenv* e, lval* a) {
  LASSERT_ARG("if", a, ==, 3);
  LASSERT_TYPE("if", a, 0, LVAL_BOOL);
  LASSERT_TYPE("if", a, 1, LVAL_QEXPR);
  LASSERT_TYPE("if", a, 1, LVAL_QEXPR);

  lval* conditional = lval_pop(a, 0);
  lval* exp;
  if (conditional->bool == BVAL_TRUE) {
    a->cell[0]->type = LVAL_SEXPR;
    exp = lval_eval(e, lval_pop(a, 0));
  } else {
    a->cell[1]->type = LVAL_SEXPR;
    exp = lval_eval(e, lval_pop(a, 1));
  }
  lval_del(a);
  lval_del(conditional);
  return exp;
}

void lenv_add_variable(lenv* e, char* name, lval* v) {
  lval* k = lval_sym(name);
  lenv_put(e, k, v);
  lval_del(k), lval_del(v);
}
void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(name, func);
  lenv_put(e, k, v);
  lval_del(k), lval_del(v);
}

void lenv_add_builtins(lenv* e) {
  char* ctrue = "true";
  char* cfalse = "false"; 
  lval* ltrue = lval_bool(BVAL_TRUE);
  lval* lfalse = lval_bool(BVAL_TRUE);
  lenv_add_variable(e, ctrue, ltrue);
  lenv_add_variable(e, cfalse, lfalse);

  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "cons", builtin_cons);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "init", builtin_init);
  lenv_add_builtin(e, "len", builtin_len);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "\\", builtin_lambda);

  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
  lenv_add_builtin(e, "max", builtin_max);
  lenv_add_builtin(e, "min", builtin_min);
  lenv_add_builtin(e, "^", builtin_pow);
  lenv_add_builtin(e, "%", builtin_rem);
  lenv_add_builtin(e, "def", builtin_def);
  lenv_add_builtin(e, "fun", builtin_fun);
  lenv_add_builtin(e, ">", builtin_gt);
  lenv_add_builtin(e, "<", builtin_lt);
  lenv_add_builtin(e, ">=", builtin_ge);
  lenv_add_builtin(e, "<=", builtin_le);
  lenv_add_builtin(e, "==", builtin_eq);
  lenv_add_builtin(e, "!=", builtin_neq);
  lenv_add_builtin(e, "if", builtin_if);
  lenv_add_builtin(e, "||", builtin_or);
  lenv_add_builtin(e, "&&", builtin_and);
  lenv_add_builtin(e, "!", builtin_not);
}

int main(int argc, char** argv) {

  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Sexpr = mpc_new("sexpr");
  mpc_parser_t* Qexpr = mpc_new("qexpr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
  " \
    number: /-?[0-9]+\\.?[0-9]*/; \
    symbol: /[a-zA-Z0-9_+\\-*\\/\\\\=|<>!&]+/ ; \
    sexpr: '(' <expr>* ')'; \
    qexpr: '{' <expr>* '}'; \
    expr: <number> | <symbol> | <sexpr> | <qexpr>; \
    lispy: /^/ <expr>* /$/; \
  ",
  Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  puts("Lispy version 0.0.0.0.2");
  puts("Press Ctrl+c to exit\n");

  lenv* e= lenv_new();
  lenv_add_builtins(e);

  while(1) {
    char* input = readline("Lispy> ");
    add_history(input); 
    mpc_result_t r;

    if (strcmp(input, "exit")== 0) {
      free(input);
      break;
    }
    else if(mpc_parse("<stdin>", input, Lispy, &r)) {
      lval* result = lval_eval(e, lval_read(r.output));
      lval_println(result);
      mpc_ast_delete(r.output);
      lval_del(result);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }
  lenv_del(e);
  mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

  return 0;
}
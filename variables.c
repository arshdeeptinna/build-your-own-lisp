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

#define LASSERT_ARG(args, check, num) \
   LASSERT(args, args->count check num, \
    "Invalid number of arguments")

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

typedef lval*(*lbuiltin)(lenv*, lval*);

struct lval {
  int type;
  double num;
  char* err;
  char* sym;
  lbuiltin fun;
  int count;
  lval** cell;
};

struct lenv {
  int count;
  char** syms;
  lval** vals;
};

enum LVAL_TYPE { LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_SEXPR, LVAL_QEXPR, LVAL_FUN };

char* get_type_by_name(enum LVAL_TYPE type) {
  char* types[] = { "LVAL_NUM", "LVAL_ERR", "LVAL_SYM", "LVAL_SEXPR", "LVAL_QEXPR", "LVAL_FUN" };

  return types[type];
}
lval* lval_num(double x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
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
  v->fun = func;
  return v;
}

lenv* lenv_new(void) {
  lenv* e = malloc(sizeof(lenv));
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

void lval_del(lval* v) {
  switch (v->type)
  {
    case LVAL_NUM: break;
    case LVAL_FUN: free(v->sym); break;
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
  return lval_err("Unbound symbol %s", k->sym);
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
      printf("<Function: %s >", v->sym);
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
  }
}

lval* lval_copy(lval* v) {
  lval* x = malloc((sizeof(lval)));
  x->type = v->type;

  switch(v->type) {
    case LVAL_NUM:
      x->num = v->num;
      break;
    case LVAL_FUN:
      x->fun = v->fun;
      x->sym = malloc(strlen(v->sym)+1);
      strcpy(x->sym, v->sym);
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
    LASSERT(a, a->cell[i]->type == LVAL_NUM,
    "Function %s passed incorrect type for argument %i. "
    "Got %s, Expected LVAL_NUM.",
    op, i, get_type_by_name(a->cell[i]->type));
  }

  if (strcmp("-", op) == 0) { LASSERT_ARG(a, >, 0);}
  else {LASSERT_ARG(a, >, 1);}

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
  LASSERT(a, a->count == 1,
    "Function 'head' passed incorrect number arguments. "
    "Got %i, expected %i.",
    a->count, 1);

  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'head' passed incorrect types. "
    "Got %s, expected LVAL_QEXPR.",
    get_type_by_name(a->cell[0]->type));

  LASSERT(a, a->cell[0]->count != 0,
  "Function 'head' passed {}!");

  lval* v = lval_take(a, 0);

  while(v->count > 1) {lval_del(lval_pop(v, 1));}
  return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  LASSERT(a, a->count == 1,
    "Function 'tail' passed incorrect number of arguments."
    "Got %i, expected %i.",
    a->count, 1);

  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'tail' passed incorrect types."
    "Got %s, expected LVAL_QEXPR.",
    get_type_by_name(a->cell[0]->type));

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
  LASSERT(a, a->count == 1,
    "Function 'eval' passed too many arguments."
    "Got %i, expected %i.",
    a->count, 1);

  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'eval' passed incorrect types."
    "Got %s, expected LVAL_QEXPR.",
    get_type_by_name(a->cell[0]->type));
  
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
    LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
      "Function 'join' passed incorrect types."
      "Got %s, expected LVAL_QEXPR.",
      get_type_by_name(a->cell[i]->type));
  }

  lval* x = lval_pop(a, 0);

  while(a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }
  lval_del(a);
  return x;
}

lval* builtin_cons(lenv* e, lval* a) {

  LASSERT_ARG(a, ==, 2);
  LASSERT(a, a->cell[1]->type==LVAL_QEXPR,
    "Function 'join' passed incorrect types. "
    "Second argument should be a LVAL_QEXPR. "
    "Got %s",
    get_type_by_name(a->cell[1]->type));
  
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
  LASSERT_ARG(a, ==, 1);
  
  LASSERT(a, a->cell[0]->type==LVAL_QEXPR,
    "Function 'init' passed incorrect types. "
    "Got %s, expected LVAL_QEXPR",
    get_type_by_name(a->cell[0]->type));
  
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
  LASSERT_ARG(a, ==, 1);
  
  LASSERT(a, a->cell[0]->type==LVAL_QEXPR,
    "Function 'len' passed incorrect types. "
    "Got %s, expected LVAL_QEXPR",
    get_type_by_name(a->cell[0]->type));
  
  double count = a->cell[0]->count;
  lval_del(a);
  return lval_num(count);
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
    lval_del(f);
    lval_del(v);
    return lval_err("S-expression does not start with a symbol", f);
  }
  lval* result = f->fun(e, v);
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

lval* builtin_def(lenv* e, lval* a) {

  LASSERT(a, a->cell[0]->type==LVAL_QEXPR,
    "Function 'def' passed incorrect type!");

  lval* syms = a->cell[0];

  for(int i = 0; i < syms->count; i++) {
    LASSERT(syms, syms->cell[i]->type==LVAL_SYM,
      "Function 'def' cannot define non-symbol");
  }

  LASSERT(a, syms->count == a->count-1,
    "Function 'def' cannot define incorrect "
    "number of values to symbols");
  
  for (int i = 0; i < syms->count; i++) {
    lenv_put(e, syms->cell[i], a->cell[i+1]);
  }
  lval_del(a);
  return lval_sexpr();
}

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(name, func);
  lenv_put(e, k, v);
  lval_del(k), lval_del(v);
}

void lenv_add_builtins(lenv* e) {
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "cons", builtin_cons);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "init", builtin_init);
  lenv_add_builtin(e, "len", builtin_len);
  lenv_add_builtin(e, "eval", builtin_eval);

  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
  lenv_add_builtin(e, "max", builtin_max);
  lenv_add_builtin(e, "min", builtin_min);
  lenv_add_builtin(e, "^", builtin_pow);
  lenv_add_builtin(e, "%", builtin_rem);
  lenv_add_builtin(e, "def", builtin_def);
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
    symbol: /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ; \
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
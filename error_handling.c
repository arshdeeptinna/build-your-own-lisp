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

typedef struct {
  int type;
  double num;
  int err;
} lval;

enum LVAL_TYPE { LVAL_NUM, LVAL_ERR };
enum LVAL_ERR_TYPE { LERR_BAD_ZERO, LERR_BAD_OP, LERR_BAD_NUM };

lval lval_num(double x) {
  lval v;
  v.type = LVAL_NUM;
  v.num = x;
  return v;
}

lval lval_err(int x) {
  lval v;
  v.type = LVAL_ERR;
  v.err = x;
  return v;
}

void lval_print(lval v) {
  switch(v.type){
    case LVAL_NUM: 
      if (floor(v.num) == v.num) {
        printf("%.0lf", v.num);
      } 
      else {
        printf("%.4lf", v.num); 
      }
      break;
    case LVAL_ERR:
      if (v.err == LERR_BAD_ZERO) {
        printf("Error: Division By Zero!");
      } 
      if (v.err == LERR_BAD_OP) {
        printf("Error: Invalid Operator!");
      }
      if (v.err == LERR_BAD_NUM) {
        printf("Error: Invalid Number!");
      }
      break;
  }
}

void lval_println(lval v) {
  lval_print(v);
  putchar('\n');
}

lval eval_op(lval x, char* op, lval y) {

  if (x.type == LVAL_ERR) { return x; }
  if (y.type == LVAL_ERR) { return y; }
  
  if (strcmp(op, "+") == 0) { return lval_num(x.num + y.num); }
  if (strcmp(op, "-") == 0) { return lval_num(x.num - y.num); }
  if (strcmp(op, "*") == 0) { return lval_num(x.num * y.num); }
  if (strcmp(op, "^") == 0) { return lval_num((long)pow(x.num, y.num)); }
  if (strcmp(op, "min") == 0) { return x.num < y.num ? lval_num(x.num) : lval_num(y.num); }
  if (strcmp(op, "max") == 0) { return x.num > y.num ? lval_num(x.num) : lval_num(y.num); }
  if (strcmp(op, "/") == 0) { 
    return y.num == 0 
      ? lval_err(LERR_BAD_ZERO)
      : lval_num(x.num / y.num); 
  }
  if (strcmp(op, "\%") == 0) { 
    return (floor(x.num) != x.num) || (floor(y.num) != y.num)
      ? lval_err(LERR_BAD_NUM)
      : lval_num((long)x.num % (long)y.num); 
  }
  
  return lval_err(LERR_BAD_OP);
}

lval eval(mpc_ast_t* t) {
  if (strstr(t->tag, "number")) {
    errno = 0;
    double x  = strtod(t->contents, NULL);
    return errno != ERANGE ? lval_num(x): lval_err(x);
  }

  char* op = t->children[1]->contents;

  lval x = eval(t->children[2]);

  int i = 3;
  while(strstr(t->children[i]->tag, "expr")) {
    x = eval_op(x, op, eval(t->children[i]));
    i++;
  }

  return x;
}

long leaves(mpc_ast_t* t) {
  if (strstr(t->tag, "number") || (strstr(t->tag, "operator"))) {
    return 1;
  } 
  long x = leaves(t->children[1]);
  int i = 2;
  while(strstr(t->children[i]->tag, "expr")) {
    x = x + leaves(t->children[i]);
    i++;
  }
  return x;
}

long branches(mpc_ast_t* t) {
  printf("here %s\n", t->contents);
  mpc_ast_print(t);
  if (strstr(t->contents, "(")) {
    return 1;
  }
  long x = branches(t->children[2]);
  int i = 2;
  while(strstr(t->children[i]->tag, "expr")) {
    printf("x: %li\n", x);
    x = x + branches(t->children[i]);
    i++;
  }
  return x;
}

int main(int argc, char** argv) {

  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Operator = mpc_new("operator");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
  " \
    number: /-?[0-9]+\\.?[0-9]*/; \
    operator: '+' | '-' | '*' | '/' | '\%' | '^' | \"min\" | \"max\"; \
    expr: <number> | '(' <operator> <expr>+ ')'; \
    lispy: /^/ <operator> <expr>+ /$/; \
  ",
  Number, Operator, Expr, Lispy);
  // | /^/ <number> /$/; 
  puts("Lispy version 0.0.0.0.2");
  puts("Press Ctrl+c to exit\n");

  while(1) {
    char* input = readline("Lispy> ");
    add_history(input);

    mpc_result_t r;
    if(mpc_parse("<stdin>", input, Lispy, &r)) {
      lval result = eval(r.output);
      lval_println(result);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }
  mpc_cleanup(4, Number, Operator, Expr, Lispy);

  return 0;
}
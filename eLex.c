
/* A simple One-Pass Compiler */
/* global.h */

#include <stdio.h>  /* include declarations for i/o routines */
#include <ctype.h>  /* ... and for character test routines */
#include <stdlib.h> /*and for some standard routines, such as exit*/
#include <string.h> /* ... and for string routines */

#define BSIZE  128  /* buffer size */
#define NONE   -1
#define EOS    '\0'

#define NUM    256
#define DIV    257
#define MOD    258
#define ID     259
#define DONE   260

extern int tokenval;   /*  value of token attribute */  
extern int lineno;

struct entry {  /*  form of symbol table entry  */
  char *lexptr; 
  int  token;
  int  value;
};

struct StackItem {
  int t;
  int tval;
};

struct StackTrace {
    char* description;
    struct StackTrace* next;
};

extern struct entry symtable[];  /* symbol table  */
void emit (int t, int tval);  /*  generates output  */
void error(char* m);  /* generates all error messages  */

void stack1Push(int t, int tval); /* Pushes to the stack 1 */
struct StackItem stack1Pop(); /* Pops from the stack 1 */
void stack2Push(int t, int tval); /* Pushes to the stack 2 */
struct StackItem stack2Pop(); /* Pops from the stack 2 */
char* calculateStack(); /* Calculates the stack contents */
int stack1Size(); /* The current size of the stack 1 */
int stack2Size(); /* The current size of the stack 2 */

//-----------------------------------------------------

/* lexer.c */

char lexbuf[BSIZE];
int  lineno = 1;
int  tokenval = NONE;

int lexan (){  /*  lexical analyzer  */
  int t;
  while(1) {
    t = getchar ();
    if (t == ' ' || t == '\t')
      ;  /*  strip out white space  */
    else if (t == '\n')
      lineno = lineno + 1;
    else if (isdigit (t)) {  /*  t is a digit  */
      ungetc(t, stdin);
      scanf("%d", &tokenval);
      return NUM;
    }
    else if (isalpha(t)) {  /*  t is a letter */
      int p, b = 0;
      while (isalnum(t)) {  /* t is alphanumeric  */
        lexbuf [b] = t; 
        t = getchar ();
        b = b + 1;
        if (b >= BSIZE)
          error("compiler error");
      }
      lexbuf[b] = EOS;
      if (t != EOF)
        ungetc(t, stdin);
      p = lookup (lexbuf);
      if (p == 0)
        p = insert (lexbuf, ID);
      tokenval = p;
      return symtable[p].token;
    }
    else if (t == EOF)
      return DONE;
    else {
      tokenval = NONE;
      return t;
    }
  }
}


//-----------------------------------------------------

/* parser.c -- without the optimizations */

int lookahead;

char* match(int);
void start(), list();
char* expr();
char* moreterms();
char* term();
char* morefactors();
char* factor();

void parse()  /*  parses and translates expression list  */
{
  lookahead = lexan();
  start();
}

void start ()
{
  /* Just one production for start, so we don't need to check lookahead */
  list(); match(DONE);
}

void finishExpression(char* err){
    if (err != NULL)
        printf("ERR: %s\n", err);
    while (lookahead != ';')
        lookahead = lexan();
    parse();
}

void list()
{
  if (lookahead == '(' || lookahead == ID || lookahead == NUM) {
    char* err = expr();
    if (err == NULL) {
        err = calculateStack();
        if (err == NULL){
            err = match(';');
            if (err == NULL) {
                list();
            } else {
                finishExpression(err);
            }
        } else {
            finishExpression(err);
        }
    } else {
        finishExpression(err);
    }
  }
  else {
    /* Empty */
    finishExpression(NULL);
  }
}

char* expr ()
{
  char* err;
  if (lookahead == ID){
    emit(lookahead, tokenval); match(ID);
    if (lookahead == '=') {
        err = match('='); if (err != NULL) return err;
        err = term(); if (err != NULL) return err;
        err = moreterms(); if (err != NULL) return err;
        emit('=', tokenval);
    } else
        err = moreterms(); if (err != NULL) return err;
  } else{
    err = term(); if (err != NULL) return err;
    err = moreterms(); if (err != NULL) return err;
  }
  
  return NULL;
}

char* moreterms()
{
  char* err;
  if (lookahead == '+') {
    err = match('+'); if (err != NULL) return err;
    err = term(); if (err != NULL) return err;
    emit('+', tokenval);
    err = moreterms(); if (err != NULL) return err;
  }
  else if (lookahead == '-') {
    err = match('-'); if (err != NULL) return err;
    err = term(); if (err != NULL) return err;
    emit('-', tokenval);
    err = moreterms(); if (err != NULL) return err;
  }
  else {
    /* Empty */
  }
  
  return NULL;
}

char* term ()
{
  /* Just one production for term, so we don't need to check lookahead */
  char* err;
  err = factor(); if (err != NULL) return err;
  err = morefactors(); if (err != NULL) return err;
  
  return NULL;
}

char* morefactors ()
{
  char* err;
  if (lookahead == '*') {
    err = match('*'); if (err != NULL) return err;
    err = factor(); if (err != NULL) return err;
    emit('*', tokenval);
    err = morefactors(); if (err != NULL) return err;
  }
  else if (lookahead == '/') {
    err = match('/'); if (err != NULL) return err;
    err = factor(); if (err != NULL) return err;
    emit('/', tokenval);
    err = morefactors(); if (err != NULL) return err;
  }
  else if (lookahead == DIV) {
    err = match(DIV); if (err != NULL) return err;
    err = factor(); if (err != NULL) return err;
    emit(DIV, tokenval);
    err = morefactors(); if (err != NULL) return err;
  }
  else if (lookahead == MOD) {
    err = match(MOD); if (err != NULL) return err;
    err = factor(); if (err != NULL) return err;
    emit(MOD, tokenval);
    err = morefactors(); if (err != NULL) return err;
  }
  else {
    /* Empty */
  }
  
  return NULL;
}

char* factor ()
{
  char* err;
  if (lookahead == '(') {
    err = match('('); if (err != NULL) return err;
    err = expr(); if (err != NULL) return err;
    err = match(')'); if (err != NULL) return err;
  }
  else if (lookahead == ID) {
    int id_lexeme = tokenval;
    err = match(ID); if (err != NULL) return err;
    emit(ID, id_lexeme);
  }
  else if (lookahead == NUM) {
    int num_value = tokenval;
    err = match(NUM); if (err != NULL) return err;
    emit(NUM, num_value);
  }
  else
    return "syntax error in factor";
    
  return NULL;
}

char* match(int t)
{
  char* err;
  if (lookahead == t)
    lookahead = lexan();
  else{
    return "syntax error in match";
  }
  
  return NULL;
}

//-----------------------------------------------------

/* emitter.c */
void emit (int t, int tval)  /*  generates output  */
{

  stack1Push(t, tval);
  return;
    
  switch(t) {
  case '+' : case '-' : case '*' : case '/': case '=':
    printf("%c\n", t); 
    break;
  case DIV:
    printf("DIV\n"); break; 
  case MOD:
    printf("MOD\n"); break;
  case NUM:
    printf("%d\n", tval); break;
  case ID:
    printf("%s\n", symtable[tval].lexptr); break; 
  default:     
    printf("token %d, tokenval %d\n", t, tval);
  }
}

//-----------------------------------------------------

/* symbol.c */

#define STRMAX 999  /*  size of lexemes array  */
#define SYMMAX 100  /*  size of symbol table */

char lexemes[STRMAX];
int  lastchar = - 1;  /*  last used position in lexemes   */
struct entry symtable[SYMMAX];
int lastentry = 0;    /*  last used position in symtable  */

int lookup(char *s)      /*  returns position of entry for s */
{
  int p;
  for (p = lastentry; p > 0; p = p - 1)
    if (strcmp(symtable[p].lexptr, s) == 0)
      return p;
  return 0;
}

int insert(char *s, int tok)/*returns position of entry for s*/
{
  int len;
  len = strlen(s);  /*  strlen computes length of s     */
  if (lastentry + 1 >= SYMMAX)
    error ("symbol table full");
  if (lastchar + len + 1 >= STRMAX)
    error ("lexemes array full");
  lastentry = lastentry + 1;
  symtable[lastentry].token = tok;
  symtable[lastentry].lexptr = &lexemes[lastchar + 1];
  lastchar = lastchar + len + 1;
  strcpy(symtable[lastentry].lexptr, s);
  return lastentry;
}

//-----------------------------------------------------

/* init.c */

struct entry keywords[] = {
  { "div", DIV },
  { "mod", MOD, },
  { 0,     0 }
};

void init()  /*  loads keywords into symtable  */
{
  struct entry *p;
  for (p = keywords; p->token; p++)
    insert(p->lexptr, p->token);
}


//-----------------------------------------------------

/* error.c */


void error(char* m)  /* generates all error messages  */
{
  fprintf(stderr, "line %d: %s\n", lineno, m);
  //exit(EXIT_FAILURE);  /*  unsuccessful termination  */
}


//-----------------------------------------------------

/* main.c */


int main( )
{
  init();
  parse();
  exit(0);    /*  successful termination  */
}



//------------------------------------------------------

/* stack.c */

struct StackItem stack1[1024];
int stack1Pointer = -1;
struct StackItem stack2[1024];
int stack2Pointer = -1;

void stack1Push(int t, int tval){
    struct StackItem a;
    a.t = t;
    a.tval = tval;
    stack1Pointer++;
    stack1[stack1Pointer] = a;
}

int stack1Size(){
    return stack1Pointer + 1;
}


int stack2Size(){
    return stack2Pointer + 1;
}

struct StackItem stack1Pop(){
    if (stack1Pointer == -1){
        struct StackItem a;
        a.t = NONE;
        return a;
    }
    return stack1[stack1Pointer--];
}

void stack2Push(int t, int tval){
    struct StackItem a;
    a.t = t;
    a.tval = tval;
    stack2Pointer++;
    stack2[stack2Pointer] = a;
}

struct StackItem stack2Pop(){
    if (stack2Pointer == -1) {
        struct StackItem a;
        a.t = NONE;
        return a;
    }
    return stack2[stack2Pointer--];
}

void stack2BackToStack1(){
    struct StackItem tmpItem = stack2Pop();
    while (tmpItem.t != NONE){
        stack1Push(tmpItem.t, tmpItem.tval);
        
        tmpItem = stack2Pop();
    }
}

void printStack1(){
    struct StackItem i = stack1Pop();
    printf("---------------Stack-----------\n");
    int count = 0;
    while (i.t != NONE){
        count++;
        stack2Push(i.t, i.tval);
        if (i.t == ID)
            printf("%s\n", symtable[i.tval].lexptr);
        else if (i.t == NUM)
            printf("%d\n", i.tval);
        else
            printf("%c\n", i.t);
            
        i = stack1Pop();
    }
    
    for (int j = 0; j < count; j++){
        i = stack2Pop();
        stack1Push(i.t, i.tval);
    }
    
    printf("----------------End------------\n");
}

int aaa = 1;
char* calculateStack(){
    struct StackItem item = stack1Pop();
    int lastOperator = NONE;
    int hasLastNum = 0; // as boolean
    int lastNum;
    char* err;
    
    while (item.t != NONE){
        switch (item.t){
            case '+':
            case '-':
            case '*':
            case '/':
            case DIV:
            case MOD:
            case '=':
                if (lastOperator != NONE)
                    stack2Push(lastOperator, NONE);
                lastOperator = item.t;
                break;
            case NUM:
                if (hasLastNum) {
                    int result;
                    switch (lastOperator){
                        case '+':
                            result = item.tval + lastNum;
                            break;
                        case '-':
                            result = item.tval - lastNum;
                            break;
                        case '*':
                            result = item.tval * lastNum;
                            break;
                        case '/':
                        case DIV:
                            result = item.tval / lastNum;
                            break;
                        case MOD:
                            result = item.tval % lastNum;
                            break;
                        default:
                            printStack1();
                            return "Invalid operation";
                    }
                    stack1Push(NUM, result);
                    
                    stack2BackToStack1();
                    hasLastNum = 0;
                    lastOperator = NONE;
                } else {
                    lastNum = item.tval;
                    hasLastNum = 1;
                }
                break;
            case ID:
                if (lastOperator == '='){
                    if (hasLastNum){
                        // put last num to symtable
                        symtable[item.tval].value = lastNum;
                    } else {
                        return "Nothing to asign to variable";
                    }
                } else {
                    // add value of id to the stack1
                    stack1Push(NUM, symtable[item.tval].value);
                    if (hasLastNum)
                        stack1Push(NUM, lastNum);
                    if (lastOperator != NONE)
                        stack1Push(lastOperator, NONE);
                    stack2BackToStack1();
                    hasLastNum = 0;
                    lastOperator = NONE;
                }
        }
        
        item = stack1Pop();
    }
    
    printf("%d\n", lastNum);
    
    
    // Empty second stack
    item = stack2Pop();
    while(item.t != NONE)
        item = stack2Pop();
        
    return NULL;
}









/*  Copyright (C) 2008 by www.guidealgoritmi.it
    Author: Vincenzo Lo Cicero.
    e-mail: vincenzolocicero@guidealgoritmi.it
    http://www.guidealgoritmi.it

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/


/*

This version of EvalInfix includes a wrapper to allow calls from
fortran code (written by Lorenzo Paulatto, 2008).

An example F90 program follows:

PROGRAM use_ex
   implicit none
   character(len=256) :: expr
   integer :: ierr
   real(8) :: result
   real(8),external :: eval_infix

   expr = "3 * 3"
   result = eval_infix(ierr, expr)
   if (ierr == 0) then
      write(*,*) result, expr
   else
      stop
   endif
END PROGRAM

*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#include "c_defs.h"

/* #pragma warning( disable : 4996 ) */

#define MAXOP        100  /* dimensione massima di un operando o operatore */
#define MAXSTACK    100  /* dimensione massima dello stack */


typedef int        BOOL;

#ifndef FALSE
#define FALSE    0
#endif

#ifndef TRUE
#define TRUE    1
#endif


typedef enum tagTokenType
{
    EOL, UNKNOWN, VALUE, OPAREN, CPAREN, EXP, UPLUS, UMINUS, MULT, DIV, PLUS, MINUS
}TokenTypeEnum;

typedef struct tagToken
{
    TokenTypeEnum Type;
    char str[54];
    double Value;
}Token;

struct Precedence
{
    int inputSymbol;
    int topOfStack;
} PREC_TABLE [ ] =
{
    { 0, -1 }, {-1, -1}, { 0, 0 },    /* EOL, UNKNOWN, VALUE */
    { 100, 0 }, { 0, 99 },            /* OPAREN, CPAREN */
    { 6, 5 }, {6, 5}, {6, 5},         /* EXP, UPLUS, UMINUS */
    { 3, 4 }, { 3, 4 },               /* MULT, DIV */
    { 1, 2 }, { 1, 2 }                /* PLUS, MINUS */
};

int nNextPos = 0;
TokenTypeEnum PreviousTokenType = EOL;

int sp_op = 0;
Token stack_op[MAXSTACK];  /* stack degli operatori */

/* Operazioni sullo stack degli operatori */
void push_op(Token, char *);
Token pop_op(char *);
Token top_op(char *);
BOOL is_empty_op();

int sp_val = 0;
double stack_val[MAXSTACK];  /* stack degli operandi */

/* Operazioni sullo stack degli operandi */
void push_val(double, char *);
double pop_val(char *);
double top_val(char *);
BOOL is_empty_val();

TokenTypeEnum GetNextToken(const char *str, Token *token, BOOL bIsInfix);

double BinaryOperation(double left, double right, char op, char *strError);
/*BOOL InfixToPostfix(const char *strInfix, char *strPostfix, char *strError);
  double EvalPostfix(const char *strExpression, char *strError); */
double EvalInfix(const char *strExpression, char *strError);

/* inserisce un elemento nello stack degli operatori */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
void push_op(Token Tok, char *strError)
{
    strcpy(strError, "");

    if (sp_op < MAXSTACK)
        stack_op[sp_op++] = Tok;
    else
        sprintf(strError, "Error: operators stack is full, cannot add more elements %c\n", Tok.str[0]);
}

/* Estrae e ritorna un elemento dallo stack degli operatori */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
Token pop_op(char *strError)
{
    Token tok_temp;

    strcpy(strError, "");

    if (sp_op > 0)
        return stack_op[--sp_op];
    else
    {
        sprintf(strError, "Error: missing operator\n");
        strcpy(tok_temp.str, "");
        tok_temp.Type = UNKNOWN;
        return tok_temp;
    }
}

/* Ritorna il valore in cima allo stack degli operatori senza estrarlo */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
Token top_op(char *strError)
{
    Token tok_temp;

    strcpy(strError, "");

    if (sp_op >= 0)
        return stack_op[sp_op - 1];
    else
    {
        sprintf(strError, "Error: missing operator\n");
        strcpy(tok_temp.str, "");
        tok_temp.Type = UNKNOWN;
        return tok_temp;
    }
}

/* Ritorna un valore diverso da zero se lo stack degli operatori è vuoto */
BOOL is_empty_op()
{
    if ( sp_op > 0 )
        return FALSE;
    else
        return TRUE;
}

/* Inserisce un elemento nello stack degli operandi */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
void push_val(double c, char *strError)
{
    strcpy(strError, "");

    if (sp_val < MAXSTACK)
        stack_val[sp_val++] = c;
    else
        sprintf(strError, "Error: values stack is full: cannot add more elements %g\n", c);
}

/* Estrae e ritorna un elemento dallo stack degli operandi */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
double pop_val(char *strError)
{
    strcpy(strError, "");

    if (sp_val > 0)
        return stack_val[--sp_val];
    else
    {
        sprintf(strError, "Error: missing operand\n");
        return 0;
    }
}

/* ritorna il valore in cima allo stack degli operandi senza estrarlo */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
double top_val(char *strError)
{
    strcpy(strError, "");

    if (sp_val > 0)
        return stack_val[sp_val - 1];
    else
    {
        sprintf(strError, "Error top: values stack is empty\n");
        return 0;
    }
}

/* ritorna un valore diverso da zero se lo stack degli operandi è vuoto */
BOOL is_empty_val()
{
    if ( sp_val > 0 )
        return FALSE;
    else
        return TRUE;
}
/* ritorna un valore diverso da zero per "e", "E", "d o "D", o se il carattere prima lo era */
BOOL is_scientific(char strChar)
{
    BOOL static was_scientific = FALSE;

    if (was_scientific)
    {
        was_scientific = FALSE;
        return TRUE;
    }
    else if (strChar == 'e' || strChar == 'E'||
             strChar == 'd' || strChar == 'D')
    {
        was_scientific = TRUE;
        return TRUE;
    }
    else if ( isdigit(strChar) ){
        was_scientific = FALSE;
        return TRUE;
        }
    else
    {
        was_scientific = FALSE;
        return FALSE;
    }
}


/* Analizzatore lessicale */
TokenTypeEnum GetNextToken(const char *str, Token *token, BOOL bIsInfix)
{
    int i;
    char strToken[MAXOP];

    while ( 1 )
    {
        while ( str[nNextPos++] == ' ' )
            ;
        --nNextPos;

        if ( str[nNextPos] == '\0' )
        {
            token->Type = EOL;
            strcpy(token->str, "\n");
            nNextPos = 0;
            PreviousTokenType = EOL;
            return EOL;
        }
        else if ( is_scientific(str[nNextPos]) )
        {
            i = 0;
            while ( is_scientific(strToken[i++] = str[nNextPos++]) )
                if (strToken[i-1] == 'd' || strToken[i-1] == 'D') strToken[i-1] = 'e';
            if ( str[nNextPos - 1] == '.' )
            {
                while ( is_scientific(strToken[i++] = str[nNextPos++]) )
                if (strToken[i-1] == 'd' || strToken[i-1] == 'D') strToken[i-1] = 'e';
                strToken[i - 1] = '\0';
                --nNextPos;
                token->Type = VALUE;
                strcpy(token->str, strToken);
                token->Value = atof(strToken);
                return VALUE;
            }
            else
            {
                strToken[i - 1] = '\0';
                --nNextPos;
                token->Type = VALUE;
                strcpy(token->str, strToken);
                token->Value = atof(strToken);
                return VALUE;
            }
        }
        else if ( str[nNextPos] == '.' )
        {
            i = 0;
            strToken[i++] = str[nNextPos++];
            while ( is_scientific(strToken[i++] = str[nNextPos++]) )
                if (strToken[i-1] == 'd' || strToken[i-1] == 'D') strToken[i-1] = 'e';
            strToken[i - 1] = '\0';
            --nNextPos;
            token->Type = VALUE;
            strcpy(token->str, strToken);
            token->Value = atof(strToken);
            return VALUE;
        }
        else if ( str[nNextPos] == '(' )
        {
            token->Type = OPAREN;
            strcpy(token->str, "(");
            ++nNextPos;
            return OPAREN;
        }
        else if ( str[nNextPos] == ')' )
        {
            token->Type = CPAREN;
            strcpy(token->str, ")");
            ++nNextPos;
            return CPAREN;
        }
        else if ( str[nNextPos] == '+' )
        {
            strcpy(token->str, "+");
            ++nNextPos;
            if ( !bIsInfix )
            {
                token->Type = PLUS;
                return PLUS;
            }
            else
            {
                if ( PreviousTokenType == CPAREN || PreviousTokenType == VALUE )
                {
                    token->Type = PLUS;
                    return PLUS;
                }
                else
                {
                    token->Type = UPLUS;
                    return UPLUS;
                }
            }
        }
        else if ( str[nNextPos] == '-' )
        {
            strcpy(token->str, "-");
            ++nNextPos;
            if ( !bIsInfix )
            {
                token->Type = MINUS;
                return MINUS;
            }
            else
            {
                if ( PreviousTokenType == CPAREN || PreviousTokenType == VALUE )
                {
                    token->Type = MINUS;
                    return MINUS;
                }
                else
                {
                    token->Type = UMINUS;
                    return UMINUS;
                }
            }
        }
        else if ( str[nNextPos] == '~' )
        {
            strcpy(token->str, "~");
            ++nNextPos;
            if ( !bIsInfix )
            {
                token->Type = UMINUS;
                return UMINUS;
            }
            else
            {
                token->Type = UNKNOWN;
                return UNKNOWN;
            }
        }
        else if ( str[nNextPos] == '*' )
        {
            token->Type = MULT;
            strcpy(token->str, "*");
            ++nNextPos;
            return MULT;
        }
        else if ( str[nNextPos] == '/' )
        {
            token->Type = DIV;
            strcpy(token->str, "/");
            ++nNextPos;
            return DIV;
        }
        else if ( str[nNextPos] == '^' )
        {
            token->Type = EXP;
            strcpy(token->str, "^");
            ++nNextPos;
            return EXP;
        }
        else
        {
            token->Type = UNKNOWN;
            token->str[0] = str[nNextPos];
            token->str[1] = '\0';
            ++nNextPos;
            return UNKNOWN;
        }
    }

    return EOL;
}

/* Ritorna il risultato di un'operazione binaria */
/* In caso di errore viene riportato un messaggio nel parametro strError */
/* In assenza di errori, il parametro strError è impostato ala stringa vuota = "" */
double BinaryOperation(double left, double right, char op, char* strError)
{
    strcpy(strError, "");

    switch ( op )
    {
    case '-':
        return left - right;
    case '+':
        return left + right;
    case '*':
        return left * right;
    case '/':
        if ( right == 0 )
        {
            sprintf(strError, "Error: division by zero!\n");
            return 0.0;
        }
        else
            return left / right;
    case '^':
        return pow(left, right);
    default:
        if ( op == '(' )
            sprintf(strError, "Error: unbalanced brackets.\n");
        else
            sprintf(strError, "Error: unknown operator: %c\n", op);
        return 0.0;
    }
}

/* Calcola e restituisce il risultato di un'espressione in forma infissa */
double EvalInfix(const char *strExpression, char * strError)
{
    int i = 0;
    Token tok;
    Token tok_temp;
    double left, right;
    double dblRet;

    strcpy(strError, "");

    tok_temp.Type = EOL;
    tok_temp.str[0] = '@';
    tok_temp.str[1] = '\0';
    push_op(tok_temp, strError);
    if ( strError[0] != '\0' )
        return 0.0;

    while ( (PreviousTokenType = GetNextToken(strExpression, &tok, TRUE)) != EOL )
    {
        if ( tok.Type == UNKNOWN )
        {
            sprintf(strError, "Error: invalid token: %s\n", tok.str);
            return 0.0;
        }
        else if ( tok.Type == VALUE )
        {
            push_val(tok.Value, strError);
            if ( strError[0] != '\0' )
                return 0.0;
        }
        else if ( tok.Type == OPAREN || tok.Type == UMINUS || tok.Type == UPLUS )
        {
            push_op(tok, strError);
            if ( strError[0] != '\0' )
                return 0.0;
        }
        else if ( tok.Type == CPAREN )
        {
            while ( top_op(strError).Type != OPAREN )
            {
                if ( strError[0] != '\0' )
                    return 0.0;

                tok_temp = pop_op(strError);
                if ( strError[0] != '\0' )
                    return 0.0;

                if ( (tok_temp.Type == EOL) || (is_empty_op()) )
                {
                    sprintf(strError, "Error: unbalanced brackets.\n");
                    return 0.0;
                }

                right = pop_val(strError);
                if ( strError[0] != '\0' )
                    return 0.0;

                if ( tok_temp.Type != UMINUS )
                {
                    left = pop_val(strError);
                    if ( strError[0] != '\0' )
                        return 0.0;

                    dblRet = BinaryOperation(left, right, tok_temp.str[0], strError);
                    if ( strError[0] != '\0' )
                        return 0.0;

                    push_val(dblRet, strError);
                    if ( strError[0] != '\0' )
                        return 0.0;
                }
                else
                {
                    push_val( -1 * right, strError );
                    if ( strError[0] != '\0' )
                        return 0.0;
                }
            }
            pop_op(strError);
            if ( strError[0] != '\0' )
                return 0.0;
        }
        else
        {
            while ( PREC_TABLE[ top_op(strError).Type ].topOfStack >= PREC_TABLE[ tok.Type ].inputSymbol )
            {
                if ( strError[0] != '\0' )
                    return 0.0;

                if ( top_op(strError).Type != UMINUS && top_op(strError).Type != UPLUS )
                {
                    if ( strError[0] != '\0' )
                        return 0.0;

                    right = pop_val(strError);
                    if ( strError[0] != '\0' )
                        return 0.0;

                    left = pop_val(strError);
                    if ( strError[0] != '\0' )
                        return 0.0;

                    tok_temp = pop_op(strError);
                    if ( strError[0] != '\0' )
                        return 0.0;

                    dblRet = BinaryOperation(left, right, tok_temp.str[0], strError);
                    if ( strError[0] != '\0' )
                        return 0.0;

                    push_val(dblRet, strError);
                    if ( strError[0] != '\0' )
                        return 0.0;
                }
                else
                {
                    if ( top_op(strError).Type == UMINUS )
                    {
                        if ( strError[0] != '\0' )
                            return 0.0;

                        right = pop_val(strError);
                        if ( strError[0] != '\0' )
                            return 0.0;

                        pop_op(strError);
                        if ( strError[0] != '\0' )
                            return 0.0;

                        push_val(-1 * right, strError);
                        if ( strError[0] != '\0' )
                            return 0.0;
                    }
                    else
                    {
                        pop_op(strError);
                        if ( strError[0] != '\0' )
                            return 0.0;
                    }
                }
            }

            if ( tok.Type != EOL )
            {
                push_op(tok, strError);
                if ( strError[0] != '\0' )
                    return 0.0;
            }
        }
    }

    while ( 1 )
    {
        tok_temp = pop_op(strError);
        if ( strError[0] != '\0' )
            return 0.0;

        if ( tok_temp.Type == EOL )
            break;

        if ( tok_temp.Type != UPLUS )
        {
            right = pop_val(strError);
            if ( strError[0] != '\0' )
                return 0.0;
        }

        if ( tok_temp.Type != UMINUS && tok_temp.Type != UPLUS )
        {
            left = pop_val(strError);
            if ( strError[0] != '\0' )
                return 0.0;

            dblRet = BinaryOperation(left, right, tok_temp.str[0], strError);
            if ( strError[0] != '\0' )
                return 0.0;

            push_val(dblRet, strError);
            if ( strError[0] != '\0' )
                return 0.0;
        }
        else
        {
            push_val( -1 * right, strError );
            if ( strError[0] != '\0' )
                return 0.0;
        }
    }

    dblRet = pop_val(strError);
    if ( strError[0] != '\0' )
        return 0.0;

    if ( is_empty_val() )
    {
        return dblRet;
    }
    else
    {
        sprintf(strError, "Error: malformed expression.\n");
        return 0.0;
    }
}

double eval_infix( int *ierr, const char *strExpression, int len )
{
  double result = 0.0;
  char strHelper[257];
  char strError[257];
  int i;

  /* maximum length of strExpression is 256 chars */
  if (len>256) {
    printf("[eval_infix.c] expression longer than 256 characters\n");
    ierr[0] = 1;
    return result;
  }

  /* it's safer to reformat strings for C, with null terminator '\0' */
  for(i=0;i<len;i++) strHelper[i]=' ';
  strHelper[len] = '\0'; 

  for(i=0;i<len;i++) strHelper[i] = strExpression[i];

  for(i=0;i<len;i++) strError[i] = ' ';
  strError[len] = '\0';

  result =  EvalInfix(strHelper, strError);

  if ( strError[0] != '\0' ) {
    printf("[eval_infix.c] A parsing error occurred\n");
    /*printf("input string: \n----\n%s\n----\n", strExpression);*/
    printf("helper string:\n%s\n", strHelper);
    printf("error code:   \n%s\n", strError);
    ierr[0] = 1;
  }
  else ierr[0] = 0;

  return result;
}

/* FORTRAN interface */
double F77_FUNC_(eval_infix_wrapper,EVAL_INFIX_WRAPPER)( int *ierr, const int *strExpression, const int *len )
{
	int    i;
        double result = 0.0;
	char   tmp[256];
	if( *len < 1 ) {
		*ierr = 2;
	} else if( *len > 256 ) {
		*ierr = 3;
	} else {
		for( i = 0; i < *len; i ++ ) {
			tmp[i] = (char)strExpression[i];
		}
		result = eval_infix( ierr, tmp, *len );
	}
	return result;
}

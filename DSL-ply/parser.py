import ply.yacc as yacc
import ply.lex as lex
from ply import *
import sys

keywords = (
    'LET', 'READ', 'DATA', 'PRINT', 'GOTO', 'IF', 'THEN', 'FOR', 'NEXT', 'TO', 'STEP',
    'END', 'STOP', 'DEF', 'GOSUB', 'DIM', 'REM', 'RETURN', 'RUN', 'LIST', 'NEW',
)

'''
Definicao o conjunto dos tokens
'''

tokens = [
    'NUMBER',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'DIVIDEINT',
    'QUOTIENT',
    'EXPONENT',
    'LPAR',
    'RPAR',
    'EQUALS',
    'NAME',
    'LCHAVES',
    'RCHAVES',
    'COMMA',
    'ENDLINE',
    'DEF'
]

# Definindo os tokens onde tokens = conjunto

t_PLUS = r'\+'
t_MINUS = r'\-'
t_DIVIDE = r'\/'
t_MULTIPLY = r'\*'
t_EQUALS = r'\='
t_DIVIDEINT = r'\/\/'
t_QUOTIENT = r'\%'
t_EXPONENT = r'\^'
t_LPAR = r'\('
t_RPAR = r'\)'
t_LCHAVES = r'\{'
t_RCHAVES = r'\}'
#t_DEF = r'def'
t_COMMA = r'\,'
t_ENDLINE = r'\;'

# Ignorando espacos em branco
t_ignore = r' '


# Definindo um numero 
def t_NUMBER(tokens):
    r'\d+\.?\d*([Ee][+-]?\d+)?'
    tokens.value = float(tokens.value)
    return tokens

def t_DEF(tokens):
    r'def'
    tokens.type = 'DEF'
    return tokens


# Definindo name para variaveis, exige uma letra e pode ser seguido por letras ou numeros 
def t_NAME(tokens):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    tokens.type = 'NAME'
    return tokens

def t_error(tokens):
    print("Não pode ser processado, verifique a entrada")
    tokens.lexer.skip(1)

lexer = lex.lex()

env = {}
contexto = ''

# Definindo as precedencias
precedence = (
    ('left','PLUS','MINUS'),
    ('left','DIVIDEINT','QUOTIENT'),
    ('left','MULTIPLY','DIVIDE'),
    ('left','EXPONENT'),
    ('left','LPAR','RPAR'),
    ('left','LCHAVES','RCHAVES')
)

def p_contexto(entrada):
    '''
    contexto    : deffuncao
                | funcao
                | expression
    '''
    print(run(entrada[1]))


def p_funcao(entrada):
    '''
    funcao  : NAME values
    '''
    contexto = entrada[1]
    print("deu certo")

def p_values(entrada):
    '''
    values  : NAME
            | NUMBER
    '''
    env['func'+contexto][2].add(entrada[1])


def p_deffuncao(entrada):
    '''
    deffuncao   : DEF NAME args bloco
    '''
    entrada[0] = ('def', entrada[2],entrada[3],entrada[4])


def p_args(entrada):
    '''
    args    : LPAR NAME RPAR
    '''
    entrada[0]= ('fc',entrada[2])

def p_args_largs(entrada):
    '''
    args    : largs
    '''
    entrada[0] = entrada[1]

def p_largs(entrada):
    '''
    largs   : args args
    '''
    entrada[0] = ('fcs',entrada[1],entrada[2])

def p_expression(entrada):
    '''
    expression  : term
                | var_assign
                | empty
    '''
    entrada[0] = entrada[1]
    

def p_bloco(entrada):
    '''
    bloco   : LCHAVES expression RCHAVES
    '''
    entrada[0] = entrada[2]


#def p_linha(entrada):
#    '''
#    linha   : expression ENDLINE
#    '''
#    entrada[0] = entrada[1]
 
# Definicao de como passar um valor para uma variavel 
def p_var_assign(entrada):
    '''
    var_assign  : NAME EQUALS term
    '''
    entrada[0] = ('=', entrada[1],entrada[3])

# Definicao de expressao exige que a maior hierarquia apareca primeiro
def p_term(entrada):
    '''
    term    : term EXPONENT term
            | term MULTIPLY term
            | term DIVIDE term
            | term DIVIDEINT term
            | term QUOTIENT term
            | term MINUS term
            | term PLUS term
    '''
    entrada[0] = (entrada[2], entrada[1], entrada[3])


def p_term_factor(entrada):
    '''
    term    : factor
    '''
    entrada[0] = entrada[1]


def p_factor(entrada):
    '''
    factor  : nterm
            | NUMBER
            | positive
            | negative
    '''
    entrada[0] = entrada[1]
    

def p_negative(entrada):
    '''
    negative    : MINUS term
    '''
    entrada[0] = - entrada[2]

def p_positive(entrada):
    '''
    positive    : PLUS term
    '''
    entrada[0] = entrada[2]


def p_nterm(entrada):
    '''
    nterm : LPAR term RPAR
    '''
    entrada[0] = entrada[2]

# Definicao de uma variavel 
def p_term_var(entrada):
    '''
    term    : NAME
    '''
    entrada[0] = ('var', entrada[1])

def p_error(entrada):
    print("Syntax error found!" + entrada.value)

def p_empty(entrada):
    '''
    empty   :
    '''
    entrada[0]= entrada

parser = yacc.yacc()
# env é um dicionário que contém as variáveis. Este dicionário é global.


# Executando a string de entrada
def run(entrada):
    global env
    global contexto
    if type(entrada) == tuple:
        if entrada[0] == '+':
            return run(entrada[1]) + run(entrada[2])
        elif entrada[0] == '-':
            return run(entrada[1]) - run(entrada[2])
        elif entrada[0] == '*':
            return run(entrada[1]) * run(entrada[2])
        elif entrada[0] == '/':
            return run(entrada[1]) / run(entrada[2])
        elif entrada[0] == '//':
            return run(entrada[1]) // run(entrada[2])
        elif entrada[0] == '%':
            return run(entrada[1]) % run(entrada[2])
        elif entrada[0] == '^':
            return run(entrada[1]) ** run(entrada[2])
        elif entrada[0] == '=':
            if (contexto == ''):
                env[entrada[1]] = run(entrada[2])
            else:
                env[contexto+entrada[1]] = run(entrada[2])
        elif entrada[0] == 'var':
            if entrada[1] in env:
                return env[entrada[1]]
            elif (contexto+entrada[1]) in env:
                return env[contexto+entrada[1]]
            else:
                return 'Undeclared variable found!'
        elif entrada[0] == 'def':
            print(entrada[1])
            env['func'+entrada[1]] = []
            env['func'+entrada[1]].append([])
            env['func'+entrada[1]][0].append(entrada[3])
            contexto = entrada[1]
            run(entrada[2])
            contexto=''
            print('declarando funcao')
        elif entrada[0] == 'fcs':
            run(entrada[1])
            run(entrada[2])
        elif entrada[0] == 'fc':
            env['func'+contexto][0].append(entrada[1])
        elif entrada[0] == 'func':
            contexto = entrada[1]
            #run(entrada[2])
            print('executando funcao')
    else:
        return entrada


def rodaFuncao ():
    print("roda funcao")
    return 0




while True:
    s = input('>> ')
    if s == 'exit':
        break
    try:
        lexer.input(s)
        while True:
            tok = lexer.token()
            if not tok:
                break           
    except EOFError:
        break
    parser.parse(s)
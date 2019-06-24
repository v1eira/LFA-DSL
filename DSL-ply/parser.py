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

# Definindo os tokens onde t = conjunto

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
def t_NUMBER(t):
    r'\d+\.?\d*([Ee][+-]?\d+)?'
    t.value = float(t.value)
    return t

def t_DEF(t):
    r'def'
    t.type = 'DEF'
    return t


# Definindo name para variaveis, exige uma letra e pode ser seguido por letras ou numeros 
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

def t_error(t):
    print("Não pode ser processado, verifique a entrada")
    t.lexer.skip(1)

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

def p_contexto(p):
    '''
    contexto    : deffuncao
                | funcao
                | expression
    '''
    print(run(p[1]))


def p_funcao(p):
    '''
    funcao  : NAME values
    '''
    contexto = p[1]
    print("deu certo")

def p_values(p):
    '''
    values  : NAME
            | NUMBER
    '''
    env['func'+contexto][2].add(p[1])


def p_deffuncao(p):
    '''
    deffuncao   : DEF NAME args bloco
    '''
    p[0] = ('def', p[2],p[3],p[4])


def p_args(p):
    '''
    args    : LPAR NAME RPAR
    '''
    p[0]= ('fc',p[2])

def p_args_largs(p):
    '''
    args    : largs
    '''
    p[0] = p[1]

def p_largs(p):
    '''
    largs   : args args
    '''
    p[0] = ('fcs',p[1],p[2])

def p_expression(p):
    '''
    expression  : term
                | var_assign
                | empty
    '''
    p[0] = p[1]
    

def p_bloco(p):
    '''
    bloco   : LCHAVES expression RCHAVES
    '''
    p[0] = p[2]


#def p_linha(p):
#    '''
#    linha   : expression ENDLINE
#    '''
#    p[0] = p[1]
 
# Definicao de como passar um valor para uma variavel 
def p_var_assign(p):
    '''
    var_assign  : NAME EQUALS term
    '''
    p[0] = ('=', p[1],p[3])

# Definicao de expressao exige que a maior hierarquia apareca primeiro
def p_term(p):
    '''
    term    : term EXPONENT term
            | term MULTIPLY term
            | term DIVIDE term
            | term DIVIDEINT term
            | term QUOTIENT term
            | term MINUS term
            | term PLUS term
    '''
    p[0] = (p[2], p[1], p[3])


def p_term_factor(p):
    '''
    term    : factor
    '''
    p[0] = p[1]


def p_factor(p):
    '''
    factor  : nterm
            | NUMBER
            | positive
            | negative
    '''
    p[0] = p[1]
    

def p_negative(p):
    '''
    negative    : MINUS term
    '''
    p[0] = - p[2]

def p_positive(p):
    '''
    positive    : PLUS term
    '''
    p[0] = p[2]


def p_nterm(p):
    '''
    nterm : LPAR term RPAR
    '''
    p[0] = p[2]

# Definicao de uma variavel 
def p_term_var(p):
    '''
    term    : NAME
    '''
    p[0] = ('var', p[1])

def p_error(p):
    print("Syntax error found!" + p.value)

def p_empty(p):
    '''
    empty   :
    '''
    p[0]= p

parser = yacc.yacc()
# env é um dicionário que contém as variáveis. Este dicionário é global.


# Executando a string de entrada
def run(p):
    global env
    global contexto
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        elif p[0] == '-':
            return run(p[1]) - run(p[2])
        elif p[0] == '*':
            return run(p[1]) * run(p[2])
        elif p[0] == '/':
            return run(p[1]) / run(p[2])
        elif p[0] == '//':
            return run(p[1]) // run(p[2])
        elif p[0] == '%':
            return run(p[1]) % run(p[2])
        elif p[0] == '^':
            return run(p[1]) ** run(p[2])
        elif p[0] == '=':
            if (contexto == ''):
                env[p[1]] = run(p[2])
            else:
                env[contexto+p[1]] = run(p[2])
        elif p[0] == 'var':
            if p[1] in env:
                return env[p[1]]
            elif (contexto+p[1]) in env:
                return env[contexto+p[1]]
            else:
                return 'Undeclared variable found!'
        elif p[0] == 'def':
            print(p[1])
            env['func'+p[1]] = []
            env['func'+p[1]].append([])
            env['func'+p[1]][0].append(p[3])
            contexto = p[1]
            run(p[2])
            contexto=''
            print('declarando funcao')
        elif p[0] == 'fcs':
            run(p[1])
            run(p[2])
        elif p[0] == 'fc':
            env['func'+contexto][0].append(p[1])
        elif p[0] == 'func':
            contexto = p[1]
            #run(p[2])
            print('executando funcao')
    else:
        return p


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
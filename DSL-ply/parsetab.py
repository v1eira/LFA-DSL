
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftPLUSMINUSleftDIVIDEINTQUOTIENTleftMULTIPLYDIVIDEleftEXPONENTleftLPARRPARleftLCHAVESRCHAVESCOMMA DEF DIVIDE DIVIDEINT ENDLINE EQUALS EXPONENT LCHAVES LPAR MINUS MULTIPLY NAME NUMBER PLUS QUOTIENT RCHAVES RPAR\n    contexto    : deffuncao\n                | funcao\n                | expression\n    \n    funcao  : NAME values\n    \n    values  : NAME\n            | NUMBER\n    \n    deffuncao   : DEF NAME args bloco\n    \n    args    : LPAR NAME RPAR\n    \n    args    : largs\n    \n    largs   : args args\n    \n    expression  : term\n                | var_assign\n                | empty\n    \n    bloco   : LCHAVES expression RCHAVES\n    \n    var_assign  : NAME EQUALS term\n    \n    term    : term EXPONENT term\n            | term MULTIPLY term\n            | term DIVIDE term\n            | term DIVIDEINT term\n            | term QUOTIENT term\n            | term MINUS term\n            | term PLUS term\n    \n    term    : factor\n    \n    factor  : nterm\n            | NUMBER\n            | positive\n            | negative\n    \n    negative    : MINUS term\n    \n    positive    : PLUS term\n    \n    nterm : LPAR term RPAR\n    \n    term    : NAME\n    \n    empty   :\n    '
    
_lr_action_items = {'DEF':([0,],[5,]),'NAME':([0,5,6,10,11,17,21,23,24,25,26,27,28,29,35,48,],[6,18,19,31,31,31,31,31,31,31,31,31,31,31,49,51,]),'$end':([0,1,2,3,4,6,7,8,9,12,13,14,15,16,19,20,22,30,31,32,37,38,39,40,41,42,43,44,45,47,53,],[-32,0,-1,-2,-3,-31,-11,-12,-13,-23,-24,-25,-26,-27,-5,-4,-6,-28,-31,-29,-15,-16,-17,-18,-19,-20,-21,-22,-30,-7,-14,]),'NUMBER':([0,6,10,11,17,21,23,24,25,26,27,28,29,48,],[14,22,14,14,14,14,14,14,14,14,14,14,14,14,]),'LPAR':([0,10,11,17,18,21,23,24,25,26,27,28,29,34,36,46,48,52,],[17,17,17,17,35,17,17,17,17,17,17,17,17,35,-9,35,17,-8,]),'PLUS':([0,6,7,10,11,12,13,14,15,16,17,21,23,24,25,26,27,28,29,30,31,32,33,37,38,39,40,41,42,43,44,45,48,51,],[11,-31,29,11,11,-23,-24,-25,-26,-27,11,11,11,11,11,11,11,11,11,-28,-31,-29,29,29,-16,-17,-18,-19,-20,-21,-22,-30,11,-31,]),'MINUS':([0,6,7,10,11,12,13,14,15,16,17,21,23,24,25,26,27,28,29,30,31,32,33,37,38,39,40,41,42,43,44,45,48,51,],[10,-31,28,10,10,-23,-24,-25,-26,-27,10,10,10,10,10,10,10,10,10,-28,-31,-29,28,28,-16,-17,-18,-19,-20,-21,-22,-30,10,-31,]),'EXPONENT':([6,7,12,13,14,15,16,30,31,32,33,37,38,39,40,41,42,43,44,45,51,],[-31,23,-23,-24,-25,-26,-27,23,-31,23,23,23,-16,23,23,23,23,23,23,-30,-31,]),'MULTIPLY':([6,7,12,13,14,15,16,30,31,32,33,37,38,39,40,41,42,43,44,45,51,],[-31,24,-23,-24,-25,-26,-27,24,-31,24,24,24,-16,-17,-18,24,24,24,24,-30,-31,]),'DIVIDE':([6,7,12,13,14,15,16,30,31,32,33,37,38,39,40,41,42,43,44,45,51,],[-31,25,-23,-24,-25,-26,-27,25,-31,25,25,25,-16,-17,-18,25,25,25,25,-30,-31,]),'DIVIDEINT':([6,7,12,13,14,15,16,30,31,32,33,37,38,39,40,41,42,43,44,45,51,],[-31,26,-23,-24,-25,-26,-27,26,-31,26,26,26,-16,-17,-18,-19,-20,26,26,-30,-31,]),'QUOTIENT':([6,7,12,13,14,15,16,30,31,32,33,37,38,39,40,41,42,43,44,45,51,],[-31,27,-23,-24,-25,-26,-27,27,-31,27,27,27,-16,-17,-18,-19,-20,27,27,-30,-31,]),'EQUALS':([6,51,],[21,21,]),'RCHAVES':([7,8,9,12,13,14,15,16,30,31,32,37,38,39,40,41,42,43,44,45,48,50,51,],[-11,-12,-13,-23,-24,-25,-26,-27,-28,-31,-29,-15,-16,-17,-18,-19,-20,-21,-22,-30,-32,53,-31,]),'RPAR':([12,13,14,15,16,30,31,32,33,38,39,40,41,42,43,44,45,49,],[-23,-24,-25,-26,-27,-28,-31,-29,45,-16,-17,-18,-19,-20,-21,-22,-30,52,]),'LCHAVES':([34,36,46,52,],[48,-9,-10,-8,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'contexto':([0,],[1,]),'deffuncao':([0,],[2,]),'funcao':([0,],[3,]),'expression':([0,48,],[4,50,]),'term':([0,10,11,17,21,23,24,25,26,27,28,29,48,],[7,30,32,33,37,38,39,40,41,42,43,44,7,]),'var_assign':([0,48,],[8,8,]),'empty':([0,48,],[9,9,]),'factor':([0,10,11,17,21,23,24,25,26,27,28,29,48,],[12,12,12,12,12,12,12,12,12,12,12,12,12,]),'nterm':([0,10,11,17,21,23,24,25,26,27,28,29,48,],[13,13,13,13,13,13,13,13,13,13,13,13,13,]),'positive':([0,10,11,17,21,23,24,25,26,27,28,29,48,],[15,15,15,15,15,15,15,15,15,15,15,15,15,]),'negative':([0,10,11,17,21,23,24,25,26,27,28,29,48,],[16,16,16,16,16,16,16,16,16,16,16,16,16,]),'values':([6,],[20,]),'args':([18,34,46,],[34,46,46,]),'largs':([18,34,46,],[36,36,36,]),'bloco':([34,],[47,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> contexto","S'",1,None,None,None),
  ('contexto -> deffuncao','contexto',1,'p_contexto','parser.py',93),
  ('contexto -> funcao','contexto',1,'p_contexto','parser.py',94),
  ('contexto -> expression','contexto',1,'p_contexto','parser.py',95),
  ('funcao -> NAME values','funcao',2,'p_funcao','parser.py',102),
  ('values -> NAME','values',1,'p_values','parser.py',108),
  ('values -> NUMBER','values',1,'p_values','parser.py',109),
  ('deffuncao -> DEF NAME args bloco','deffuncao',4,'p_deffuncao','parser.py',116),
  ('args -> LPAR NAME RPAR','args',3,'p_args','parser.py',123),
  ('args -> largs','args',1,'p_args_largs','parser.py',129),
  ('largs -> args args','largs',2,'p_largs','parser.py',135),
  ('expression -> term','expression',1,'p_expression','parser.py',141),
  ('expression -> var_assign','expression',1,'p_expression','parser.py',142),
  ('expression -> empty','expression',1,'p_expression','parser.py',143),
  ('bloco -> LCHAVES expression RCHAVES','bloco',3,'p_bloco','parser.py',150),
  ('var_assign -> NAME EQUALS term','var_assign',3,'p_var_assign','parser.py',164),
  ('term -> term EXPONENT term','term',3,'p_term','parser.py',171),
  ('term -> term MULTIPLY term','term',3,'p_term','parser.py',172),
  ('term -> term DIVIDE term','term',3,'p_term','parser.py',173),
  ('term -> term DIVIDEINT term','term',3,'p_term','parser.py',174),
  ('term -> term QUOTIENT term','term',3,'p_term','parser.py',175),
  ('term -> term MINUS term','term',3,'p_term','parser.py',176),
  ('term -> term PLUS term','term',3,'p_term','parser.py',177),
  ('term -> factor','term',1,'p_term_factor','parser.py',184),
  ('factor -> nterm','factor',1,'p_factor','parser.py',191),
  ('factor -> NUMBER','factor',1,'p_factor','parser.py',192),
  ('factor -> positive','factor',1,'p_factor','parser.py',193),
  ('factor -> negative','factor',1,'p_factor','parser.py',194),
  ('negative -> MINUS term','negative',2,'p_negative','parser.py',201),
  ('positive -> PLUS term','positive',2,'p_positive','parser.py',207),
  ('nterm -> LPAR term RPAR','nterm',3,'p_nterm','parser.py',214),
  ('term -> NAME','term',1,'p_term_var','parser.py',221),
  ('empty -> <empty>','empty',0,'p_empty','parser.py',230),
]
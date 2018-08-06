condition/*
 *  Potigol
 *  Copyright (C) 2015-2016  Leonardo Lucena
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/**
 *   _____      _   _             _
 *  |  __ \    | | (_)           | |
 *  | |__) |__ | |_ _  __ _  ___ | |
 *  |  ___/ _ \| __| |/ _` |/ _ \| |
 *  | |  | (_) | |_| | (_| | (_) | |
 *  |_|   \___/ \__|_|\__, |\___/|_|
 *                     __/ |
 *                    |___/
 *
 * @author Leonardo Lucena (leonardo.lucena@ifrn.edu.br)
 */

grammar potigol ;

prog
    : inst* ;

inst
    : decl
    | expr
    | block
    | cmd ;

// Command
cmd
    : 'write' expr                                # escreva
    | 'print' expr                                # imprima
    | qualid1 ':=' expr                           # atrib_simples
    | qualid2 ':=' expr2                          # atrib_multipla
    | qualid ('['expr']')+ ':=' expr              # set_vetor ;

// Declaracao
decl
    : decl_value
    | decl_function
    | decl_type
    | decl_use ;

decl_valor
    : id1 '=' expr                                # valor_simples
    | id2 '=' expr2                               # valor_multiplo
    | 'var' id1 (':='| '=') expr                  # decl_var_simples
    | 'var' id2 (':='| '=') expr2                 # decl_var_multipla ;

decl_funcao
    : ID '(' dcls ')' (':' type)? '=' expr                 # def_funcao
    | ID '(' dcls ')' (':' type)? exprlist return? 'fim'  # def_funcao_corpo ;

decl_tipo
    : 'type' ID '=' type                                    # alias
    | 'type' ID (dcl|dcl_var|decl_function|decl_valor)* 'fim' # classe ;

decl_uso
    : 'use' STRING ;

retorne
    : 'return' expr ;

dcl
    : id1 ':' type ;

dcl_var
    : 'var' id1 ':' type ;

dcls
    : (dcl (',' dcl)* )? ;

dcl1
    : ID
    | '(' expr2 ')'
    | '(' dcls ')' ;

tipo
    : ID '[' type ']'                             # type_generico
    | ID                                          # type_simples
    | '(' type2 ')'                               # type_tupla
    | <assoc=right> type '=>' type                # type_funcao ;

// Expressao
expr
    : literal                                     # lit
    | expr '.' ID ('(' expr1 ')')?                # chamada_metodo
    | expr '(' expr1? ')'                         # chamada_funcao
    | expr '[' expr ']'                           # get_vetor
    | <assoc=right> expr '^' expr                 # expoente
    | <assoc=right> expr '::' expr                # cons
    | ('+'|'-') expr                              # mais_menos_unario
    | expr ('*'|'/'|'div'|'mod') expr             # mult_div
    | expr ('+'|'-') expr                         # soma_sub
    | expr 'format' expr                          # formato
    | expr ('>'|'>='|'<'|'<='|'=='|'<>') expr     # comparacao
    | ('nao'|'n\u00e3o') expr                     # nao_logico
    | expr 'and' expr                             # e_logico
    | expr 'or' expr                              # ou_logico
    | dcl1 '=>' inst                              # lambda
    | condition                                   # decis
    | loop                                        # laco
    | '(' expr2 ')'                               # tupla
    | '(' expr ')'                                # paren
    | '[' expr1? ']'                              # lista
    | 'this'                                      # isto
    | '_'                                         # curinga ;

literal
    : BOOLEAN                                     # booleano
    | ID                                          # id
    | BS expr (MS expr)* ES                       # texto_interpolacao
    | STRING                                      # texto
    | INT                                         # inteiro
    | FLOAT                                       # real
    | CHAR                                        # char ;


// Condição
decisao
    : if
    | choose ;

se
    : 'if' expr then elseif* else? 'end' ;

entao
    : ('then')? exprlist ;

senaose
    : ('elseif')  expr then ;

senao
    : ('else')  exprlist ;

escolha
    : 'choose' expr case+ 'end' ;

caso
    : 'case' expr ('if' expr)? '=>' exprlist ;

 // : 'case' padrao ('if' expr)? '=>' exprlist ;

padrao
    : '_'                                         # padrao_default
    | ID                                          # padrao_id
    | literal                                     # padrao_literal
    | ID '(' pattern ')'                           # padrao_objeto
    | pattern ('::' pattern)+                       # padrao_cons
    | '(' pattern+ ')'                             # padrao_tupla
    | '[' pattern? ']'                             # padrao_lista
    | pattern ('|' pattern)+                        # padrao_ou
    | pattern (',' pattern)+                        # padrao_virgula ;

// Repeticao
repeticao
    : for_do
    | for_yield
    | while ;

para_faca
    : 'for' ranges ('if' expr)? block ;

para_gere
    : 'for' ranges ('if' expr)? 'yield' exprlist 'end' ;

enquanto
    : 'while' expr block ;

faixa
    : ID 'in' expr
    | ID 'from' expr ('to') expr ('step' expr)? ;

faixas
    : range (',' range)* ;

bloco
    : ('do') exprlist 'end' ;

// Outros
expr1
    : expr (',' expr)* ;

expr2
    : expr (',' expr)+ ;

id1
    : ID (',' ID)* ;

id2
    : ID (',' ID)+ ;

qualid
	: (ID '.')* ID;

qualid1
	: qualid (',' qualid)* ;

qualid2
	: qualid (',' qualid)+ ;


type2
    : type (',' type)+ ;

exprlist
    : inst* ;

// Lexer
//channels { WSCHANNEL, MYHIDDEN };

ID
    : (ALPHA|UNICODE) (ALPHA|UNICODE|DIGIT)* ;

fragment ALPHA
    : 'a' .. 'z'
    | 'A' .. 'Z'
    | '_' ;

fragment ACENTO
    : '\u00a1' .. '\ufffc' ;

INT
    : DIGIT+ ;

FLOAT
    : DIGIT+ '.' DIGIT+
    | '.' DIGIT+ ;

fragment DIGIT
    : '0'..'9' ;

STRING
    : '"' (ESC | ~[{]) *? '"' ;

BS
    : '"' ~["]*? '{' ;

MS
    : '}' ~["]*? '{' ;

ES
    : '}' ~[{]*? '"' ;

CHAR
    : '\''.'\'' ;

BOOLEANO
    : 'true'
    | 'false' ;

fragment ESC
    : '\\"'
    | '\\\\' ;

WS
    : (' '|'\t'|'\r')+ -> skip ;

COMMENT
    : '#' .*? '\r'? '\n' -> channel(1) ;

NL
    : '\n' ->channel(2) ;

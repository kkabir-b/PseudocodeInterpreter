global error_det
error_det = ''
 
from strings_with_arrows import * #Huge thanks to CodePulse for this library
import string
#Constant
#-----------------------------
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS

#ERRORS
#-----------------------------

class Error:
    def __init__(self, pos_start,pos_end,error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        global error_det
        result = f'{self.error_name}:{self.details}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        return result


class IllegalCharError(Error):
    def __init__(self,pos_start,pos_end,details):
        super().__init__(pos_start,pos_end,'Illegal Character',details)



class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details = ''):
        super().__init__(pos_start, pos_end, 'Invalid syntax', details)

    def as_string(self):
        global error_det
        result = f'{self.error_name}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        return result
class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start,pos_end,'Expected character',details)
class RTError(Error):
    def __init__(self, pos_start, pos_end, details,context):
        super().__init__(pos_start, pos_end, 'Runtime error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}:{self.details}\n'
        result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end) + '\n'
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result = f'   File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result

#Position
#-------------------------------------------


class Position:
    def __init__(self,idx,ln,col,fn,ftxt):
        self.fn = fn
        self.ftxt = ftxt
        self.idx = idx
        self.ln = ln
        self.col = col

    def advance(self,current_char = None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0

        return self
    def regress(self,current_char = None):
        self.idx -= 1
        self.col -= 1

    def copy(self):
        return Position(self.idx,self.ln,self.col,self.fn,self.ftxt)

#TOKENS
#-----------------------------
TT_INT = 'Int'
TT_REAL = 'Real'
TT_STRING = 'String'
TT_IDENTIFIER = 'Identifier'
TT_KEYWORD = 'Keyword'
TT_EQ = 'Eq'
TT_PLUS = 'Plus'
TT_MINUS = 'Minus'
TT_MUL = 'Mul'
TT_DIV = 'Div'
TT_POWER = 'Power'
TT_LPAREN = 'LParen'
TT_RPAREN = "RParen"
TT_LSQUAREBRACKET = 'LSquareBracket'
TT_RSQUAREBRACKET = 'RSquareBracket'
TT_LT = 'NE'
TT_EE = 'EE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_NE = 'NE'
TT_Comma = 'COMMA'
TT_Arrow = "ARROW"
TT_Newline = 'Newline'
TT_EOF = "Eof"

KEYWORDS = [
'AND',"OR","NOT","IF","ELSE","THEN","FOR","WHILE","DO","STEP","TO","REPEAT","UNTIL","FUNCTION",'RETURNS','ENDIF','ENDFUNCTION','RETURNS','ENDWHILE',"NEXT",'RETURN','PROCEDURE','ENDPROCEDURE',"CASE","OF",'ENDCASE','CALL'
]

class Token:
    def __init__(self,type_,value = None, pos_start = None, pos_end = None):
        self.type = type_
        self.value = value
        if pos_start:
            self.pos_start = pos_start
            self.pos_end = self.pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

    def matches(self,type_,value):
        return self.type == type_ and self.value == value

#LEXER
#-----------------------------


class Lexer:
    def __init__(self,fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1,0,-1,fn,text)
        self.current_char = None
        self.advance()
    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
    def regress(self):
        self.pos.regress(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
    def make_tokens(self):
        tokens = []

        while self.current_char != None:

            if self.current_char in ' \t':
                self.advance()

            elif self.current_char in ['\n',';']:
                tokens.append(Token(TT_Newline,pos_start=self.pos))
                self.advance()

            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
                if tokens[-1].matches(TT_KEYWORD,'CALL'):tokens.pop()

            elif self.current_char in DIGITS:
                tokens.append(self.make_number())


            elif self.current_char in ['"',"'"]:
                tokens.append(self.make_string(self.current_char))

            elif self.current_char == '[':
                tokens.append(Token(TT_LSQUAREBRACKET,pos_start = self.pos))
                self.advance()

            elif self.current_char == ']':
                tokens.append(Token(TT_RSQUAREBRACKET,pos_start = self.pos))
                self.advance()

            elif self.current_char == '+' or self.current_char == '&':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()

            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == ':':
                tokens.append(Token(TT_Arrow,pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()

            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(Token(TT_POWER,pos_start = self.pos))
                self.advance()
            elif self.current_char == '<':
                tok = self.make_not_lessThan_assign()
                tokens.append(tok)
            elif self.current_char == '=':
                tokens.append(Token(TT_EE,pos_start=self.pos))
                self.advance()

            elif self.current_char == '>':
                tokens.append(self.make_greater_than())

            elif self.current_char == ',':
                tokens.append(Token(TT_Comma, pos_start=self.pos))
                self.advance()
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [],IllegalCharError(pos_start,self.pos,"'" + char + "'")

        tokens.append(Token(TT_EOF, pos_start=self.pos))

        return tokens, None

    def make_string(self,end):
        string = ''
        pos_start = self.pos.copy()
        self.advance()
        while self.current_char != None and self.current_char != end:
            string += self.current_char
            self.advance()

        self.advance()
        return Token(TT_STRING,string,pos_start,self.pos)


    def make_number(self):
        num_str = ''
        dotCount = 0
        pos_start = self.pos.copy()
        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dotCount == 1: break
                num_str += '.'
                dotCount += 1
            else:
                num_str += self.current_char
            self.advance()
        if dotCount == 0:
            return Token(TT_INT,int(num_str),pos_start,self.pos)
        else:
            return Token(TT_REAL,float(num_str),pos_start,self.pos)

    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()
        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER

        return Token(tok_type,id_str,pos_start,self.pos)

    def make_greater_than(self):
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            return Token(TT_GTE,pos_start=pos_start,pos_end=self.pos)
        return Token(TT_GT,pos_start=pos_start,pos_end=self.pos)

    def make_not_lessThan_assign(self):
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '-':
            self.advance()
            return Token(TT_EQ,pos_start=pos_start,pos_end=self.pos)
        elif self.current_char == '>':
            self.advance()
            return Token(TT_NE,pos_start=pos_start,pos_end=self.pos)
        elif self.current_char == '=':
            self.advance()
            return Token(TT_LTE, pos_start=pos_start, pos_end=self.pos)
        else:
            return Token(TT_LT, pos_start=pos_start, pos_end=self.pos)

#nodes
#--------------------------------------------------

class NumberNode:
    def __init__(self,tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class StringNode:
    def __init__(self,tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class ListNode:
    def __init__(self,element_nodes,pos_start,pos_end):
        self.element_nodes = element_nodes
        self.pos_start = pos_start
        self.pos_end = pos_end


class VarAccessNode:
    def __init__(self,var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = var_name_tok.pos_start
        self.pos_end = var_name_tok.pos_end


class VarAssignNode:
    def __init__(self,var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node

        self.pos_start = var_name_tok.pos_start
        self.pos_end = var_name_tok.pos_end


class BinOpNode:
    def __init__(self,left_node,op_tok,right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node},{self.op_tok},{self.right_node})'


class UnaryOpNode:
    def __init__(self,op_tok,node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = self.op_tok.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'


class IfNode:
    def __init__(self,cases,else_case):
        self.cases = cases
        self.else_case = else_case

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[-1][0]).pos_end
class CaseOfNode:
    def __init__(self,orignalNode,cases):
        self.cases = cases
        self.orignalNode = orignalNode

        self.pos_start = orignalNode.pos_start
        self.pos_end = cases[-1][0].pos_end
class ForNode:
    def __init__(self,var_name_tok,start_value_node,end_value_node,step_value_node,body_ode):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_ode

        self.pos_start=self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end

class WhileNode:
    def __init__(self,condition_node,body_node):
        self.condition_node = condition_node
        self.body_node = body_node

        self.pos_start = self.condition_node.pos_start
        self.pos_end = self.body_node.pos_end
class RepeatNode:
    def __init__(self,body_node,condition_node):
        self.condition_node = condition_node
        self.body_node = body_node

        self.pos_start = self.body_node.pos_start
        self.pos_end = self.condition_node.pos_end

class FuncDefNode:
    def __init__(self,var_name_tok,arg_name_toks,body_nodes,ret_node):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_nodes
        self.pos_start = self.var_name_tok.pos_start
        self.ret_node = ret_node
        self.pos_end = self.body_node.pos_end
class ProcedureDefNode:
    def __init__(self,var_name_tok,arg_name_toks,body_nodes):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_nodes
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end

class CallNode:
    def __init__(self,node_to_call,arg_node):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_node

        self.pos_start = self.node_to_call.pos_start
        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[-1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end


#Parser result
#---------------------------------------

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
        self.to_reverse_count = 0


    def register(self,res):
        if isinstance(res,ParseResult):
            if res.error: self.error = res.error
            self.advance_count += res.advance_count
            return res.node
        self.advance_count += 1
        return res

    def success(self,node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self

    def try_register(self,res):
        if res.error:
            self.to_reverse_count = res.advance_count
            res.error = None
            return None
        return res.register(res)


#Parser
#---------------------------------------

class Parser:
    def __init__(self,tokens):
        self.tokens = tokens
        self.hasEq = 0
        for q in tokens:
            if q.type == TT_EQ:
                self.hasEq += 1
        self.tok_idx = -1
        self.advance()



    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]

        return self.current_tok

    def regress(self):
        self.tok_idx -= 1
        self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok

    def reverse(self,amount = 1):
        self.tok_idx -= amount
        self.update_current_tok()
        return self.current_tok

    def update_current_tok(self):
        if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]

    def parse(self):
        res = self.statements()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '+', '-', '*' or '/'"
            ))
        return res



    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_tok.pos_start.copy()
        n = 0
        while self.current_tok.type == TT_Newline:
            res.register(self.advance())

        statement = res.register(self.expr())
        if res.error: return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_tok.type == TT_Newline:
                res.register(self.advance())
                newline_count +=1
            if newline_count == 0: more_statements = False

            if not more_statements: break
            statement = res.try_register(self.expr())
            if statement == None:
                self.reverse(1)
                more_statements = False
                continue
            statements.append(statement)

        return res.success(ListNode(
            statements,pos_start,self.current_tok.pos_end.copy()
        ))

    def for_expr(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD,'FOR'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end,f"Expected 'FOR'"))
        res.register(self.advance())
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected identifier"))
        var_name = self.current_tok
        res.register(self.advance())
        if self.current_tok.type != TT_EQ:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, "Expected '<-'"))
        res.register(self.advance())
        start_value = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD,'TO'):

            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'TO'"))
        res.register(self.advance())
        end_value = res.register(self.expr())
        if res.error: return res

        if self.current_tok.matches(TT_KEYWORD,'STEP'):
            res.register(self.advance())
            step_value = res.register(self.expr())
            if res.error:return res
        else:
            step_value = None
        if not self.current_tok.type == TT_Newline:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected Newline"))
        res.register(self.advance())
        body = res.register(self.statements())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD,'NEXT'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'NEXT'"))
        res.register(self.advance())

        if not self.current_tok.type == TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected Identifier"))
        res.register(self.advance())

        if not self.current_tok.type in (TT_Newline,TT_EOF):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected EOF or Newline"))


        return res.success(ForNode(var_name,start_value,end_value,step_value,body))

    def while_expr(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD,"WHILE"):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'WHILE'"))
        res.register(self.advance())

        condition = res.register(self.expr())
        if res.error: return res
        if self.current_tok.type != TT_Newline:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                           self.current_tok.pos_end, f"Expected Newline"))
        res.register(self.advance())

        if not self.current_tok.matches(TT_KEYWORD,'DO'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'DO'"))
        res.register(self.advance())

        if self.current_tok.type != TT_Newline:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                           self.current_tok.pos_end, f"Expected Newline"))
        res.register(self.advance())
        body = res.register(self.statements())
        if res.error: return res
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD,'ENDWHILE'):
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                      self.current_tok.pos_end, f"Expected 'ENDWHILE'"))
        res.register(self.advance())
        if not self.current_tok.type in (TT_Newline,TT_EOF):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected Newline or EOF"))
        return res.success(WhileNode(condition,body))

    def repeat_expr(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD,"REPEAT"):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'REPEAT'"))
        res.register(self.advance())
        if not self.current_tok.type == TT_Newline:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected Newline"))
        body = res.register(self.statements())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD,'UNTIL'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'UNTIL'"))
        res.register(self.advance())
        condition = res.register(self.expr())

        if not self.current_tok.type in (TT_Newline,TT_EOF):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected newline or EOF"))
        return res.success(RepeatNode(body,condition))

    def case_expr(self):
        res = ParseResult()
        cases = []
        if not self.current_tok.matches(TT_KEYWORD,'CASE'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'CASE'"))

        res.register(self.advance())

        if not self.current_tok.matches(TT_KEYWORD,'OF'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'OF'"))
        res.register(self.advance())
        orignalNode = res.register(self.expr())
        if res.error: return res

        while not self.current_tok.matches(TT_KEYWORD,'ENDCASE'):

            if not self.current_tok.type == TT_Newline:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                      self.current_tok.pos_end, f"Expected newline"))
            res.register(self.advance())

            value = res.register(self.expr())
            if res.error:return res

            if not self.current_tok.type == TT_Arrow:
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                          self.current_tok.pos_end, f"Expected ':'"))

            res.register(self.advance())

            toDo = res.register(self.statements())
            if res.error:return res

            if self.current_tok.type == TT_Arrow:
                self.regress()
                self.regress()
            cases.append([value,toDo])

        res.register(self.advance())
        if not self.current_tok.type in (TT_EOF,TT_Newline):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected EOF or Newline"))
        return res.success(CaseOfNode(orignalNode,cases))

    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None
        if not self.current_tok.matches(TT_KEYWORD,'IF'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'IF'"))

        res.register(self.advance())
        cond = res.register(self.expr())
        if res.error: return res

        if self.current_tok.type != TT_Newline:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'Newline'"))
        res.register(self.advance())

        if not self.current_tok.matches(TT_KEYWORD,'THEN'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'THEN'"))

        res.register(self.advance())
        if self.current_tok.type != TT_Newline:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'Newline'"))
        res.register(self.advance())

        statements = res.register(self.statements())
        if res.error: return res
        cases.append((cond,statements))

        if self.current_tok.matches(TT_KEYWORD,'ELSE'): #instead of making a separate else-expr I added it here
            res.register(self.advance())
            if not self.current_tok.type == TT_Newline:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                      self.current_tok.pos_end, f"Expected 'Newline'"))
            else_case = res.register(self.statements())
            if res.error: return res


        if not self.current_tok.matches(TT_KEYWORD,'ENDIF'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected 'ENDIF'"))

        res.register(self.advance())
        if not self.current_tok.type in (TT_Newline,TT_EOF):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end, f"Expected EOF or Newline"))

        return res.success(IfNode(cases,else_case))



    def call(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error:
            return res

        if self.current_tok.type == TT_LPAREN:
            res.register(self.advance())
            arg_nodes = []

            if self.current_tok.type  == TT_RPAREN:
                res.register(self.advance())

            else:
                tok = self.current_tok
                arg_nodes.append(res.register(self.expr()))
                if res.error: res.failure(InvalidSyntaxError(tok.pos_start,tok.pos_end,""))

                while self.current_tok.type == TT_Comma:
                 res.register(self.advance())
                 arg_nodes.append(res.register(self.expr()))
                 if res.error: return res

                if self.current_tok.type != TT_RPAREN:
                    res.failure(
                        InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ')' or ','"))
                res.register(self.advance())
            return res.success(CallNode(atom,arg_nodes))
        return res.success(atom)

    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in [TT_INT,TT_REAL]:
            res.register(self.advance())
            return res.success(NumberNode(tok))
        if tok.type == TT_STRING:
            res.register(self.advance())
            return res.success(StringNode(tok))

        elif tok.type == TT_IDENTIFIER:
            res.register(self.advance())
            return res.success(VarAccessNode(tok))
        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                ))
        elif tok.type == TT_LSQUAREBRACKET:
            list_expr = res.register(self.list_expr())
            if res.error:
                return res
            return res.success(list_expr)

        elif tok.matches(TT_KEYWORD,'IF'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)

        elif tok.matches(TT_KEYWORD,'FOR'):
            for_expr = res.register(self.for_expr())
            if res.error:
                return res
            return res.success(for_expr)

        elif tok.matches(TT_KEYWORD,'WHILE'):
            while_expr = res.register(self.while_expr())
            if res.error:return res
            return res.success(while_expr)
        elif tok.matches(TT_KEYWORD,"REPEAT"):
            repeat_expr = res.register(self.repeat_expr())
            if res.error:
                return res
            return res.success(repeat_expr)

        elif tok.matches(TT_KEYWORD,"FUNCTION"):
            function_def = res.register(self.func_def())
            if res.error:
                return res
            return res.success(function_def)

        elif tok.matches(TT_KEYWORD,'PROCEDURE'):
            procedure_def = res.register(self.pocedure_def())
            if res.error: return res
            return res.success(procedure_def)

        elif tok.matches(TT_KEYWORD,'CASE'):
            case_expr = res.register(self.case_expr())
            if res.error: return res
            return res.success(case_expr)

        else:
            return res.failure(InvalidSyntaxError(tok.pos_start,tok.pos_end,"Expected int,real, '+', '-','(','IF','FOR','WHILE','REPEAT', or 'FUNCTION'"))

    def power(self):
        return self.bin_op(self.call,(TT_POWER,),self.factor)

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS,TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error:
                return res
            return res.success(UnaryOpNode(tok,factor))

        return self.power()


    def term(self):
        return self.bin_op(self.factor,(TT_DIV,TT_MUL))
    def arith_expr(self):
        return self.bin_op(self.term,(TT_PLUS,TT_MINUS))
    def comp_expr(self):
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD,'NOT'):
            op_tok = self.current_tok
            res.register(self.advance())
            if res.error: return res
            node = res.register(self.comp_expr())
            if res.error:return res
            return res.success(UnaryOpNode(op_tok,node))
        node = res.register(self.bin_op(self.arith_expr,(TT_EE,TT_NE,TT_LT,TT_LTE,TT_GT,TT_GTE)))
        tok = self.current_tok
        if res.error:
            return res
        return res.success(node)

    def expr(self):
        res = ParseResult()
        if self.current_tok.type == TT_IDENTIFIER:
            var_name = self.current_tok
            res.register(self.current_tok)
            res.register(self.advance())
            if self.current_tok.type == TT_EQ:
                res.register(self.advance())
                expr = res.register(self.expr())
                if res.error: return res
                return res.success(VarAssignNode(var_name,expr))
            self.regress()

        return self.bin_op(self.comp_expr,((TT_KEYWORD,'AND'),(TT_KEYWORD,'OR')))

    def bin_op(self,func_a,ops,func_b = None):
        if func_b == None:
            func_b = func_a
        res = ParseResult()
        left = res.register(func_a())

        while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register(self.advance())
            right = res.register(func_b())
            if res.error:
                return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)

    def list_expr(self):
        res = ParseResult()
        element_nodes = []
        pos_start = self.current_tok.pos_start.copy()

        res.register(self.advance())
        if self.current_tok.type == TT_RSQUAREBRACKET:
            res.register(self.advance())
        else:
            tok = self.current_tok
            element_nodes.append(res.register(self.expr()))
            if res.error: res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ']', 'IF','FOR','WHILE','FUNCTION',variable,datatypoe"))

            while self.current_tok.type == TT_Comma:
                res.register(self.advance())
                element_nodes.append(res.register(self.expr()))
                if res.error: return res

            if self.current_tok.type != TT_RSQUAREBRACKET:
                res.failure(
                    InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ']' or ','"))
            res.register(self.advance())
        return ListNode(element_nodes,pos_start,self.current_tok.pos_end.copy())

    def pocedure_def(self):
        res = ParseResult()
        tok = self.current_tok
        if not self.current_tok.matches(TT_KEYWORD,'PROCEDURE'):
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'PROCEDURE'"))
        res.register(self.advance())

        if self.current_tok.type != TT_IDENTIFIER:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected indentifier"))
        var_name_tok = self.current_tok
        res.register(self.advance())

        if self.current_tok.type != TT_LPAREN:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected '('"))

        res.register(self.advance())
        arg_name_toks = []
        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register(self.advance())
            if not self.current_tok.type == TT_Arrow:
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ':'"))
            res.register(self.advance())
            if not self.current_tok.type == TT_IDENTIFIER or self.current_tok.value not in ['REAL', 'INTEGER', 'CHAR',
                                                                                            'BOOLEAN', 'STRING']:
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected datatype"))
            res.register(self.advance())
            while self.current_tok.type == TT_Comma:
                res.register(self.advance())

                if self.current_tok.type != TT_IDENTIFIER:
                    tok = self.current_tok
                    return res.failure(
                        InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected identifier"))
                name = self.current_tok
                res.register(self.advance())
                if not self.current_tok.type == TT_Arrow:
                    return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ':'"))
                res.register(self.advance())
                if not self.current_tok.type == TT_IDENTIFIER or self.current_tok.value not in ['REAL', 'INTEGER',
                                                                                                'CHAR', 'BOOLEAN',
                                                                                                'STRING']:
                    return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected datatype"))
                arg_name_toks.append(name)
                res.register(self.advance())

            if self.current_tok.type != TT_RPAREN:
                tok = self.current_tok
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ',' or ')'"))

        else:
            if self.current_tok.type != TT_RPAREN:
                tok = self.current_tok
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected identifier or ')'"))
        res.register(self.advance())

        if not self.current_tok.type == TT_Newline:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected Newline'"))

        node_to_return = res.register(self.statements())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD, 'ENDPROCEDURE'):
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'ENDPROCEDURE'"))
        res.register(self.advance())
        if not self.current_tok.type in (TT_Newline, TT_EOF):
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected Newline or EOF'"))
        return res.success(ProcedureDefNode(var_name_tok,arg_name_toks,node_to_return))

    def func_def(self):
        res = ParseResult()
        tok = self.current_tok
        if not self.current_tok.matches(TT_KEYWORD,'FUNCTION'):
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'FUNCTION'"))

        res.register(self.advance())
        if self.current_tok.type != TT_IDENTIFIER:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected indentifier"))
        var_name_tok = self.current_tok
        res.register(self.advance())

        if self.current_tok.type != TT_LPAREN:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected '('"))

        res.register(self.advance())
        arg_name_toks = []
        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register(self.advance())
            if not self.current_tok.type == TT_Arrow:
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ':'"))
            res.register(self.advance())
            if not self.current_tok.type == TT_IDENTIFIER or self.current_tok.value not in ['REAL', 'INTEGER', 'CHAR','BOOLEAN', 'STRING']:
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected datatype"))
            res.register(self.advance())
            while self.current_tok.type == TT_Comma:
                res.register(self.advance())

                if self.current_tok.type != TT_IDENTIFIER:
                    tok = self.current_tok
                    return res.failure(
                        InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected identifier"))
                name = self.current_tok
                res.register(self.advance())
                if not self.current_tok.type == TT_Arrow:
                    return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ':'"))
                res.register(self.advance())
                if not self.current_tok.type == TT_IDENTIFIER or self.current_tok.value not in ['REAL', 'INTEGER', 'CHAR', 'BOOLEAN', 'STRING']:
                    return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected datatype"))
                arg_name_toks.append(name)
                res.register(self.advance())


            if self.current_tok.type != TT_RPAREN:
                tok = self.current_tok
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected ',' or ')'"))

        else:
            if self.current_tok.type != TT_RPAREN:
                tok = self.current_tok
                return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected identifier or ')'"))
        res.register(self.advance())

        if not self.current_tok.matches(TT_KEYWORD,'RETURNS'):
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'RETURNS'"))

        res.register(self.advance())
        if self.current_tok.value not in ['INTEGER','REAL','BOOLEAN','STRING','CHAR']:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'INTEGER','REAL','BOOLEAN','STRING' or 'CHAR'"))
        res.register(self.advance())
        if not self.current_tok.type == TT_Newline:
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected Newline'"))

        node_to_return = res.register(self.statements())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD,'RETURN'):
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'RETURN'"))
        res.register(self.advance())
        expr = res.register(self.expr())
        if res.error: return res
        res.register(self.advance())

        if not self.current_tok.matches(TT_KEYWORD,'ENDFUNCTION'):
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected 'ENDFUNCTION'"))
        res.register(self.advance())
        if not self.current_tok.type in (TT_Newline,TT_EOF):
            tok = self.current_tok
            return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected Newline or EOF'"))

        return res.success(FuncDefNode(
            var_name_tok,arg_name_toks,node_to_return,expr

        ))





#runtime result
#-------------------------------------------------

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self,res):
        if res.error: self.error = res.error
        return res.value

    def success(self,value):
        self.value = value

        return self

    def failure(self,error):
        self.error = error

        return self

#values
#-------------------------------------------------
class Value:
    def __init__(self):
        self.set_context()
        self.set_pos()
    def set_context(self,context = None):
        self.context = context
        return self

    def set_pos(self,pos_start = None,pos_end = None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def added_to(self,other):
        return None,self.illegal_operation(other)

    def subtracted_by(self,other):
        return None,self.illegal_operation(other)

    def multiplied_by(self,other):
        return None,self.illegal_operation(other)

    def divided_by(self,other):
        return None, self.illegal_operation(other)

    def powered_by(self,other):
        return None,self.illegal_operation(other)

    def get_comparison_eq(self, other):
        return None,self.illegal_operation(other)

    def get_comparison_ne(self, other):
        return None,self.illegal_operation(other)

    def get_comparison_lt(self, other):
        return None,self.illegal_operation(other)

    def get_comparison_gt(self, other):
        return None,self.illegal_operation(other)

    def get_comparison_lte(self, other):
        return None,self.illegal_operation(other)

    def get_comparison_gte(self, other):
        return None,self.illegal_operation(other)

    def anded_by(self, other):
        return None,self.illegal_operation(other)

    def ored_by(self, other):
        return None,self.illegal_operation(other)

    def notted(self):
        return None,self.illegal_operation(self)

    def execute(self, args):
        return RTResult().failure(self.illegal_operation())


    def copy(self):
        raise Exception('No copy method defined')
    def is_true(self):
        return False

    def illegal_operation(self, other=None):
        if not other: other = self
        return RTError(
            self.pos_start, other.pos_end,
            'Illegal operation',
            self.context
        )

class Number(Value):

    def __init__(self,value):
        self.value = value
        super().__init__()

    def set_context(self,context = None):
        self.context = context
        return self

    def set_pos(self,pos_start = None,pos_end = None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def added_to(self,other):
        if isinstance(other,Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def subtracted_by(self,other):
        if isinstance(other,Number):
            return Number(self.value - other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multiplied_by(self,other):
        if isinstance(other,Number):
            return Number(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def divided_by(self,other):
        if isinstance(other,Number):
            if other.value == 0:
                return None,RTError(
                    other.pos_start
                    ,other.pos_end,
                    'Divison by Zero',
                    self.context
                )
            return Number(self.value / other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def powered_by(self,other):
        if isinstance(other,Number):
            return Number(self.value ** other.value).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_eq(self, other):
        if isinstance(other, Number):

            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None


    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start,self.pos_end)
        copy.set_context(self.context)
        return copy

    def is_true(self):
        return self.value != 0
    def __repr__(self):
        return f'{self.value}'

class String(Value):
    def __init__(self,value):
        super().__init__()
        self.value = value

    def added_to(self,other):
        if isinstance(other,String):
            return String(self.value + other.value).set_context(self.context),None
        else:
            return None,Value.illegal_operation(self,other)

    def get_comparison_eq(self, other):
        if isinstance(other,String):
            return Number(self.value == other.value).set_context(self.context),None
        else:
            return None,Value.illegal_operation(self,other)

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start,self.pos_end)
        copy.set_context(self.context)
        return copy

    def __str__(self):
        return self.value
    def __repr__(self):
        return f'"{self.value}"'

class List(Value):
    def __init__(self,elements):
        super().__init__()
        self.elements = elements

    def added_to(self,other):
        new_list = self.copy()
        new_list.elements.append(other)
        return new_list,None
    def subtracted_by(self,other):
        if isinstance(other,Number):
            new_list = self.copy()
            try:
               new_list.elements.pop(other)
               return new_list,None
            except:
                return None,RTError(other.pos_start,other.pos_end,'Element at this indewx is out of bound',self.context)

        else:
            return None,Value.illegal_operation(self,other)
    def multiplied_by(self,other):
        if isinstance(other,List):
            new_list = self.copy()
            new_list.elements.extend(other.elements)
        else:
            return None,Value.illegal_operation(self,other)

    def divided_by(self,other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value],None
            except:
                return None, RTError(other.pos_start, other.pos_end, 'Element at this indewx is out of bound',
                                     self.context)

        else:
            return None, Value.illegal_operation(self, other)

    def copy(self):
        copy = List(self.elements)
        copy.set_pos(self.pos_start,self.pos_end)
        copy.set_pos(self.context)
        return copy

    def __repr__(self):
        return f"[{','.join([str(x) for x in self.elements])}]"

class BaseFunction(Value):
    def __init__(self,name):
        super().__init__()
        self.name = name

    def generateNewContext(self):
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        return new_context

    def check_args(self,arg_names,args):
        res = RTResult()
        if len(args) > len(arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(args) - len(arg_names)} too many args passed into '{self.name}'",
                self.context
            ))

        if len(args) < len(arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(arg_names) - len(args)} too few args passed into '{self.name}'",
                self.context
            ))
        return res.success(None)

    def populate_args(self,arg_names,args,exec_ctx):
        for i in range(len(args)):
            new_context = exec_ctx
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(new_context)
            new_context.symbol_table.set(arg_name, arg_value)


    def check_and_populate_args(self,arg_names,args,exec_ctx):
        res = RTResult()

        res.register(self.check_args(arg_names,args))
        if res.error: return res
        self.populate_args(arg_names,args,exec_ctx)
        return res.success(None)

class Procedure(BaseFunction):
    def __init__(self,name,body_node,arg_names):
        super().__init__(name)
        self.body_node = body_node
        self.arg_names = arg_names

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        new_context = self.generateNewContext()

        res.register(self.check_and_populate_args(self.arg_names,args,new_context))
        if res.error: return res
        value = res.register(interpreter.visit(self.body_node, new_context))
        if res.error: return res

        return res.success(Number(0))

    def copy(self):
        copy = Procedure(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy

    def __repr__(self):
        return f"<procedure {self.name}>"

class Function(BaseFunction):
    def __init__(self,name,body_node,arg_names,ret_node):
        super().__init__(name)
        self.body_node = body_node
        self.arg_names = arg_names
        self.ret_node = ret_node

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        new_context = self.generateNewContext()

        res.register(self.check_and_populate_args(self.arg_names,args,new_context))
        if res.error: return res
        value = res.register(interpreter.visit(self.body_node, new_context))
        if res.error: return res
        q = res.register(interpreter.visit(self.ret_node,new_context))
        return res.success(q)

    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names,self.ret_node)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy

    def __repr__(self):
        return f"<function {self.name}>"


class BuiltInFunction(BaseFunction):
    def __init__(self,name):
        super().__init__(name)


    def execute(self, args):
        res = RTResult()
        exec_ctx = self.generateNewContext()

        method_name = f'execute_{self.name}'
        method = getattr(self,method_name,self.no_visit_method)
        res.register(self.check_and_populate_args(method.arg_names,args,exec_ctx))
        if res.error: return res

        return_value = res.register(method(exec_ctx))
        if res.error: return res

        return res.success(return_value)

    def no_visit_method(self,node,context):
        raise Exception(f'No execute_{self.name} method defined')

    def copy(self):
        copy = BuiltInFunction(self.name)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start,self.pos_end)
        return copy

    def __repr__(self):
        return f'<built-in function {self.name}>'

    #BuiltIn functions

    def execute_output(self,exec_ctx):
        print(str(exec_ctx.symbol_table.get('value')))
        return RTResult().success(Number(0)) #return null equivalent

    execute_output.arg_names = ['value']


    def execute_left(self,exec_ctx):
        s = exec_ctx.symbol_table.get('s')
        a = exec_ctx.symbol_table.get('a')
        f = str(s)[:a.value]
        return RTResult().success(String(f))
    execute_left.arg_names = ['s','a']


    def execute_mid(self,exec_ctx):

        s = exec_ctx.symbol_table.get('s')
        a = exec_ctx.symbol_table.get('a')
        b = exec_ctx.symbol_table.get('b')
        f = str(s)[a.value - 1 :a.value - 1 + b.value ]
        return RTResult().success(String(f))

    execute_mid.arg_names = ['s','a','b']

    def execute_length(self,exec_ctx):
        s = exec_ctx.symbol_table.get('s')
        q = len(str(s))
        return RTResult().success(Number(q))

    execute_length.arg_names = ['s']

    def execute_asc(self,exec_ctx):
        s = exec_ctx.symbol_table.get('s')
        q = ord(str(s))
        return RTResult().success(Number(q))
    execute_asc.arg_names = ['s']

    def execute_chr(self,exec_ctx):
        n = exec_ctx.symbol_table.get('n')
        q = chr(n.value)
        return RTResult().success(String(q))

    execute_chr.arg_names = ['n']

BuiltInFunction.output = BuiltInFunction('output')
BuiltInFunction.mid = BuiltInFunction('mid')
BuiltInFunction.left = BuiltInFunction('left')
BuiltInFunction.length = BuiltInFunction('length')
BuiltInFunction.asc = BuiltInFunction('asc')
BuiltInFunction.chr = BuiltInFunction('chr')
#context
#--------------------------------------------------

class Context:
    def __init__(self,display_name, parent = None, parent_entry_pos = None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None


#symbol stable
#--------------------------------------------------

class SymbolTable:
    def __init__(self,parent = None):
        self.symbols = dict()
        self.parent = parent

    def get(self,name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self,name):
        del self.symbols[name]


#interpreter
#--------------------------------------------------

class Interpreter:
    def visit(self,node,context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self,method_name,self.no_visit_method)
        return method(node,context)



    def no_visit_method(self,node,context):
        raise Exception(f'No visit_{type(node).__name__} defined')

    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RTError(node.pos_start,
                                       node.pos_end,f"'{var_name} is not defined'",context))
        value = value.copy().set_pos(node.pos_start,node.pos_end).set_context(context)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node,context))
        if res.error: return res

        context.symbol_table.set(var_name,value)
        return res.success(value)

    def visit_NumberNode(self,node,context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start,node.pos_end)
        )

    def visit_StringNode(self,node,context):
        return RTResult().success(
            String(node.tok.value).set_context(context).set_pos(node.pos_start,node.pos_end)
        )
    def visit_ListNode(self,node,context):
        res = RTResult()
        elements = []
        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node,context)))
            if res.error:return res

        return res.success(
            List(elements).set_context(context).set_pos(node.pos_start,node.pos_end)
        )

    def visit_BinOpNode(self,node,context):
        res = RTResult()
        left = res.register(self.visit(node.left_node,context))
        if res.error: return res
        right = res.register(self.visit(node.right_node,context))
        if res.error: return res

        if node.op_tok.type == TT_PLUS or node.op_tok.type:
            result,error = left.added_to(right)

        if node.op_tok.type == TT_MINUS:
            result,error = left.subtracted_by(right)

        if node.op_tok.type == TT_MUL:
            result,error = left.multiplied_by(right)

        if node.op_tok.type == TT_DIV:
            result,error = left.divided_by(right)

        if node.op_tok.type == TT_POWER:
            result,error = left.powered_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, 'AND'):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, 'OR'):
            result, error = left.ored_by(right)


        if error: return res.failure(error)
        return res.success(result.set_pos(node.pos_start,node.pos_end))

    def visit_UnaryOpNode(self,node,context):
        res = RTResult()
        number = res.register(self.visit(node.node,context))
        if res.error: return res

        error = None

        if node.op_tok.type == TT_MINUS:
            number,error = number.multiplied_by(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD, 'NOT'):
            number, error = number.notted()

        if error: return res.failure(error)
        return res.success(number.set_pos(node.pos_start,node.pos_end))
    def visit_CaseOfNode(self,node,context):
        res = RTResult()
        orig = res.register(self.visit(node.orignalNode,context))
        if res.error:return res
        for val,expr in node.cases:
            val = res.register(self.visit(val,context))
            if res.error:return res
            if orig.value == val.value:
                expr_value = res.register(self.visit(expr,context))
                return res.success(Number(0))
        return res.success(Number(0))


    def visit_IfNode(self,node,context):
        res = RTResult()
        for condition,expr in node.cases:
            condition_value = res.register(self.visit(condition,context))
            if res.error: return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr,context))
                if res.error: return res
                return res.success(Number(0)) #returns null eq
            if node.else_case:
                else_value = res.register(self.visit(node.else_case,context))
                if res.error:return res
                return res.success(Number(0))
            return res.success(Number(0))

    def visit_ForNode(self,node,context):
        res = RTResult()
        elements = []

        start_value = res.register(self.visit(node.start_value_node,context))
        if res.error:return res

        end_value = res.register(self.visit(node.end_value_node,context))
        if res.error: return res

        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node,context))
            if res.error: return res

        else:
            step_value = Number(1)
        i = start_value.value

        if step_value.value >= 0:
            condition = lambda: i <= end_value.value
        else:
            condition = lambda: i >= end_value.value

        while condition():
            context.symbol_table.set(node.var_name_tok.value,Number(i))
            i += step_value.value
            elements.append(res.register(self.visit(node.body_node,context)))
            if res.error: return res

        return res.success(
            Number(0)
        )
    def visit_WhileNode(self,node,context):
        res = RTResult()
        elements = []
        while True:
            condition = res.register(self.visit(node.condition_node,context))
            if res.error: return res
            if not condition.is_true(): break
            elements.append(res.register(self.visit(node.body_node,context)))
            if res.error: return res

        return res.success(
            Number(0)
        )

    def visit_RepeatNode(self,node,context):
        res = RTResult()
        elements = []
        while True:
            elements.append(res.register(self.visit(node.body_node,context)))
            if res.error: return res
            condition = res.register(self.visit(node.condition_node,context))
            if res.error: return res
            if condition.is_true():break
        return res.success(
            Number(0)
        )
    def visit_ProcedureDefNode(self,node,context):
        res = RTResult()

        procedure_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]

        procedure_value = Procedure(procedure_name,body_node,arg_names).set_context(context).set_pos(node.pos_start,
                                                                                            node.pos_end)

        if node.var_name_tok:
            context.symbol_table.set(procedure_name, procedure_value)
        return res.success(procedure_value)
    def visit_FuncDefNode(self,node,context):
        res = RTResult()

        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names,node.ret_node).set_context(context).set_pos(node.pos_start,
                                                                                            node.pos_end)

        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)

        return res.success(func_value)

    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []

        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error: return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error: return res

        return_value = res.register(value_to_call.execute(args))
        if res.error: return res
        return_value = return_value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(return_value)
#run
#--------------------------------------------------

global_symbol_table = SymbolTable()
global_symbol_table.set('TRUE',Number(1))
global_symbol_table.set('FALSE',Number(0))

global_symbol_table.set('OUTPUT',BuiltInFunction.output)
global_symbol_table.set('MID',BuiltInFunction.mid)
global_symbol_table.set('LEFT',BuiltInFunction.left)
global_symbol_table.set('LENGTH',BuiltInFunction.length)
global_symbol_table.set('ASC',BuiltInFunction.asc)
global_symbol_table.set('CHR',BuiltInFunction.chr)

def run(fn,text):
    lexer = Lexer(fn,text)
    tokens,error = lexer.make_tokens()

    if error: return None,error

    #generate ast
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None,ast.error

    #runs
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node,context)

    return result.value,result.error

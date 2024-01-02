#imports
#-----------------------------

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
        result = f'{self.error_name}:{self.details}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end) + '\n'
        return result


class IllegalCharError(Error):
    def __init__(self,pos_start,pos_end,details):
        super().__init__(pos_start,pos_end,'Illegal Character',details)



class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details = ''):
        super().__init__(pos_start, pos_end, 'Invalid syntax', details)
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

        return 'Traceback (most recent call last ):\n' + result

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
TT_LT = 'NE'
TT_EE = 'EE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_NE = 'NE'
TT_EOF = "Eof"

KEYWORDS = [
'AND',"OR","NOT","IF","ELSE","THEN"
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

            elif self.current_char in LETTERS:

                tokens.append(self.make_identifier())

            elif self.current_char in DIGITS:
                tokens.append(self.make_number())

            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
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
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [],IllegalCharError(pos_start,self.pos,"'" + char + "'")


        tokens.append(Token(TT_EOF, pos_start=self.pos))

        return tokens, None

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

#Parser result
#---------------------------------------

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None


    def register(self,res):
        if isinstance(res,ParseResult):
            if res.error: self.error = res.error
            return res.node

        return res

    def success(self,node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self


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

    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '+', '-', '*' or '/'"
            ))
        return res
    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None
        if not self.current_tok.matches(TT_KEYWORD,'IF'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end,f"Expected 'IF'"))
        res.register(self.advance())
        condition = res.register(self.expr())

        if not self.current_tok.matches(TT_KEYWORD,'THEN'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start
                                                  ,self.current_tok.pos_end,
                                                  f"Expected 'THEN'"))
        res.register(self.advance())
        expr = res.register(self.expr())
        if res.error: return res
        cases.append((condition,expr))

        if self.current_tok.matches(TT_KEYWORD,'ELSE'):
            res.register(self.advance())

            expr = res.register(self.expr())
            if res.error: return res
            else_case = expr
        return res.success(IfNode(cases,else_case))

    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in [TT_INT,TT_REAL]:
            res.register(self.advance())
            return res.success(NumberNode(tok))

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
        elif tok.matches(TT_KEYWORD,'IF'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)

        return res.failure(InvalidSyntaxError(tok.pos_start,tok.pos_end,"Expected int,real, '+', '-', or '('"))

    def power(self):
        return self.bin_op(self.atom,(TT_POWER,),self.factor)

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS,TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
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
        if res.error: res.failure(InvalidSyntaxError(tok.pos_start,tok.pos_end,"Expected int,real, '+', '-', or '(','NOT'"))
        return res.success(node)
    def expr(self):

        res = ParseResult()

        if self.current_tok.type == TT_IDENTIFIER and self.hasEq != 0:
            self.hasEq -= 1
            var_name = self.current_tok
            res.register(self.current_tok)
            res.register(self.advance())

            res.register(self.advance())

            expr = res.register(self.expr())
            if res.error: return res

            return res.success(VarAssignNode(var_name,expr))




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

#runtime result
#-------------------------------------------------

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self,res):
        if res.error: self.error = res.error
        return  res.value

    def success(self,value):
        self.value = value

        return self

    def failure(self,error):
        self.error = error

        return self

#values
#-------------------------------------------------

class Number:

    def __init__(self,value):
        self.value = value
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
        if isinstance(other,Number):
            return Number(self.value + other.value).set_context(self.context), None

    def subtracted_by(self,other):
        if isinstance(other,Number):
            return Number(self.value - other.value).set_context(self.context), None

    def multiplied_by(self,other):
        if isinstance(other,Number):
            return Number(self.value * other.value).set_context(self.context), None

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

    def powered_by(self,other):
        if isinstance(other,Number):
            return Number(self.value ** other.value).set_context(self.context),None

    def get_comparison_eq(self, other):
        if isinstance(other, Number):

            return Number(int(self.value == other.value)).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None

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
    def __init__(self):
        self.symbols = dict()
        self.parent = None

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
        value = value.copy().set_pos(node.pos_start,node.pos_end)
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

    def visit_BinOpNode(self,node,context):
        res = RTResult()
        left = res.register(self.visit(node.left_node,context))
        if res.error: return res
        right = res.register(self.visit(node.right_node,context))
        if res.error: return res

        if node.op_tok.type == TT_PLUS:
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

    def visit_IfNode(self,node,context):
        res = RTResult()
        for condition,expr in node.cases:
            condition_value = res.register(self.visit(condition,context))
            if res.error: return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr,context))
                if res.error: return res
                return res.success(expr_value)
            if node.else_case:
                else_value = res.register(self.visit(node.else_case,context))
                if res.error:return res
                return res.success(else_value)
            return res.success(None)
#run
#--------------------------------------------------

global_symbol_table = SymbolTable()
global_symbol_table.set('TRUE',Number(1))
global_symbol_table.set('FALSE',Number(0))
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

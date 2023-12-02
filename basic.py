#Constant
#-----------------------------
DIGITS = '0123456789'


#ERRORS
#-----------------------------
class Error:
    def __init__(self, pos_start,pos_end,error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}:{self.details}'
        result += f'\nFile {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        return result


class IllegalCharError(Error):
    def __init__(self,pos_start,pos_end,details):
        super().__init__(pos_start,pos_end,'Illegal Character',details)


#Position
#-------------------------------------------


class Position:
    def __init__(self,idx,ln,col,fn,ftxt):
        self.fn = fn
        self.ftxt = ftxt
        self.idx = idx
        self.ln = ln
        self.col = col

    def advance(self,current_char):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0

        return self

    def copy(self):
        return Position(self.idx,self.ln,self.col,self.fn,self.ftxt)

#TOKENS
#-----------------------------
TT_INT = 'Int'
TT_REAL = 'Real'
TT_PLUS = 'Plus'
TT_MINUS = 'Minus'
TT_MUL = 'Mul'
TT_DIV = 'Div'
TT_LPAREN = 'LParen'
TT_RPAREN = "RParen"


class Token:
    def __init__(self,type_,value = None):
        self.type = type_
        self.value = value

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

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

    def make_tokens(self):
        tokens = []

        while self.current_char != None:

            if self.current_char in ' \t':
                self.advance()

            elif self.current_char in DIGITS:
                tokens.append(self.make_number())

            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL))
                self.advance()

            elif self.current_char == '/':
                tokens.append(Token(TT_DIV))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN))
                self.advance()
            else:
                pos_start = self.pos.copy()

                char = self.current_char
                self.advance()
                return [],IllegalCharError(pos_start,self.pos,"'" + char + "'")



        return tokens, None

    def make_number(self):
        num_str = ''
        dotCount = 0

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dotCount == 1: break
                num_str += '.'
                dotCount += 1
            else:
                num_str += self.current_char
            self.advance()
        if dotCount == 0:
            return Token(TT_INT,int(num_str))
        else:
            return Token(TT_REAL,float(num_str))

#run
#--------------------------------------------------


def run(fn,text):
    lexer = Lexer(fn,text)
    tokens,error = lexer.make_tokens()
    return tokens,error

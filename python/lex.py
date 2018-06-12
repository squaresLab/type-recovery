from pygments import lex
from pygments.lexers.c_cpp import CLexer, inherit
from pygments.token import Token, is_token_subtype

class BetterCLexer(CLexer):
    tokens = {
        'statements' : [
            (r'->', Token.Operator),
            (r'\+\+', Token.Operator),
            (r'--', Token.Operator),
            (r'==', Token.Operator),
            (r'!=', Token.Operator),
            (r'>=', Token.Operator),
            (r'<=', Token.Operator),
            (r'&&', Token.Operator),
            (r'\|\|', Token.Operator),
            (r'\+=', Token.Operator),
            (r'-=', Token.Operator),
            (r'\*=', Token.Operator),
            (r'/=', Token.Operator),
            (r'%=', Token.Operator),
            (r'&=', Token.Operator),
            (r'\^=', Token.Operator),
            (r'\|=', Token.Operator),
            (r'<<=', Token.Operator),
            (r'>>=', Token.Operator),
            (r'<<', Token.Operator),
            (r'>>', Token.Operator),
            (r'\.\.\.', Token.Operator),
            (r'##', Token.Operator),
            inherit
        ]
    }


class Lexer:
    def __init__(self, file_path):
        self.program_text = open(file_path, 'r').read()
        lexer = BetterCLexer()
        self.__tokenize(self.program_text, lexer)

    def __tokenize(self, program_text, lexer):
        self.token_list = list(lex(program_text, lexer))

    def get_tokens(self):
        return Lexer.format_token_list(self.token_list)

    @staticmethod
    def format_token_list(token_list):
        tokens = []
        for (token_type, token) in token_list:
            if is_token_subtype(token_type, Token.Comment):
                continue
            elif not is_token_subtype(token_type, Token.Text):
                tokens.append(token.strip())
        return tokens

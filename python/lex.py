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
        self.token_list = list(lex(self.program_text, lexer))

    def get_tokens(self):
        return Lexer.format_token_list(self.token_list)

    def replace_tokens_named(self, token_name, replace_with="!TOKEN!"):
        tokens = self.get_tokens()
        return [t.replace(token_name, replace_with) for t in tokens]

    @staticmethod
    def format_token_list(token_list):
        tokens = []
        for (token_type, token) in token_list:
            if is_token_subtype(token_type, Token.Comment):
                continue
            elif not is_token_subtype(token_type, Token.Text):
                tokens.append(token.strip())
        return tokens


def tokenize(file_name):
    lexer = Lexer(file_name)
    return lexer.get_tokens()

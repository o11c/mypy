"""Lexical analyzer for mypy.

Translate a string that represents a file or a compilation unit to a list of
tokens.

This module can be run as a script:
    python3 -m mypy.syntax.lexer FILE...
    python3 -m mypy.syntax.lexer < FILE
"""


from typing import (
        Any,
        Callable,
        List,
        Optional,
        Tuple,
        Union,

        cast,
)

import re

from mypy.syntax.dialect import Dialect, special_name_whitelist, top_tokens
from mypy.syntax.span import (
        CodeMap,
        FileInfo,
        Span,
)
import mypy.syntax.tokens as tokens
from mypy.syntax.tokens import Comment, HasToken, Token
from mypy.syntax.tokens import TokIndent, TokDedent, TokNewline, TokEof


# Bug: in python 2.7, if `from __future__ import unicode_literals` occurs
# after the docstring and the docstring does not have a u or b prefix,
# then the docstring will have bytes type instead of unicode.
# This is worked around in the `Parser` constructor instead.

# TODO https://github.com/JukkaL/mypy/issues/731
# FutureHook = Callable[[Token], 'FutureHook']
FutureHook = Callable[[Token], Any]

TokenOrTokenTree = Union[Token, 'TokenTree']
TokenLineOrTokenBlock = Union['TokenLine', 'TokenBlock']


class TokenTree(HasToken):
    __slots__ = ('_open', '_inner', '_close')

    def __init__(self, open: Token, inner: List[TokenOrTokenTree], close: Token) -> None:
        super().__init__(Span.make(open._span, close._span))
        self._open = open
        self._inner = inner
        self._close = close

    def __repr__(self) -> str:
        return 'TokenTree(%r, %r, %r)' % (self._open, self._inner, self._close)

    def __eq__(self, other) -> bool:
        return (
                self._span == other._span
                and self._open == other._open
                and self._inner == other._inner
                and self._close == other._close
        )

    def __ne__(self, other) -> bool:
        return not (self == other)

    def head(self) -> Token:
        return self._open

    def tail(self) -> Token:
        return self._close

class TokenLine(HasToken):
    __slots__ = ('_contents', '_terminator')

    def __init__(self, contents: List[TokenOrTokenTree], terminator: Token) -> None:
        span = Span.make(contents[0]._span if contents else terminator._span, terminator._span)
        super().__init__(span)
        self._contents = contents
        self._terminator = terminator

    def __repr__(self) -> str:
        return 'TokenLine(%r, %r)' % (self._contents, self._terminator)

    def __eq__(self, other) -> bool:
        return (
                self._span == other._span
                and self._contents == other._contents
                and self._terminator == other._terminator
        )

    def __ne__(self, other) -> bool:
        return not (self == other)

    def head(self) -> Token:
        return self._contents[0] if self._contents else self._terminator

    def tail(self) -> Token:
        return self._terminator

class TokenBlock(HasToken):
    __slots__ = ('_open', '_inner', '_close')

    def __init__(self, open: Token, inner: List[TokenLineOrTokenBlock], close: Token) -> None:
        assert inner
        super().__init__(Span.make(inner[0]._span, inner[-1]._span))
        self._open = open
        self._inner = inner
        self._close = close

    def __repr__(self) -> str:
        return 'TokenBlock(%r, %r, %r)' % (self._open, self._inner, self._close)

    def __eq__(self, other) -> bool:
        return (
                self._span == other._span
                and self._open == other._open
                and self._inner == other._inner
                and self._close == other._close
        )

    def __ne__(self, other) -> bool:
        return not (self == other)

    def head(self) -> Token:
        return self._open

    def tail(self) -> Token:
        return self._close

class TokenFile(HasToken):
    __slots__ = ('_contents', '_terminator')

    def __init__(self, contents: List[TokenLineOrTokenBlock], terminator: Token) -> None:
        span = Span.make(contents[0]._span if contents else terminator._span, terminator._span)
        super().__init__(span)
        self._contents = contents
        self._terminator = terminator

    def __repr__(self) -> str:
        return 'TokenFile(%r, %r)' % (self._contents, self._terminator)

    def __eq__(self, other) -> bool:
        return (
                self._span == other._span
                and self._contents == other._contents
                and self._terminator == other._terminator
        )

    def __ne__(self, other) -> bool:
        return not (self == other)

    def head(self) -> Token:
        return self._contents[0] if self._contents else self._terminator

    def tail(self) -> Token:
        return self._terminator


def future_disallowed(tok: Token) -> FutureHook:
    return future_disallowed

def future_allowed_docstring(tok: Token) -> FutureHook:
    if isinstance(tok, (tokens.LitBytes, tokens.LitUnicode)):
        return future_allowed_docstring
    if isinstance(tok, (tokens.TokNewline, tokens.OpSemicolon)):
        return future_allowed
    if isinstance(tok, tokens.KwFrom):
        return future_got_from
    return future_disallowed

def future_allowed(tok: Token) -> FutureHook:
    if isinstance(tok, tokens.KwFrom):
        return future_got_from
    return future_disallowed

def future_got_from(tok: Token) -> FutureHook:
    if isinstance(tok, tokens.TokName) and tok._text == '__future__':
        return future_got_from_future
    return future_disallowed

def future_got_from_future(tok: Token) -> FutureHook:
    if isinstance(tok, tokens.KwImport):
        return future_got_from_future_import
    return future_disallowed

def future_got_from_future_import(tok: Token) -> FutureHook:
    if isinstance(tok, tokens.TokName):
        return future_complete
    return future_disallowed

def future_complete(tok: Token) -> FutureHook:
    if isinstance(tok, tokens.OpComma):
        return future_got_from_future_import
    if isinstance(tok, (tokens.TokNewline, tokens.OpSemicolon)):
        return future_allowed
    return future_disallowed


class Lexer:

    # It's simpler to never match \n, but skip the emission of Newline.
    _whitespace_regex = re.compile('(?:[ \t\f]|\\\\\n)*')
    _pairs = {'(': ')', '[': ']', '{': '}'}

    def __init__(self, cm: CodeMap, file: FileInfo, dialect: Dialect) -> None:
        self._cm = cm
        self._file = file
        if False:
            self._dialect = dialect
            self._blackspace_regex = re.compile('')
        self.set_dialect(dialect)

        self._index = 0
        self._matching = [] # type: List[str]
        self._indents = ['']
        # For async/await keywords in 3.5/3.6 .
        self._def_async_stack = [False]
        self._def_async_cur = self._def_async_stack[-1]
        self._was_newline = True
        self._was_async = True
        self._future = future_allowed_docstring # type: FutureHook

    def set_dialect(self, dialect: Dialect) -> None:
        self._dialect = dialect
        self._blackspace_regex = re.compile(_compute_regex(dialect))

    def _pull_something(self) -> Tuple[str, str, str, Span]:
        ws_regex = self._whitespace_regex
        ws_match = ws_regex.match(self._file.content, self._index)
        assert ws_match is not None
        ws_txt = ws_match.group()
        ws_begin = ws_match.start()
        ws_end = ws_match.end()
        assert ws_begin == self._index
        if self._was_newline and not self._file.content.startswith(('#', '\n'), ws_end):
            # This whitespace is leading indentation.
            # Possibly emit an Indent or Dedent token.
            old_indent = self._indents[-1]
            # Formfeeds are ignored at start of line and undefined elsewhere.
            new_indent = ws_txt.replace('\f', '')
            if new_indent.startswith(old_indent):
                delta_indent = new_indent[len(old_indent):]
                if delta_indent:
                    self._indents.append(new_indent)
                    ws_middle = ws_end - len(delta_indent)
                    # ws_span = Span(ws_begin, ws_middle, self._file)
                    bs_span = Span(ws_middle, ws_end, self._file)
                    self._index = ws_end
                    return 'TokIndent', old_indent, delta_indent, bs_span
            else:
                # valid dedent, invalid dedent, or space/tab conflict
                if old_indent.startswith(new_indent):
                    bs_span = Span(ws_end, ws_end, self._file)
                    self._indents.pop()
                    prev_indent = self._indents[-1]
                    if prev_indent.startswith(new_indent):
                        # One level of dedent is valid.
                        # Do not update self._index; leave it for the next
                        # call, which may be another Dedent or a real token.
                        # Note that, while the span is located past the
                        # current index, it is not past the next blackspace.
                        self._index = ws_begin
                        return 'TokDedent', '', '', bs_span
                    assert new_indent.startswith(prev_indent)
                    delta_indent = new_indent[len(prev_indent):]
                    assert delta_indent
                    e_span = Span(ws_end - len(delta_indent), ws_end, self._file)
                    self._cm.error(e_span, 'invalid dedent level')
                    # Recover by adjusting the last indent to match this one.
                    self._indents.append(new_indent)
                else:
                    e_span = Span(ws_begin, ws_end, self._file)
                    self._cm.error(e_span, 'inconsistent use of tabs/spaces in indentation')
                    # Recover by pretending the indentation didn't change.
                    # In particular, don't attempt to figure out what looks
                    # more indented, not even in Python2 which permits it.
        self._index = ws_end

        bs_regex = self._blackspace_regex
        bs_match = bs_regex.match(self._file.content, self._index)
        if bs_match is None:
            e_span = Span(self._index, self._index + 1, self._file)
            self._cm.error(e_span, 'invalid token')
            # Recover by stepping past it.
            self._index += 1
            return 'TokError', ws_txt, self._file.content[self._index - 1], e_span

        bs_type = bs_match.lastgroup
        bs_txt = bs_match.group()
        bs_begin = bs_match.start()
        bs_end = bs_match.end()
        bs_span = Span(bs_begin, bs_end, self._file)
        assert bs_begin == self._index
        assert bs_begin != bs_end or bs_type == 'TokEof'
        self._index = bs_end
        if bs_type == 'X_LitString':
            string_prefix = bs_match.group('StringPrefix').lower()
            if string_prefix not in self._dialect.string_prefixes:
                pfx_span = Span(bs_begin, bs_begin + len(string_prefix), self._file)
                self._cm.error(pfx_span, 'invalid string prefix')
            if 'u' in string_prefix or ('b' not in string_prefix and self._dialect.default_unicode_literals):
                bs_type = 'LitUnicode'
            else:
                bs_type = 'LitBytes'
        return bs_type, ws_txt, bs_txt, bs_span

    def pull_token(self) -> Token:
        # TODO: pull out the 'coding[:=]' comment?
        # Python's rules are weird. It must be in a comment, and must
        # be the first thing in the file except for possibly the docstring.
        # Also, it must be within the first two lines.
        # It should be given its correct span, but not bump the pointer.
        # Also keep in mind the BOM.
        # (Currently, this API is entirely based on already-decoded text).

        comments = [] # type: List[Comment]
        more_ws = ''

        while True:
            type_name, white, black, span = self._pull_something()
            if type_name == 'Comment':
                comments.append(Comment(more_ws + white, black, span))
                more_ws = ''
                continue

            was_was_newline = self._was_newline
            if type_name == 'TokNewline':
                if self._was_newline or self._matching:
                    more_ws += white + '\n'
                    continue
                self._was_newline = True
            else:
                if type_name != 'TokDedent':
                    self._was_newline = False
            if was_was_newline:
                if type_name == 'TokIndent':
                    self._def_async_stack.append(self._def_async_cur)
                else:
                    if type_name == 'TokDedent':
                        self._def_async_stack.pop()
                    self._def_async_cur = self._def_async_stack[-1]
            if black == 'def':
                self._def_async_cur = self._was_async
            self._was_async = black == 'async'

            if type_name == 'TokName':
                is_kw = black in self._dialect.keywords
                if not is_kw and self._dialect.coroutines:
                    if self._def_async_cur and black in ('async', 'await'):
                        is_kw = True
                    elif black == 'async':
                        eows = self._whitespace_regex.match(self._file.content, self._index).end()
                        bs_match = self._blackspace_regex.match(self._file.content, eows)
                        is_kw = bs_match is not None and bs_match.group() == 'def'
                if is_kw:
                    type_name = 'Kw' + black.strip('_').title()
                elif black.startswith('__') and black.endswith('__') and black.strip('_'):
                    if black not in special_name_whitelist:
                        self._cm.warning(span, 'unrecognized dunder name')

            x = self._pairs.get(black)
            if x:
                self._matching.append(x)
            if self._matching:
                if self._matching[-1] == black:
                    self._matching.pop()
                elif type_name in top_tokens:
                    # pop the non-matching token
                    x = self._matching.pop()
                    # Inject an error.
                    type_name = 'TokError'
                    self._cm.error(span, 'expected %r before %r' % (x, black))
                    # step back for recovery
                    self._index -= len(black)
                    black = ''
                    span = Span(span.begin, span.begin, None)
            if type_name in ('OpSlash', 'OpSlashEqual') and not self._dialect.default_true_division:
                type_name = 'OpClassic' + type_name[2:]
                self._cm.warning(span, 'classic division; use true division or floor division instead')
            cls = getattr(tokens, type_name)
            rv = cls(comments, more_ws + white, black, span)
            self._future = self._future(rv) # type: FutureHook
            if self._future is future_complete:
                self.set_dialect(self._dialect.add_future(rv._text))
            return rv

    def pull_tt(self) -> TokenOrTokenTree:
        """Pull a token tree.

        A token tree aggregates all the tokens between paired tokens.

        For python, there are 4 pairs:
        - ( )
        - [ ]
        - { }
        - indent dedent

        But the last is done in pull_block(), not here.

        Note that for error recovery, sometimes extra pairs pop "too soon".

        Unmatched open parens will be paired with a TokError.

        Unmatched close parens will be passed through singly, like non-pairs.
        """
        open = self.pull_token()
        xlast = self._pairs.get(open._text)
        if xlast is None:
            return open
        inner = [] # type: List[TokenOrTokenTree]
        ml = len(self._matching)
        while True:
            close = self.pull_tt()
            if ml != len(self._matching):
                break
            inner.append(close)
        return TokenTree(open, inner, cast(Token, close))

    def pull_line(self) -> Union[TokenLine, Token]:
        """Pull a logical line of token trees.

        Each logical line may cover more than one physical line, in case of
        backslash-newline or being inside matched parentheses.

        The last token is always TokNewline or TokEof.
        """
        contents = [] # type: List[TokenOrTokenTree]
        while True:
            terminator = self.pull_tt()
            if isinstance(terminator, (TokIndent, TokDedent)):
                assert not contents
                return cast(Token, terminator)
            if isinstance(terminator, (TokNewline, TokEof)):
                break
            if isinstance(terminator, Token):
                if terminator._text in (')', ']', '}'):
                    self._cm.error(terminator._span, 'unexpected %r' % terminator._text)
            contents.append(terminator)
        return TokenLine(contents, cast(Token, terminator))

    def pull_block(self) -> Union[TokenLineOrTokenBlock, TokDedent]:
        """Pull an indented block or else a single line.

        This is analogous to pull_tt() but for TokenLine instead of Token.
        """
        open = self.pull_line()
        if not isinstance(open, TokIndent):
            return cast(Union[TokenLine, TokDedent], open)
        inner = [] # type: List[TokenLineOrTokenBlock]
        while True:
            close = self.pull_block()
            if isinstance(close, TokDedent):
                break
            assert isinstance(close, TokenLineOrTokenBlock)
            inner.append(close)
        return TokenBlock(cast(Token, open), inner, cast(Token, close))

    def pull_file(self) -> TokenFile:
        """Pull an entire file worth of tokens.

        This is analogous to TokenLine but for TokenBlock instead of TokenTree.

        This is *always* terminated by a TokEof, but if the input has an
        error there may be a different TokEof at the end of the last line.
        """
        contents = [] # type: List[TokenLineOrTokenBlock]
        while True:
            terminator = self.pull_block()
            assert isinstance(terminator, TokenLineOrTokenBlock)
            if isinstance(terminator, TokenLine):
                if not terminator._contents and isinstance(terminator._terminator, TokEof):
                    break
            contents.append(cast(TokenLineOrTokenBlock, terminator))
        return TokenFile(contents, cast(TokenLine, terminator)._terminator)

def _compute_regex(dialect: Dialect) -> str:
    plain = [
            ('OpBacktick', '`' if dialect.matmul else None),
            ('OpTilde', '~'),
            ('OpLess', '<'),
            ('OpGreater', '>'),
            ('OpLessEqual', '<='),
            ('OpGreaterEqual', '>='),
            ('OpEqEqual', '=='),
            # OpNotEqual is defined below

            ('OpPlus', '+'),
            ('OpMinus', '-'),
            ('OpStar', '*'),
            ('OpStarStar', '**'),
            ('OpSlash', '/'),
            ('OpSlashSlash', '//' if dialect.floor_division else None),
            ('OpPercent', '%'),
            ('OpAt', '@' if (dialect.decorators or dialect.matmul) else None),
            ('OpLShift', '<<'),
            ('OpRShift', '>>'),
            ('OpAmper', '&'),
            ('OpVBar', '|'),
            ('OpCircumflex', '^'),

            ('OpPlusEqual', '+='),
            ('OpMinusEqual', '-='),
            ('OpStarEqual', '*='),
            ('OpStarStarEqual', '**='),
            ('OpSlashEqual', '/='),
            ('OpSlashSlashEqual', '//=' if dialect.floor_division else None),
            ('OpPercentEqual', '%='),
            ('OpAtEqual', '@=' if dialect.matmul else None),
            ('OpLShiftEqual', '<<='),
            ('OpRShiftEqual', '>>='),
            ('OpAmperEqual', '&='),
            ('OpVBarEqual', '|='),
            ('OpCircumflexEqual', '^='),

            ('OpEqual', '='),
            ('OpLPar', '('),
            ('OpRPar', ')'),
            ('OpLSqb', '['),
            ('OpRSqb', ']'),
            ('OpLBrace', '{'),
            ('OpRBrace', '}'),
            ('OpComma', ','),
            ('OpColon', ':'),
            ('OpDot', '.'),
            # Technically 3 tokens in python2, but that is considered a bug.
            ('OpDotDotDot', '...'),
            ('OpSemicolon', ';'),
            ('OpRArrow', '->'),
    ]
    # Stupid regex engine does not return the longest match, requiring hacks.
    plain.sort(key=lambda x: x[1] or '', reverse=True)
    regexes = [
            ('OpNotEqual',
                '|'.join(
                    re.escape(x) for x in [
                        '!=' if dialect.ne_bang else None,
                        '<>' if dialect.ne_angle else None,
                    ] if x is not None)),
            ('TokEof',      r'\Z'),
            ('TokNewline',  '\n'),
            ('Comment',     '#.*'),
            # This is incomplete in python3, but `re` does not support matching
            # by unicode category.
            ('TokName',     '[A-Za-z_][A-Za-z0-9_]*(?![A-Za-z0-9_"\'])'),
            # Allow arbitrary prefixes at this step, filter later.
            ('X_LitString', '(?P<StringPrefix>(?:[A-Za-z_][A-Za-z0-9_]*)?)'
                            '(?:'
                                "(?!''')'(?:[^\\\\\n']|\\\\(?:.|\n))*'"
                                '|(?!""")"(?:[^\\\\\n"]|\\\\(?:.|\n))*"'
                                "|'''(?:(?!''')[^\\\\]|\\\\(?:.|\n))*'''"
                                '|"""(?:(?!""")[^\\\\]|\\\\(?:.|\n))*"""'
                            ')'),
            ('LitInteger',  '(?:'
                                '[1-9][0-9]*'
                                + ('|0+' if not dialect.octal_implicit else '|0')
                                + ('|0[oO][0-7]+' if dialect.octal_explicit else '')
                                + ('|0[0-7]+' if dialect.octal_implicit else '')
                                + '|0[xX][0-9a-fA-F]+'
                                + ('|0[bB][0-1]+' if dialect.binary_literals else '')
                            # only if dialect.long_integers
                            + ')' + ('[lL]?' if dialect.long_integers else '')
                            + '(?![A-Za-z_0-9.])'),
            ('LitFloat',    '(?:'
                                r'(?:[0-9]+)?\.[0-9]+'
                                r'|[0-9]+\.'
                                '|(?:'
                                    '[0-9]+'
                                    r'|(?:[0-9]+)?\.[0-9]+'
                                    r'|[0-9]+\.'
                                ')[eE][-+]?[0-9]+'
                            ')(?![A-Za-z_0-9.])'),
            ('LitImaginary','(?:'
                                r'(?:[0-9]+)?\.[0-9]+'
                                r'|[0-9]+\.'
                                '|(?:'
                                    '[0-9]+'
                                    r'|(?:[0-9]+)?\.[0-9]+'
                                    r'|[0-9]+\.'
                                ')[eE][-+]?[0-9]+'
                                '|[0-9]+'
                            ')[jJ](?![A-Za-z_0-9.])'),
    ]
    regexes += [(name, re.escape(pattern)) for (name, pattern) in plain if pattern is not None]
    return '|'.join('(?P<%s>%s)' % (name, pattern) for (name, pattern) in regexes if pattern is not None)


def _do_token(cm: CodeMap, token: Token) -> None:
    cm.warning(token._span, 'token %r' % token.__class__.__name__)

def _do_tree(cm: CodeMap, token_tree: TokenOrTokenTree) -> None:
    if isinstance(token_tree, Token):
        _do_token(cm, token_tree)
        return
    cm.warning(token_tree._span, 'begin tree')
    _do_token(cm, cast(TokenTree, token_tree)._open)
    for e in cast(TokenTree, token_tree)._inner:
        _do_tree(cm, e)
    _do_token(cm, cast(TokenTree, token_tree)._close)
    cm.warning(token_tree._span, 'end tree')

def _do_line(cm: CodeMap, token_line: TokenLine) -> None:
    cm.warning(token_line._span, 'begin line')
    for e in token_line._contents:
        _do_tree(cm, e)
    _do_token(cm, token_line._terminator)
    cm.warning(token_line._span, 'end line')

def _do_block(cm: CodeMap, token_block: TokenLineOrTokenBlock) -> None:
    if isinstance(token_block, TokenLine):
        _do_line(cm, token_block)
        return
    cm.warning(token_block._span, 'begin block')
    _do_token(cm, cast(TokenBlock, token_block)._open)
    for e in cast(TokenBlock, token_block)._inner:
        _do_block(cm, e)
    _do_token(cm, cast(TokenBlock, token_block)._close)
    cm.warning(token_block._span, 'end block')

def _do_file(cm: CodeMap, token_file: TokenFile) -> None:
    cm.warning(token_file._span, 'begin file')
    for e in token_file._contents:
        _do_block(cm, e)
    _do_token(cm, token_file._terminator)
    cm.warning(token_file._span, 'end file')

def _do_input(cm: CodeMap, dialect: Dialect, name: str, txt: str) -> bool:
    file = cm.add(name, txt)
    lexer = Lexer(cm, file, dialect)
    token_file = lexer.pull_file()
    _do_file(cm, token_file)
    print('Tokenized file %r with %d errors' % (name, cm.errors.count))
    rv = bool(cm.errors.count)
    cm.errors.count = 0
    return rv

def main() -> None:
    import sys
    args = sys.argv[1:]
    cm = CodeMap()
    dialect = Dialect(sys.version[:3])
    rv = 0
    for a in args:
        with open(a) as f:
            t = f.read()
        rv += _do_input(cm, dialect, a, t)
    if not args:
        rv += _do_input(cm, dialect, '<stdin>', sys.stdin.read())
    print('Tokenized %d files; %d had errors.' % (len(cm.files.data), rv))
    sys.exit(min(rv, 125))

if __name__ == '__main__':
    main()

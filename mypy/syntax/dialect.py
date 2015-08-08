"""Python dialect control.

This may be incomplet or inkorrekt.

Versions < 2.7 is not a priority.
Versions >= 3.0 and < 3.2 are not a priority.
Versions < 2.4 have many fundamental differences.
Versions < 2.2 have even more fundamental differences.
Versions 1.6 and 2.0 are not distinguished.
Versions < 1.6 are not even mentioned.
"""


from typing import (
        List,
        Set,
)


# Maintain our own copy instead of using __future__ just in case we need
# to parse a newer version of python than we're running.

available_futures = {
        'mypy-codec':       '0.0',
        'nested_scopes':    '2.1',
        'generators':       '2.2',
        'division':         '2.2',
        'absolute_import':  '2.5',
        'with_statement':   '2.5',
        'print_function':   '2.6',
        'unicode_literals': '2.6',
        'barry_as_FLUFL':   '3.1',
}

# List of __dunder__ names permitted anywhere in Python2 or Python3, either
# in the interpreter itself or in the standard libraries (e.g. pydoc). Any
# other name is a warning.
#
# Absolutely no attempt is made to verify whether these names are used
# *correctly*, or whether they mean anything in the current dialect.
special_name_whitelist = {
        '__BIT_TYPES_DEFINED__',
        '__GLIBC_MINOR__',
        '__GLIBC__',
        '__GNU_LIBRARY__',
        '__STDC_IEC_559_COMPLEX__',
        '__STDC_IEC_559__',
        '__STDC_ISO_10646__',
        '__UNDEF__',
        '__USER_LABEL_PREFIX__',
        '__about__',
        '__abs__',
        '__abstractmethods__',
        '__add__',
        '__all__',
        '__and__',
        '__annotations__',
        '__attribute__',
        '__attribute_alloc_size__',
        '__attribute_format_arg__',
        '__author__',
        '__bases__',
        '__bool__',
        '__builtin__',
        '__builtins__',
        '__bytes__',
        '__cached__',
        '__call__',
        '__cause__',
        '__ceil__',
        '__class__',
        '__closure__',
        '__cmp__',
        '__code__',
        '__coerce__',
        '__complex__',
        '__concat__',
        '__contains__',
        '__context__',
        '__copy__',
        '__copyright__',
        '__credits__',
        '__ctype_be__',
        '__ctype_le__',
        '__cvsid__',
        '__date__',
        '__debug__', # Keyword since 2.3 instead.
        '__decimal_context__',
        '__deepcopy__',
        '__defaults__',
        '__del__',
        '__delattr__',
        '__delete__',
        '__delitem__',
        '__delslice__',
        '__dict__',
        '__dir__',
        '__displayhook__',
        '__div__',
        '__divmod__',
        '__doc__',
        '__docformat__',
        '__enter__',
        '__eq__',
        '__excepthook__',
        '__exit__',
        '__file__',
        '__flags__',
        '__float__',
        '__floor__',
        '__floordiv__',
        '__format__',
        '__format_arg__',
        '__func__',
        '__future__',
        '__ge__',
        '__get__',
        '__getattr__',
        '__getattribute__',
        '__getformat__',
        '__getinitargs__',
        '__getitem__',
        '__getnewargs__',
        '__getslice__',
        '__getstate__',
        '__globals__',
        '__gt__',
        '__hash__',
        '__hex__',
        '__iadd__',
        '__iand__',
        '__iconcat__',
        '__idiv__',
        '__ifloordiv__',
        '__ilshift__',
        '__imod__',
        '__import__',
        '__imul__',
        '__index__',
        '__init__',
        '__initializing__',
        '__instancecheck__',
        '__int__',
        '__interactivehook__',
        '__inv__',
        '__invert__',
        '__ior__',
        '__ipow__',
        '__irshift__',
        '__isabstractmethod__',
        '__isub__',
        '__iter__',
        '__itruediv__',
        '__ixor__',
        '__kwdefaults__',
        '__le__',
        '__len__',
        '__length_hint__',
        '__libmpdec_version__',
        '__loader__',
        '__long__',
        '__lshift__',
        '__lt__',
        '__main__',
        '__members__',
        '__metaclass__',
        '__missing__',
        '__mod__',
        '__module__',
        '__mro__',
        '__mul__',
        '__name__',
        '__ne__',
        '__neg__',
        '__new__',
        '__new_member__',
        '__newobj__',
        '__newobj_ex__',
        '__next__',
        '__nonzero__',
        '__not__',
        '__objclass__',
        '__oct__',
        '__or__',
        '__package__',
        '__path__',
        '__pos__',
        '__pow__',
        '__prepare__',
        '__qualname__',
        '__radd__',
        '__rand__',
        '__rcmp__',
        '__rdiv__',
        '__rdivmod__',
        '__reduce__',
        '__reduce_ex__',
        '__repr__',
        '__reversed__',
        '__rfloordiv__',
        '__rlshift__',
        '__rmod__',
        '__rmul__',
        '__ror__',
        '__round__',
        '__rpow__',
        '__rrshift__',
        '__rshift__',
        '__rsub__',
        '__rtruediv__',
        '__rxor__',
        '__self__',
        '__set__',
        '__setattr__',
        '__setitem__',
        '__setslice__',
        '__setstate__',
        '__signature__',
        '__sizeof__',
        '__slotnames__',
        '__slots__',
        '__spec__',
        '__status__',
        '__stderr__',
        '__stdin__',
        '__stdout__',
        '__str__',
        '__sub__',
        '__subclasscheck__',
        '__subclasses__',
        '__subclasshook__',
        '__suppress_context__',
        '__test__',
        '__text_signature__',
        '__traceback__',
        '__truediv__',
        '__trunc__',
        '__unicode__',
        '__unittest_expecting_failure__',
        '__unittest_skip__',
        '__unittest_skip_why__',
        '__validate_parameters__',
        '__version__',
        '__warning__',
        '__warningregistry__',
        '__weakref__',
        '__wrapped__',
        '__xor__',
}

# Token names that force an error-pop of nested parentheses.
# Note that only a single level is popped at once.
# Some of the below might not be available in any given dialect.
# Note that 'TokEof' is implied and 'TokNewline' cannot happen.
top_tokens = {
        # special
        'OpRPar',
        'OpRSqb',
        'OpRBrace',

        # keywords
        'KwAs',
        'KwAssert',
        'KwAsync',
        'KwBreak',
        'KwClass',
        'KwContinue',
        'KwDef',
        'KwDel',
        'KwElif',
        'KwExcept',
        'KwExec',
        'KwFinally',
        'KwGlobal',
        'KwImport',
        'KwNonlocal',
        'KwPass',
        'KwPrint',
        'KwRaise',
        'KwReturn',
        'KwTry',
        'KwWhile',
        'KwWith',

        # operators
        'OpPlusEqual',
        'OpMinusEqual',
        'OpStarEqual',
        'OpStarStarEqual',
        'OpSlashEqual',
        'OpSlashSlashEqual',
        'OpPercentEqual',
        'OpAtEqual',
        'OpLShiftEqual',
        'OpRShiftEqual',
        'OpAmperEqual',
        'OpVBarEqual',
        'OpCircumflexEqual',
        'OpSemicolon',
        'OpRArrow',
}

def check_futures(version: str, futures: List[str]) -> Set[str]:
    for fut in futures:
        assert version >= available_futures[fut]
    return set(futures)

class Dialect:

    def __init__(self, version: str, future_list: List[str] = []) -> None:
        """Construct a dialect for the given Python version and future set.

        `version` is like `'2.7'`, e.g. `sys.version[:3]`.
        `future_list` is like `from __future__ import X, Y, Z`.
        """
        assert len(version) == 3 and version[1] == '.'
        assert version[0].isdigit() and version[2].isdigit()
        future_set = check_futures(version, future_list)
        self.base_version = version
        self.base_future_list = future_list
        self.base_future_set = future_set

        self.possible_futures = {k for (k, v) in available_futures.items() if v <= version}

        if '3.0' <= version:
            self.default_encoding = 'utf-8'
        elif version <= '2.4':
            self.default_encoding = 'latin1'
        else:
            self.default_encoding = 'ascii'

        # TODO: is there any better way to organize these than
        # "in the order I thought of them"?
        self.explicit_encoding = '2.3' <= version
        self.literal_none = '2.4' <= version
        self.literal_bool = '3.0' <= version
        # previously, you had to nest try-except inside a try-finally
        self.try_except_finally = '2.5' <= version
        self.with_keyword = '2.6' <= version or 'with_statement' in future_set
        self.with_multiple = version == '2.7' or '3.1' <= version
        self.as_keyword = '2.6' <= version or 'with_statement' in future_set
        self.except_as = '2.6' <= version
        self.except_comma = version <= '2.7'
        # Raw literals since forever.
        self.ur_literals = version <= '2.7'
        # Previously, just br literals since bytes_literals.
        self.rb_literals = '3.3' <= version
        self.default_unicode_literals = '3.0' <= version or 'unicode_literals' in future_set
        self.bytes_literals = '2.6' <= version
        self.raw_literals = True
        self.unicode_literals = '2.0' <= version <= '2.7' or '3.3' <= version
        self.mixed_strings = '2.0' <= version <= '2.7'
        # In ancient versions it was a function actually!
        # May take (at runtime!) a 2-tuple or 3-tuple though.
        self.exec_keyword = version <= '2.7'
        self.print_keyword = not ('3.0' <= version or 'print_function' in future_set)
        self.floor_division = '2.2' <= version
        self.default_true_division = '3.0' <= version or 'division' in future_set
        # Note: `yield` is an error in `async def`.
        self.yield_keyword = '2.3' <= version or 'generators' in future_set
        self.generator_comprehensions = '2.4' <= version
        self.yield_expression = '2.5' <= version
        self.yield_from = '3.3' <= version
        self.absolute_import = '3.0' <= version or 'absolute_import' in future_set
        self.nested_scopes = '2.2' <= version or 'nested_scopes' in future_set
        self.ne_angle = version <= '2.7' or 'barry_as_FLUFL' in future_set
        self.ne_bang = 'barry_as_FLUFL' not in future_set
        # sets were introduced in 2.4 without syntax
        self.set_literals = '2.7' <= version
        self.backticks = version <= '2.7'
        self.octal_implicit = version <= '2.7'
        self.octal_explicit = '2.6' <= version
        self.matmul = '3.5' <= version
        # For transition, async/await are only keywords in `async def`.
        # Note that entering a non-async def makes them names again.
        self.coroutines = '3.5' <= version
        # Future release; necessarily a guess.
        self.async_keyword = '3.7' <= version
        self.await_keyword = '3.7' <= version
        self.unicode_identifiers = '3.0' <= version
        self.function_annotations = '3.0' <= version or 'mypy-codec' in future_set
        self.mixed_tabs = version <= '2.7'
        self.keyword_only_arguments = '3.0' <= version
        self.base_class_keywords = '3.0' <= version
        self.nonlocal_keyword = '3.0' <= version
        self.extended_iterable_unpacking = '3.0' <= version
        self.old_style_classes = version <= '2.7'
        self.new_style_classes = '2.2' <= version
        self.dict_comprehensions = '2.7' <= version
        # same as set_literals
        self.set_comprehensions = '2.7' <= version
        self.binary_literals = '2.6' <= version
        self.long_integers = version <= '2.7'
        self.raise_from = '3.0' <= version
        self.list_comprehension_strict_tuples = '3.0' <= version
        self.ellipsis = '3.0' <= version
        self.tuple_parameter_unpacking = version <= '2.7'
        self.nested_glob_imports = version <= '2.7'
        self.raise_comma = version <= '2.7'
        self.conditional_expressions = '2.5' <= version
        self.decorators = '2.4' <= version
        self.class_decorators = '2.6' <= version
        self.relative_import = '2.5' <= version
        self.import_parens = '2.4' <= version
        self.literal_debug = '2.3' <= version
        self.list_comprehsnsions = '2.0' <= version
        self.compound_assign = '2.0' <= version
        self.argument_packs = '2.0' <= version
        self.print_file = '2.0' <= version
        self.import_as = '2.0' <= version
        self.sloppy_tuple_args = version < '2.0'
        self.sloppy_str_hex = version < '2.0'
        # Not *entirely* syntax-related, but close.
        # The encoding for EXTENDED_ARG appears to support arbitrary length,
        # but the value is stored in a signed 32-bit C integer.
        self.bytecode_arg_limit = 2**31 if '2.0' <= version else 2**16

        string_prefixes = ['', 'r']
        if self.bytes_literals:
            string_prefixes += ['b', 'br']
            if self.rb_literals:
                string_prefixes += ['rb']
        if self.unicode_literals:
            string_prefixes += ['u']
            if self.ur_literals:
                string_prefixes += ['ur']

        self.string_prefixes = set(string_prefixes)

        self.keywords = {
                None,
                '__debug__' if self.literal_debug else None,
                'False' if self.literal_bool else None,
                'None' if self.literal_none else None,
                'True' if self.literal_bool else None,
                'and',
                'as' if self.as_keyword else None,
                'assert',
                'async' if self.async_keyword else None,
                'await' if self.await_keyword else None,
                'break',
                'class',
                'continue',
                'def',
                'del',
                'elif',
                'else',
                'except',
                'exec' if self.exec_keyword else None,
                'finally',
                'for',
                'from',
                'global',
                'if',
                'import',
                'in',
                'is',
                'lambda',
                'nonlocal' if self.nonlocal_keyword else None,
                'not',
                'or',
                'pass',
                'print' if self.print_keyword else None,
                'raise',
                'return',
                'try',
                'while',
                'with' if self.with_keyword else None,
                'yield' if self.yield_keyword else None,
        }
        self.keywords.remove(None)
        self.operators = {
                None,
                # In this context, do not list textual operators:
                # and, await, in, is, not, or, yield
                '~',
                '<',
                '>',
                '<=',
                '>=',
                '==',
                '!=' if self.ne_bang else None,
                '<>' if self.ne_angle else None,

                '+',
                '-',
                '*',
                '**',
                '/',
                '//' if self.floor_division else None,
                '%',
                '@' if self.decorators or self.matmul else None,
                '<<',
                '>>',
                '&',
                '|',
                '^',

                '+=',
                '-=',
                '*=',
                '**=',
                '/=',
                '//=' if self.floor_division else None,
                '%=',
                '@=' if self.matmul else None,
                '<<=',
                '>>=',
                '&=',
                '|=',
                '^=',

                '(',
                ')',
                '[',
                ']',
                '{',
                '}',
                ',',
                ':',
                '.',
                '...', # technically 3 tokens in python2, but that is considered a bug
                ';',
                '=',
                '->',
        }
        self.operators.remove(None)

    def __repr__(self) -> str:
        return 'Dialect(%r, %r)' % (self.base_version, self.base_future_list)

    def add_future(self, future: str) -> 'Dialect':
        if future in self.base_future_set:
            return self
        return Dialect(self.base_version, self.base_future_list + [future])

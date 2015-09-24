"""List of trees.

Any given Python version only supports a subset of these constructs.
"""


from typing import (
        Generic as _Generic,
        List as _List,
        Optional as _Optional,
        TypeVar as _TypeVar,
        Union as _Union,
        cast as _cast,
)

from mypy.syntax.span import (
        HasSpan as _HasSpan,
        Span as _Span,
)
from mypy.syntax.tokens import *

from abc import abstractmethod

_T = _TypeVar('_T')
TokenOrTree = _Union[Token, 'Tree', None]

# Here be dragons.
#
# TreeError and TreeRecovered (and their support in Tree) are not what
# you would call *sane*, but they make the callers much, *much* simpler.
#
# And after all, they *only* take effect if there are syntax errors.
#
# TODO should TokError be given special handling too?
# I don't have any parser testcases for it yet.

# TODO give these all better names instead of token-derived ones.

class Tree(_HasSpan):
    __slots__ = ('_children')

    @classmethod
    def _is_error(cls, obj: object) -> bool:
        if isinstance(obj, list):
            return any(cls._is_error(o) for o in obj)
        assert isinstance(obj, TokenOrTree)

        # TokError should be caught via failure of .peek_if etc
        # and a TreeError should be explicitly constructed.
        assert not isinstance(obj, TokError)
        if isinstance(obj, TreeErrorBase):
            return True
        return False

    def __new__(cls, *children):
        # Inspects, but does not store, children.
        if not issubclass(cls, TreeErrorBase):
            if cls._is_error(list(children)):
                if cls is TreeList:
                    return TreeErrorList(*children)
                return TreeError(*children)
        return super().__new__(cls) # type: ignore

    def __init__(self, children: _List[TokenOrTree]) -> None:
        assert any(children), 'constructed Tree with no non-None children.'
        for first in children:
            if first:
                break
        for last in reversed(children):
            if last:
                break
        assert getattr(first, '_span', None), type(first).__name__
        assert getattr(last, '_span', None), type(first).__name__
        super().__init__(_Span.make(first._span, last._span))
        self._children = children

class TreeList(Tree):
    # Only constructed internally, in other Tree subclass constructors.
    def __new__(cls, children):
        if not children:
            return None
        return super().__new__(cls, children) # type: ignore

    def __init__(self, children: _List[_T]) -> None:
        assert all(isinstance(c, TokenOrTree) for c in children)
        super().__init__(_cast(_List[TokenOrTree], children))

class TreeErrorBase(Tree):
    pass

class TreeError(TreeErrorBase):
    # Magically called when any subclass of Tree contains a TreeErrorBase.
    def __init__(self, *children_: TokenOrTree) -> None:
        children = [c for c in children_ if c is not None]
        for i, c in enumerate(children):
            if isinstance(c, list):
                # might be TreeErrorList
                children[i] = TreeList(c)
        # Flatten certain nested errors, but not all:
        #   TreeError(TreeError(*x)) -> TreeError(*x)
        #   TreeError(*a, TreeError(*x), *b) -> unchanged
        #   TreeError(TreeErrorList([*x])) -> unchanged (to preserve invariant)
        #   TreeError(TreeRecovered(TreeError(*x))) -> TreeError(*x)
        #   TreeError(*a, TreeRecovered(TreeError(*x)), *b) -> TreeError(*a, TreeError(*x), *b)
        for i, c in enumerate(children):
            if isinstance(c, TreeRecovered):
                children[i] = c.err
        if len(children) == 1:
            if isinstance(children[0], TreeError):
                children = children[0]._children
        super().__init__(children)

class TreeErrorList(TreeList, TreeErrorBase):
    # Use TreeList's constructor.
    # Note that this can only occur within a TreeError.
    def __init__(self, children: _List[_T]) -> None:
        children = [c for c in children if c is not None]
        # Flatten certain nested errors, but not all:
        #   TreeErrorList([TreeError(*x)]) -> unchanged
        #   TreeErrorList([*a, TreeError(*x), *b]) -> unchanged
        #   TreeErrorList([*a, TreeRecovered(TreeError(*x)), *b]) -> TreeErrorList([*a, TreeError(*x), *b])
        for i, c in enumerate(children):
            if isinstance(c, TreeRecovered):
                children[i] = _cast(_T, c.err)
        super().__init__(children)

# Used *only* when it is easy to recover.
# This class is kind of awkward, but necessary to allow type-checking
# in recovered contexts and to inhibit the _is_error overwrite.
class TreeRecovered(Tree):
    __slots__ = ('err')

    @classmethod
    def _is_error(cls, obj: object) -> bool:
        return False

    def __init__(self, err: TreeError) -> None:
        # Sanity check? We sure need it.
        assert isinstance(err, TreeError)
        super().__init__([err])
        self.err = err # type: TreeError


AnyLitString = _Union[
        LitBytes,
        LitUnicode,
]

Literal = _Union[
        KwNone,
        KwTrue,
        KwFalse,
        KwDebug,
        OpDotDotDot,
        LitInteger,
        LitFloat,
        LitImaginary,
        'LitStrings',
]

AnyAtom = _Union[
        Literal,
        TokName,
        'YieldAtom',
        'ParenthForm',
        'GeneratorExpr',
        'ListLiteral',
        'ListComprehension',
        'SetLiteral',
        'SetComprehension',
        'DictLiteral',
        'DictComprehension',
        'LegacyRepr',
        TreeRecovered,
]

AnyPostfixExpr = _Union[
        AnyAtom,
        'CallExpr',
        'Subscription',
        'AttributeRef',
]

AnyAwaitExpr = _Union[
        AnyPostfixExpr,
        'AwaitExpr',
]

AnyPower = _Union[
        AnyAwaitExpr,
        'Power',
]

AnyFactor = _Union[
        AnyPower,
        'PlusFactor',
        'MinusFactor',
        'TildeFactor',
]

AnyTerm = _Union[
        AnyFactor,
        'StarTerm',
        'AtTerm',
        'ClassicSlashTerm',
        'SlashTerm',
        'PercentTerm',
        'SlashSlashTerm',
]

AnyArithExpr = _Union[
        AnyTerm,
        'PlusExpr',
        'MinusExpr',
]

AnyShiftExpr = _Union[
        AnyArithExpr,
        'LShiftExpr',
        'RShiftExpr',
]

AnyAndExpr = _Union[
        AnyShiftExpr,
        'AndExpr',
]

AnyXorExpr = _Union[
        AnyAndExpr,
        'XorExpr',
]

AnyOrExpr = _Union[
        AnyXorExpr,
        'OrExpr',
]

AnyComparison = _Union[
        AnyOrExpr,
        'ComparisonExprChain',
]

AnyNotTest = _Union[
        AnyComparison,
        'NotTest',
]

AnyAndTest = _Union[
        AnyNotTest,
        'AndTest',
]

AnyOrTest = _Union[
        AnyAndTest,
        'OrTest',
]

Test = _Union[
        AnyOrTest,
        'TernaryExpr',
        'LambdaExpr',
]

TestNocond = _Union[
        AnyOrTest,
        'LambdaExprNocond',
]

TestList = _Union[
        Test,
        'CommaExpr',
]

TestListNocond = _Union[
        TestNocond,
        'CommaExprNocond',
]

Target = _Union[
        TokName,
        'ParenTargetList',
        'SqbTargetList',
        'AttributeRef',
        'Subscription',
        'StarTarget',
]

TargetList = _Union[
        Target,
        'CommaTarget',
]


CompOp = _Union[
        OpLess,
        OpGreater,
        OpEqEqual,
        OpGreaterEqual,
        OpLessEqual,
        OpNotEqual,
        KwIn,
        'KwsNotIn',
        KwIs,
        'KwsIsNot',
]

YieldExpr = _Union[
        'SimpleYieldExpr',
        'YieldFromExpr',
]

AsyncStmt = _Union[
        'AsyncFuncDef',
        'AsyncForStmt',
        'AsyncWithStmt',
]

CompoundStmt = _Union[
        'IfStmt',
        'WhileStmt',
        'ForStmt',
        'TryStmt',
        'WithStmt',
        'FuncDef',
        'ClassDef',
        'Decorated',
        AsyncStmt,
]

Stmt = _Union[
        CompoundStmt,
        'SimpleStmt',
        TreeRecovered,
]

Decoratable = _Union[
        'ClassDef',
        'FuncDef',
        'AsyncFuncDef',
]

TfpDef = _Union[
        'Tname',
        'TfpList',
]

VfpDef = _Union[
        'Vname',
        'VfpList',
]

AnyExprStmt = _Union[
        'ExprStmt',
        'AugmentedAssignStmt',
        'AssignStmt',
]

RaiseStmt = _Union[
        'SimpleRaiseStmt',
        'RaiseFromStmt',
        'LegacyRaiseStmt',
]

ImportStmt = _Union[
        'ImportName',
        'ImportAll',
        'ImportFrom',
]

FlowStmt = _Union[
        'BreakStmt',
        'ContinueStmt',
        'ReturnStmt',
        RaiseStmt,
        'YieldStmt',
]

SmallStmt = _Union[
        AnyExprStmt,
        'PrintStmt',
        'DelStmt',
        'PassStmt',
        FlowStmt,
        ImportStmt,
        'GlobalStmt',
        'NonlocalStmt',
        'ExecStmt',
        'AssertStmt',
        #TreeRecovered,
]

AugTarget = _Union[
        TokName,
        'AttributeRef',
        'Subscription',
        'ParenAugTarget',
]

Augassign = _Union[
        OpPlusEqual,
        OpMinusEqual,
        OpStarEqual,
        OpAtEqual,
        OpClassicSlashEqual,
        OpSlashEqual,
        OpPercentEqual,
        OpAmperEqual,
        OpVBarEqual,
        OpCircumflexEqual,
        OpLShiftEqual,
        OpRShiftEqual,
        OpStarStarEqual,
        OpSlashSlashEqual,
]

YieldExprOrTestList = _Union[
        YieldExpr,
        TestList,
]

SequenceItem = _Union[
        Test,
        'StarTest',
]

SequenceItemList = _Union[
        Test, # NOT SequenceItem
        'CommaSequenceItem',
]

MappingItem = _Union[
        'TestColonTest',
        'StarStarTest',
]

Subscript = _Union[
        Test,
        'ProperSlice',
]

SubscriptList = _Union[
        Subscript,
        'CommaSubscript',
]

Argument = _Union[
        'PositionalArgument',
        'KeywordArgument',
        'StarTest',
        'StarStarTest',
]

ListIter = _Union[
        'ListFor',
        'ListIf',
]

CompIter = _Union[
        'CompFor',
        'CompIf',
]

AnyDots = _Union[
        OpDot,
        OpDotDotDot,
]

KwAsOrOpComma = _Union[
        KwAs,
        OpComma,
]

Suite = _Union[
        'SimpleStmt',
        'ComplexSuite',
        TreeRecovered,
]


# TODO compute proper First sets (needed for optional commas).
# Currently using limited contextual negatives, but that is not extensible.

class StarTest(Tree):
    __slots__ = ('star', 'test')

    def __init__(self,
            star: OpStar,
            test: Test,
    ) -> None:
        super().__init__([
            star,
            test,
        ])
        self.star = star
        self.test = test

class SequenceItemAndOpComma(Tree):
    __slots__ = ('item', 'comma')

    def __init__(self,
            item: SequenceItem,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            item,
            comma,
        ])
        self.item = item
        self.comma = comma

class TestColonTest(Tree):
    __slots__ = ('key', 'colon', 'value')

    def __init__(self,
            key: Test,
            colon: OpColon,
            value: Test,
    ) -> None:
        super().__init__([
            key,
            colon,
            value,
        ])
        self.key = key
        self.colon = colon
        self.value = value

class StarStarTest(Tree):
    __slots__ = ('star_star', 'test')

    def __init__(self,
            star_star: OpStarStar,
            test: Test,
    ) -> None:
        super().__init__([
            star_star,
            test,
        ])
        self.star_star = star_star
        self.test = test

class MappingItemAndOpComma(Tree):
    __slots__ = ('item', 'comma')

    def __init__(self,
            item: MappingItem,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            item,
            comma,
        ])
        self.item = item
        self.comma = comma

class ListFor(Tree):
    __slots__ = ('for_kw', 'targets', 'in_kw', 'tests', 'comp_iter')

    def __init__(self,
            for_kw: KwFor,
            targets: TargetList,
            in_kw: KwIn,
            tests: TestListNocond,
            comp_iter: _Optional[ListIter],
    ) -> None:
        super().__init__([
            for_kw,
            targets,
            in_kw,
            tests,
            comp_iter,
        ])
        self.for_kw = for_kw
        self.targets = targets
        self.in_kw = in_kw
        self.tests = tests
        self.comp_iter = comp_iter

class ListIf(Tree):
    __slots__ = ('if_kw', 'test', 'comp_iter')

    def __init__(self,
            if_kw: KwIf,
            test: TestNocond,
            comp_iter: _Optional[ListIter],
    ) -> None:
        super().__init__([
            if_kw,
            test,
            comp_iter,
        ])
        self.if_kw = if_kw
        self.test = test
        self.comp_iter = comp_iter

class CompFor(Tree):
    __slots__ = ('for_kw', 'targets', 'in_kw', 'test', 'comp_iter')

    def __init__(self,
            for_kw: KwFor,
            targets: TargetList,
            in_kw: KwIn,
            test: AnyOrTest,
            comp_iter: _Optional[CompIter],
    ) -> None:
        super().__init__([
            for_kw,
            targets,
            in_kw,
            test,
            comp_iter,
        ])
        self.for_kw = for_kw
        self.targets = targets
        self.in_kw = in_kw
        self.test = test
        self.comp_iter = comp_iter

class CompIf(Tree):
    __slots__ = ('if_kw', 'test', 'comp_iter')

    def __init__(self,
            if_kw: KwIf,
            test: TestNocond,
            comp_iter: _Optional[CompIter],
    ) -> None:
        super().__init__([
            if_kw,
            test,
            comp_iter,
        ])
        self.if_kw = if_kw
        self.test = test
        self.comp_iter = comp_iter

class SetLiteral(Tree):
    __slots__ = ('open', 'items', 'close')

    def __init__(self,
            open: OpLBrace,
            items: _List[SequenceItemAndOpComma],
            close: OpRBrace,
    ) -> None:
        super().__init__([
            open,
            TreeList(items),
            close,
        ])
        self.open = open
        self.items = items
        self.close = close

class SetComprehension(Tree):
    __slots__ = ('open', 'item', 'comp_for', 'close')

    def __init__(self,
            open: OpLBrace,
            item: Test,
            comp_for: CompFor,
            close: OpRBrace,
    ) -> None:
        super().__init__([
            open,
            item,
            comp_for,
            close,
        ])
        self.open = open
        self.item = item
        self.comp_for = comp_for
        self.close = close

class DictLiteral(Tree):
    __slots__ = ('open', 'items', 'close')

    def __init__(self,
            open: OpLBrace,
            items: _List[MappingItemAndOpComma],
            close: OpRBrace,
    ) -> None:
        super().__init__([
            open,
            TreeList(items),
            close,
        ])
        self.open = open
        self.items = items
        self.close = close

class DictComprehension(Tree):
    __slots__ = ('open', 'item', 'comp_for', 'close')

    def __init__(self,
            open: OpLBrace,
            item: TestColonTest,
            comp_for: CompFor,
            close: OpRBrace,
    ) -> None:
        super().__init__([
            open,
            item,
            comp_for,
            close,
        ])
        self.open = open
        self.item = item
        self.comp_for = comp_for
        self.close = close

class LegacyRepr(Tree):
    __slots__ = ('open', 'tests', 'close')

    def __init__(self,
            open: OpBacktick,
            tests: TestList,
            close: OpBacktick,
    ) -> None:
        super().__init__([
            open,
            tests,
            close,
        ])
        self.open = open
        self.tests = tests
        self.close = close

class SubscriptAndOpComma(Tree):
    __slots__ = ('subscript', 'comma')

    def __init__(self,
            subscript: Subscript,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            subscript,
            comma,
        ])
        self.subscript = subscript
        self.comma = comma

class CommaSubscript(Tree):
    __slots__ = ('subscripts')

    def __init__(self,
            subscripts: _List[SubscriptAndOpComma],
    ) -> None:
        super().__init__([
            TreeList(subscripts),
        ])
        self.subscripts = subscripts

class ProperSlice(Tree):
    __slots__ = ('start', 'colon1', 'end', 'colon2', 'step')

    def __init__(self,
            start: _Optional[Test],
            colon1: OpColon,
            end: _Optional[Test],
            colon2: _Optional[OpColon],
            step: _Optional[Test],
    ) -> None:
        super().__init__([
            start,
            colon1,
            end,
            colon2,
            step,
        ])
        self.start = start
        self.colon1 = colon1
        self.end = end
        self.colon2 = colon2
        self.step = step


class ArgumentAndOpComma(Tree):
    __slots__ = ('argument', 'comma')

    def __init__(self,
            argument: Argument,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            argument,
            comma,
        ])
        self.argument = argument
        self.comma = comma

class ArgList(Tree):
    __slots__ = ('arguments')

    def __init__(self,
            arguments: _List[ArgumentAndOpComma],
    ) -> None:
        super().__init__([
            TreeList(arguments),
        ])
        self.arguments = arguments

class ClassDef(Tree):
    __slots__ = ('class_kw', 'name', 'open', 'bases', 'close', 'colon', 'suite')

    def __init__(self,
            class_kw: KwClass,
            name: TokName,
            open: _Optional[OpLPar],
            bases: _Optional[ArgList],
            close: _Optional[OpRPar],
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            class_kw,
            name,
            open,
            bases,
            close,
            colon,
            suite,
        ])
        self.class_kw = class_kw
        self.name = name
        self.open = open
        self.bases = bases
        self.close = close
        self.colon = colon
        self.suite = suite

class PositionalArgument(Tree):
    __slots__ = ('test', 'comp_for')

    def __init__(self,
            test: Test,
            comp_for: _Optional[CompFor],
    ) -> None:
        super().__init__([
            test,
            comp_for,
        ])
        self.test = test
        self.comp_for = comp_for

class KeywordArgument(Tree):
    __slots__ = ('name', 'equal', 'test')

    def __init__(self,
            name: TokName,
            equal: OpEqual,
            test: Test,
    ) -> None:
        super().__init__([
            name,
            equal,
            test,
        ])
        self.name = name
        self.equal = equal
        self.test = test

class LitStrings(Tree):
    __slots__ = ('strings')

    def __init__(self,
            strings: _List[AnyLitString],
    ) -> None:
        super().__init__([
            TreeList(strings),
        ])
        self.strings = strings

class SimpleYieldExpr(Tree):
    __slots__ = ('yield_kw', 'tests')

    def __init__(self,
            yield_kw: KwYield,
            tests: _Optional[TestList],
    ) -> None:
        super().__init__([
            yield_kw,
            tests,
        ])
        self.yield_kw = yield_kw
        self.tests = tests

class YieldFromExpr(Tree):
    __slots__ = ('yield_kw', 'from_kw', 'test')

    def __init__(self,
            yield_kw: KwYield,
            from_kw: KwFrom,
            test: Test,
    ) -> None:
        super().__init__([
            yield_kw,
            from_kw,
            test,
        ])
        self.yield_kw = yield_kw
        self.from_kw = from_kw
        self.test = test

class YieldAtom(Tree):
    __slots__ = ('open', 'yield_expr', 'close')

    def __init__(self,
            open: OpLPar,
            yield_expr: YieldExpr,
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            yield_expr,
            close,
        ])
        self.open = open
        self.yield_expr = yield_expr
        self.close = close

class CommaSequenceItem(Tree):
    __slots__ = ('items')

    def __init__(self,
            items: _List[SequenceItemAndOpComma],
    ) -> None:
        super().__init__([
            TreeList(items),
        ])
        self.items = items

class ParenthForm(Tree):
    __slots__ = ('open', 'items', 'close')

    def __init__(self,
            open: OpLPar,
            items: SequenceItemList,
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            items,
            close,
        ])
        self.open = open
        self.items = items
        self.close = close

class GeneratorExpr(Tree):
    __slots__ = ('open', 'test', 'comp_for', 'close')

    def __init__(self,
            open: OpLPar,
            test: Test,
            comp_for: CompFor,
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            test,
            comp_for,
            close,
        ])
        self.open = open
        self.test = test
        self.comp_for = comp_for
        self.close = close

class ListLiteral(Tree):
    __slots__ = ('open', 'items', 'close')

    def __init__(self,
            open: OpLSqb,
            items: _List[SequenceItemAndOpComma],
            close: OpRSqb,
    ) -> None:
        super().__init__([
            open,
            TreeList(items),
            close,
        ])
        self.open = open
        self.items = items
        self.close = close

class ListComprehension(Tree):
    __slots__ = ('open', 'test', 'comp_for', 'close')

    def __init__(self,
            open: OpLSqb,
            test: Test,
            comp_for: ListFor,
            close: OpRSqb,
    ) -> None:
        super().__init__([
            open,
            test,
            comp_for,
            close,
        ])
        self.open = open
        self.test = test
        self.comp_for = comp_for
        self.close = close

class CallExpr(Tree):
    __slots__ = ('lhs', 'open', 'args', 'close')

    def __init__(self,
            lhs: AnyPostfixExpr,
            open: OpLPar,
            args: _Optional[ArgList],
            close: OpRPar,
    ) -> None:
        super().__init__([
            lhs,
            open,
            args,
            close,
        ])
        self.lhs = lhs
        self.open = open
        self.args = args
        self.close = close

class Subscription(Tree):
    __slots__ = ('lhs', 'open', 'subscripts', 'close')

    def __init__(self,
            lhs: AnyPostfixExpr,
            open: OpLSqb,
            subscripts: SubscriptList,
            close: OpRSqb,
    ) -> None:
        super().__init__([
            lhs,
            open,
            subscripts,
            close,
        ])
        self.lhs = lhs
        self.open = open
        self.subscripts = subscripts
        self.close = close

class AttributeRef(Tree):
    __slots__ = ('lhs', 'op', 'name')

    def __init__(self,
            lhs: AnyPostfixExpr,
            op: OpDot,
            name: TokName,
    ) -> None:
        super().__init__([
            lhs,
            op,
            name,
        ])
        self.lhs = lhs
        self.op = op
        self.name = name

class AwaitExpr(Tree):
    __slots__ = ('await_kw', 'rhs')

    def __init__(self,
            await_kw: KwAwait,
            rhs: AnyPostfixExpr,
    ) -> None:
        super().__init__([
            await_kw,
            rhs,
        ])
        self.await_kw = await_kw
        self.rhs = rhs

class Power(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyAwaitExpr,
            op: OpStarStar,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class PlusFactor(Tree):
    __slots__ = ('op', 'rhs')

    def __init__(self,
            op: OpPlus,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            op,
            rhs,
        ])
        self.op = op
        self.rhs = rhs

class MinusFactor(Tree):
    __slots__ = ('op', 'rhs')

    def __init__(self,
            op: OpMinus,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            op,
            rhs,
        ])
        self.op = op
        self.rhs = rhs

class TildeFactor(Tree):
    __slots__ = ('op', 'rhs')

    def __init__(self,
            op: OpTilde,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            op,
            rhs,
        ])
        self.op = op
        self.rhs = rhs

class StarTerm(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyTerm,
            op: OpStar,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class AtTerm(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyTerm,
            op: OpAt,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class ClassicSlashTerm(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyTerm,
            op: OpSlash,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class SlashTerm(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyTerm,
            op: OpSlash,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class PercentTerm(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyTerm,
            op: OpPercent,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class SlashSlashTerm(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyTerm,
            op: OpSlashSlash,
            rhs: AnyFactor,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class PlusExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyArithExpr,
            op: OpPlus,
            rhs: AnyTerm,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class MinusExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyArithExpr,
            op: OpMinus,
            rhs: AnyTerm,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class LShiftExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyShiftExpr,
            op: OpLShift,
            rhs: AnyArithExpr,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class RShiftExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyShiftExpr,
            op: OpRShift,
            rhs: AnyArithExpr,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class AndExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyAndExpr,
            op: OpAmper,
            rhs: AnyShiftExpr,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class XorExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyXorExpr,
            op: OpCircumflex,
            rhs: AnyAndExpr,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class OrExpr(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyOrExpr,
            op: OpVBar,
            rhs: AnyXorExpr,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class KwsNotIn(Tree):
    __slots__ = ('not_kw', 'in_kw')

    def __init__(self,
            not_kw: KwNot,
            in_kw: KwIn,
    ) -> None:
        super().__init__([
            not_kw,
            in_kw,
        ])
        self.not_kw = not_kw
        self.in_kw = in_kw

class KwsIsNot(Tree):
    __slots__ = ('is_kw', 'not_kw')

    def __init__(self,
            is_kw: KwIs,
            not_kw: KwNot,
    ) -> None:
        super().__init__([
            is_kw,
            not_kw,
        ])
        self.is_kw = is_kw
        self.not_kw = not_kw

class CompOpAndOrExpr(Tree):
    __slots__ = ('op', 'or_expr')

    def __init__(self,
            op: CompOp,
            or_expr: AnyOrExpr,
    ) -> None:
        super().__init__([
            op,
            or_expr,
        ])
        self.op = op
        self.or_expr = or_expr

class ComparisonExprChain(Tree):
    __slots__ = ('or_expr', 'compare_exprs')

    def __init__(self,
            or_expr: AnyOrExpr,
            compare_exprs: _List[CompOpAndOrExpr],
    ) -> None:
        super().__init__([
            or_expr,
            TreeList(compare_exprs),
        ])
        self.or_expr = or_expr
        self.compare_exprs = compare_exprs

class NotTest(Tree):
    __slots__ = ('op', 'rhs')

    def __init__(self,
            op: KwNot,
            rhs: AnyNotTest,
    ) -> None:
        super().__init__([
            op,
            rhs,
        ])
        self.op = op
        self.rhs = rhs

class AndTest(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyAndTest,
            op: KwAnd,
            rhs: AnyNotTest,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class OrTest(Tree):
    __slots__ = ('lhs', 'op', 'rhs')

    def __init__(self,
            lhs: AnyOrTest,
            op: KwOr,
            rhs: AnyAndTest,
    ) -> None:
        super().__init__([
            lhs,
            op,
            rhs,
        ])
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class TernaryExpr(Tree):
    __slots__ = ('value_if_true', 'if_kw', 'condition', 'else_kw', 'value_if_false')

    def __init__(self,
            value_if_true: AnyOrTest,
            if_kw: KwIf,
            condition: AnyOrTest,
            else_kw: KwElse,
            value_if_false: Test,
    ) -> None:
        super().__init__([
            value_if_true,
            if_kw,
            condition,
            else_kw,
            value_if_false,
        ])
        self.value_if_true = value_if_true
        self.if_kw = if_kw
        self.condition = condition
        self.else_kw = else_kw
        self.value_if_false = value_if_false

class TestAndOpComma(Tree):
    __slots__ = ('test', 'comma')

    def __init__(self,
            test: Test,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            test,
            comma,
        ])
        self.test = test
        self.comma = comma

class CommaExpr(Tree):
    __slots__ = ('tests')

    def __init__(self,
            tests: _List[TestAndOpComma],
    ) -> None:
        super().__init__([
            TreeList(tests),
        ])
        self.tests = tests

class TestNocondAndOpComma(Tree):
    __slots__ = ('test', 'comma')

    def __init__(self,
            test: TestNocond,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            test,
            comma,
        ])
        self.test = test
        self.comma = comma

class CommaExprNocond(Tree):
    __slots__ = ('tests')

    def __init__(self,
            tests: _List[TestNocondAndOpComma],
    ) -> None:
        super().__init__([
            TreeList(tests),
        ])
        self.tests = tests


class OpDotAndTokName(Tree):
    __slots__ = ('dot', 'name')

    def __init__(self,
            dot: OpDot,
            name: TokName,
    ) -> None:
        super().__init__([
            dot,
            name,
        ])
        self.dot = dot
        self.name = name

class DottedName(Tree):
    __slots__ = ('name', 'dot_names')

    def __init__(self,
            name: TokName,
            dot_names: _List[OpDotAndTokName],
    ) -> None:
        super().__init__([
            name,
            TreeList(dot_names),
        ])
        self.name = name
        self.dot_names = dot_names

class Decorator(Tree):
    __slots__ = ('at', 'name', 'open', 'args', 'close', 'newline')

    def __init__(self,
            at: OpAt,
            name: DottedName,
            open: _Optional[OpLPar],
            args: _Optional[ArgList],
            close: _Optional[OpRPar],
            newline: TokNewline,
    ) -> None:
        super().__init__([
            at,
            name,
            open,
            args,
            close,
            newline,
        ])
        self.at = at
        self.name = name
        self.open = open
        self.args = args
        self.close = close
        self.newline = newline

class Decorated(Tree):
    __slots__ = ('decorators', 'decoratee')

    def __init__(self,
            decorators: _List[Decorator],
            decoratee: Decoratable,
    ) -> None:
        super().__init__([
            TreeList(decorators),
            decoratee,
        ])
        self.decorators = decorators
        self.decoratee = decoratee

class TypedArg(Tree):
    __slots__ = ('target', 'equal', 'default', 'comma')

    def __init__(self,
            target: TfpDef,
            equal: _Optional[OpEqual],
            default: _Optional[Test],
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            target,
            equal,
            default,
            comma,
        ])
        self.target = target
        self.equal = equal
        self.default = default
        self.comma = comma

class TypedArgsList(Tree):
    __slots__ = ('positional_args', 'star', 'positional_arg_pack', 'comma', 'keyword_args', 'star_star', 'keyword_arg_pack')

    def __init__(self,
            positional_args: _List[TypedArg],
            star: _Optional[OpStar],
            positional_arg_pack: _Optional[TfpDef],
            comma: _Optional[OpComma],
            keyword_args: _List[TypedArg],
            star_star: _Optional[OpStarStar],
            keyword_arg_pack: _Optional[TfpDef],
    ) -> None:
        super().__init__([
            TreeList(positional_args),
            star,
            positional_arg_pack,
            comma,
            TreeList(keyword_args),
            star_star,
            keyword_arg_pack,
        ])
        self.positional_args = positional_args
        self.star = star
        self.positional_arg_pack = positional_arg_pack
        self.comma = comma
        self.keyword_args = keyword_args
        self.star_star = star_star
        self.keyword_arg_pack = keyword_arg_pack

class Tname(Tree):
    __slots__ = ('name', 'colon', 'annotation')

    def __init__(self,
            name: TokName,
            colon: _Optional[OpColon],
            annotation: _Optional[Test],
    ) -> None:
        super().__init__([
            name,
            colon,
            annotation,
        ])
        self.name = name
        self.colon = colon
        self.annotation = annotation

class TfpDefAndOpComma(Tree):
    __slots__ = ('tfp_def', 'comma')

    def __init__(self,
            tfp_def: TfpDef,
            comma: OpComma,
    ) -> None:
        super().__init__([
            tfp_def,
            comma,
        ])
        self.tfp_def = tfp_def
        self.comma = comma

class TfpList(Tree):
    __slots__ = ('open', 'targets', 'close')

    def __init__(self,
            open: OpLPar,
            targets: _List[TfpDefAndOpComma],
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            TreeList(targets),
            close,
        ])
        self.open = open
        self.targets = targets
        self.close = close

class Parameters(Tree):
    __slots__ = ('open', 'args', 'close')

    def __init__(self,
            open: OpLPar,
            args: _Optional[TypedArgsList],
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            args,
            close,
        ])
        self.open = open
        self.args = args
        self.close = close

class FuncDef(Tree):
    __slots__ = ('def_kw', 'name', 'parameters', 'r_arrow', 'return_annotation', 'colon', 'suite')

    def __init__(self,
            def_kw: KwDef,
            name: TokName,
            parameters: Parameters,
            r_arrow: _Optional[OpRArrow],
            return_annotation: _Optional[Test],
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            def_kw,
            name,
            parameters,
            r_arrow,
            return_annotation,
            colon,
            suite,
        ])
        self.def_kw = def_kw
        self.name = name
        self.parameters = parameters
        self.r_arrow = r_arrow
        self.return_annotation = return_annotation
        self.colon = colon
        self.suite = suite

class AsyncFuncDef(Tree):
    __slots__ = ('async_kw', 'func_def')

    def __init__(self,
            async_kw: KwAsync,
            func_def: FuncDef,
    ) -> None:
        super().__init__([
            async_kw,
            func_def,
        ])
        self.async_kw = async_kw
        self.func_def = func_def

class VarArg(Tree):
    __slots__ = ('target', 'equal', 'default', 'comma')

    def __init__(self,
            target: VfpDef,
            equal: _Optional[OpEqual],
            default: _Optional[Test],
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            target,
            equal,
            default,
            comma,
        ])
        self.target = target
        self.equal = equal
        self.default = default
        self.comma = comma

class VarArgsList(Tree):
    __slots__ = ('positional_args', 'star', 'positional_arg_pack', 'comma', 'keyword_args', 'star_star', 'keyword_arg_pack')

    def __init__(self,
            positional_args: _List[VarArg],
            star: _Optional[OpStar],
            positional_arg_pack: _Optional[VfpDef],
            comma: _Optional[OpComma],
            keyword_args: _List[VarArg],
            star_star: _Optional[OpStarStar],
            keyword_arg_pack: _Optional[VfpDef],
    ) -> None:
        super().__init__([
            TreeList(positional_args),
            star,
            positional_arg_pack,
            comma,
            TreeList(keyword_args),
            star_star,
            keyword_arg_pack,
        ])
        self.positional_args = positional_args
        self.star = star
        self.positional_arg_pack = positional_arg_pack
        self.comma = comma
        self.keyword_args = keyword_args
        self.star_star = star_star
        self.keyword_arg_pack = keyword_arg_pack

class VfpDefAndOpComma(Tree):
    __slots__ = ('vfp_def', 'comma')

    def __init__(self,
            vfp_def: VfpDef,
            comma: OpComma,
    ) -> None:
        super().__init__([
            vfp_def,
            comma,
        ])
        self.vfp_def = vfp_def
        self.comma = comma

class VfpList(Tree):
    __slots__ = ('open', 'targets', 'close')

    def __init__(self,
            open: OpLPar,
            targets: _List[VfpDefAndOpComma],
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            TreeList(targets),
            close,
        ])
        self.open = open
        self.targets = targets
        self.close = close

class Vname(Tree):
    __slots__ = ('name')

    def __init__(self,
            name: TokName,
    ) -> None:
        super().__init__([
            name,
        ])
        self.name = name

class LambdaExpr(Tree):
    __slots__ = ('lambda_kw', 'args', 'colon', 'body')

    def __init__(self,
            lambda_kw: KwLambda,
            args: _Optional[VarArgsList],
            colon: OpColon,
            body: Test,
    ) -> None:
        super().__init__([
            lambda_kw,
            args,
            colon,
            body,
        ])
        self.lambda_kw = lambda_kw
        self.args = args
        self.colon = colon
        self.body = body

class LambdaExprNocond(Tree):
    __slots__ = ('lambda_kw', 'args', 'colon', 'body')

    def __init__(self,
            lambda_kw: KwLambda,
            args: _Optional[VarArgsList],
            colon: OpColon,
            body: TestNocond,
    ) -> None:
        super().__init__([
            lambda_kw,
            args,
            colon,
            body,
        ])
        self.lambda_kw = lambda_kw
        self.args = args
        self.colon = colon
        self.body = body

class SmallStmtAndOpSemicolon(Tree):
    __slots__ = ('small_stmt', 'semicolon')

    def __init__(self,
            small_stmt: SmallStmt,
            semicolon: _Optional[OpSemicolon],
    ) -> None:
        super().__init__([
            small_stmt,
            semicolon,
        ])
        self.small_stmt = small_stmt
        self.semicolon = semicolon

class SimpleStmt(Tree):
    __slots__ = ('statements', 'newline')

    def __init__(self,
            statements: _List[SmallStmtAndOpSemicolon],
            newline: TokNewline,
    ) -> None:
        super().__init__([
            TreeList(statements),
            newline,
        ])
        self.statements = statements
        self.newline = newline

class ExprStmt(Tree):
    __slots__ = ('tests')

    def __init__(self,
            tests: TestList,
    ) -> None:
        super().__init__([
            tests,
        ])
        self.tests = tests

class AugmentedAssignStmt(Tree):
    __slots__ = ('target', 'augassign', 'yield_expr_or_test_list')

    def __init__(self,
            target: AugTarget,
            augassign: Augassign,
            yield_expr_or_test_list: YieldExprOrTestList,
    ) -> None:
        super().__init__([
            target,
            augassign,
            yield_expr_or_test_list,
        ])
        self.target = target
        self.augassign = augassign
        self.yield_expr_or_test_list = yield_expr_or_test_list

class ParenAugTarget(Tree):
    __slots__ = ('open', 'target', 'close')

    def __init__(self,
            open: OpLPar,
            target: AugTarget,
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            target,
            close,
        ])
        self.open = open
        self.target = target
        self.close = close

class TargetAndOpComma(Tree):
    __slots__ = ('target', 'comma')

    def __init__(self,
            target: Target,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            target,
            comma,
        ])
        self.target = target
        self.comma = comma

class CommaTarget(Tree):
    __slots__ = ('targets')

    def __init__(self,
            targets: _List[TargetAndOpComma],
    ) -> None:
        super().__init__([
            TreeList(targets),
        ])
        self.targets = targets

class ParenTargetList(Tree):
    __slots__ = ('open', 'targets', 'close')

    def __init__(self,
            open: OpLPar,
            targets: TargetList,
            close: OpRPar,
    ) -> None:
        super().__init__([
            open,
            targets,
            close,
        ])
        self.open = open
        self.targets = targets
        self.close = close

class SqbTargetList(Tree):
    __slots__ = ('open', 'targets', 'close')

    def __init__(self,
            open: OpLSqb,
            targets: CommaTarget,
            close: OpRSqb,
    ) -> None:
        super().__init__([
            open,
            targets,
            close,
        ])
        self.open = open
        self.targets = targets
        self.close = close

class StarTarget(Tree):
    __slots__ = ('star', 'target')

    def __init__(self,
            star: OpStar,
            target: Target,
    ) -> None:
        super().__init__([
            star,
            target,
        ])
        self.star = star
        self.target = target

class TargetListAndOpEqual(Tree):
    __slots__ = ('targets', 'equal')

    def __init__(self,
            targets: TargetList,
            equal: OpEqual,
    ) -> None:
        super().__init__([
            targets,
            equal,
        ])
        self.targets = targets
        self.equal = equal

class AssignStmt(Tree):
    __slots__ = ('target_lists', 'yield_expr_or_test_list')

    def __init__(self,
            target_lists: _List[TargetListAndOpEqual],
            yield_expr_or_test_list: YieldExprOrTestList,
    ) -> None:
        super().__init__([
            TreeList(target_lists),
            yield_expr_or_test_list,
        ])
        self.target_lists = target_lists
        self.yield_expr_or_test_list = yield_expr_or_test_list

class PrintStmt(Tree):
    __slots__ = ('print_kw', 'r_shift', 'stream', 'comma', 'exprs')

    def __init__(self,
            print_kw: KwPrint,
            r_shift: _Optional[OpRShift],
            stream: _Optional[Test],
            comma: _Optional[OpComma],
            exprs: _List[TestAndOpComma],
    ) -> None:
        super().__init__([
            print_kw,
            r_shift,
            stream,
            comma,
            TreeList(exprs),
        ])
        self.print_kw = print_kw
        self.r_shift = r_shift
        self.stream = stream
        self.comma = comma
        self.exprs = exprs

class DelStmt(Tree):
    __slots__ = ('del_kw', 'targets')

    def __init__(self,
            del_kw: KwDel,
            targets: TargetList,
    ) -> None:
        super().__init__([
            del_kw,
            targets,
        ])
        self.del_kw = del_kw
        self.targets = targets

class PassStmt(Tree):
    __slots__ = ('pass_kw')

    def __init__(self,
            pass_kw: KwPass,
    ) -> None:
        super().__init__([
            pass_kw,
        ])
        self.pass_kw = pass_kw

class BreakStmt(Tree):
    __slots__ = ('break_kw')

    def __init__(self,
            break_kw: KwBreak,
    ) -> None:
        super().__init__([
            break_kw,
        ])
        self.break_kw = break_kw

class ContinueStmt(Tree):
    __slots__ = ('continue_kw')

    def __init__(self,
            continue_kw: KwContinue,
    ) -> None:
        super().__init__([
            continue_kw,
        ])
        self.continue_kw = continue_kw

class ReturnStmt(Tree):
    __slots__ = ('return_kw', 'tests')

    def __init__(self,
            return_kw: KwReturn,
            tests: _Optional[TestList],
    ) -> None:
        super().__init__([
            return_kw,
            tests,
        ])
        self.return_kw = return_kw
        self.tests = tests

class YieldStmt(Tree):
    __slots__ = ('yield_expr')

    def __init__(self,
            yield_expr: YieldExpr,
    ) -> None:
        super().__init__([
            yield_expr,
        ])
        self.yield_expr = yield_expr

class SimpleRaiseStmt(Tree):
    __slots__ = ('raise_kw', 'test')

    def __init__(self,
            raise_kw: KwRaise,
            test: _Optional[Test],
    ) -> None:
        super().__init__([
            raise_kw,
            test,
        ])
        self.raise_kw = raise_kw
        self.test = test

class RaiseFromStmt(Tree):
    __slots__ = ('raise_kw', 'test', 'from_kw', 'context')

    def __init__(self,
            raise_kw: KwRaise,
            test: Test,
            from_kw: KwFrom,
            context: Test,
    ) -> None:
        super().__init__([
            raise_kw,
            test,
            from_kw,
            context,
        ])
        self.raise_kw = raise_kw
        self.test = test
        self.from_kw = from_kw
        self.context = context

class LegacyRaiseStmt(Tree):
    __slots__ = ('raise_kw', 'type', 'comma1', 'value', 'comma2', 'traceback')

    def __init__(self,
            raise_kw: KwRaise,
            type: Test,
            comma1: OpComma,
            value: Test,
            comma2: _Optional[OpComma],
            traceback: _Optional[Test],
    ) -> None:
        super().__init__([
            raise_kw,
            type,
            comma1,
            value,
            comma2,
            traceback,
        ])
        self.raise_kw = raise_kw
        self.type = type
        self.comma1 = comma1
        self.value = value
        self.comma2 = comma2
        self.traceback = traceback

class DottedAsName(Tree):
    __slots__ = ('dotted_name', 'as_kw', 'name', 'comma')

    def __init__(self,
            dotted_name: DottedName,
            as_kw: _Optional[KwAs],
            name: _Optional[TokName],
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            dotted_name,
            as_kw,
            name,
            comma,
        ])
        self.dotted_name = dotted_name
        self.as_kw = as_kw
        self.name = name
        self.comma = comma

class ImportName(Tree):
    __slots__ = ('import_kw', 'names')

    def __init__(self,
            import_kw: KwImport,
            names: _List[DottedAsName],
    ) -> None:
        super().__init__([
            import_kw,
            TreeList(names),
        ])
        self.import_kw = import_kw
        self.names = names

class ImportAll(Tree):
    __slots__ = ('from_kw', 'dots', 'dotted_name', 'import_kw', 'star')

    def __init__(self,
            from_kw: KwFrom,
            dots: _List[AnyDots],
            dotted_name: _Optional[DottedName],
            import_kw: KwImport,
            star: OpStar,
    ) -> None:
        super().__init__([
            from_kw,
            TreeList(dots),
            dotted_name,
            import_kw,
            star,
        ])
        self.from_kw = from_kw
        self.dots = dots
        self.dotted_name = dotted_name
        self.import_kw = import_kw
        self.star = star

class ImportAsName(Tree):
    __slots__ = ('orig_name', 'as_kw', 'local_name', 'comma')

    def __init__(self,
            orig_name: TokName,
            as_kw: _Optional[KwAs],
            local_name: _Optional[TokName],
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            orig_name,
            as_kw,
            local_name,
            comma,
        ])
        self.orig_name = orig_name
        self.as_kw = as_kw
        self.local_name = local_name
        self.comma = comma

class ImportFrom(Tree):
    __slots__ = ('from_kw', 'dots', 'dotted_name', 'import_kw', 'open', 'names', 'close')

    def __init__(self,
            from_kw: KwFrom,
            dots: _List[AnyDots],
            dotted_name: _Optional[DottedName],
            import_kw: KwImport,
            open: _Optional[OpLPar],
            names: _List[ImportAsName],
            close: _Optional[OpRPar],
    ) -> None:
        super().__init__([
            from_kw,
            TreeList(dots),
            dotted_name,
            import_kw,
            open,
            TreeList(names),
            close,
        ])
        self.from_kw = from_kw
        self.dots = dots
        self.dotted_name = dotted_name
        self.import_kw = import_kw
        self.open = open
        self.names = names
        self.close = close

class TokNameAndOpComma(Tree):
    __slots__ = ('name', 'comma')

    def __init__(self,
            name: TokName,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            name,
            comma,
        ])
        self.name = name
        self.comma = comma

class GlobalStmt(Tree):
    __slots__ = ('global_kw', 'names')

    def __init__(self,
            global_kw: KwGlobal,
            names: _List[TokNameAndOpComma],
    ) -> None:
        super().__init__([
            global_kw,
            TreeList(names),
        ])
        self.global_kw = global_kw
        self.names = names

class NonlocalStmt(Tree):
    __slots__ = ('nonlocal_kw', 'names')

    def __init__(self,
            nonlocal_kw: KwNonlocal,
            names: _List[TokNameAndOpComma],
    ) -> None:
        super().__init__([
            nonlocal_kw,
            TreeList(names),
        ])
        self.nonlocal_kw = nonlocal_kw
        self.names = names

class ExecStmt(Tree):
    __slots__ = ('exec_kw', 'or_expr', 'in_kw', 'globals', 'comma', 'locals')

    def __init__(self,
            exec_kw: KwExec,
            or_expr: AnyOrExpr,
            in_kw: _Optional[KwIn],
            globals: _Optional[Test],
            comma: _Optional[OpComma],
            locals: _Optional[Test],
    ) -> None:
        super().__init__([
            exec_kw,
            or_expr,
            in_kw,
            globals,
            comma,
            locals,
        ])
        self.exec_kw = exec_kw
        self.or_expr = or_expr
        self.in_kw = in_kw
        self.globals = globals
        self.comma = comma
        self.locals = locals

class AssertStmt(Tree):
    __slots__ = ('assert_kw', 'test', 'comma', 'message')

    def __init__(self,
            assert_kw: KwAssert,
            test: Test,
            comma: _Optional[OpComma],
            message: _Optional[Test],
    ) -> None:
        super().__init__([
            assert_kw,
            test,
            comma,
            message,
        ])
        self.assert_kw = assert_kw
        self.test = test
        self.comma = comma
        self.message = message

class IfSuite(Tree):
    __slots__ = ('if_kw', 'condition', 'colon', 'suite')

    def __init__(self,
            if_kw: KwIf,
            condition: Test,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            if_kw,
            condition,
            colon,
            suite,
        ])
        self.if_kw = if_kw
        self.condition = condition
        self.colon = colon
        self.suite = suite

class ElifSuite(Tree):
    __slots__ = ('elif_kw', 'condition', 'colon', 'suite')

    def __init__(self,
            elif_kw: KwElif,
            condition: Test,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            elif_kw,
            condition,
            colon,
            suite,
        ])
        self.elif_kw = elif_kw
        self.condition = condition
        self.colon = colon
        self.suite = suite

class ElseSuite(Tree):
    __slots__ = ('else_kw', 'colon', 'suite')

    def __init__(self,
            else_kw: KwElse,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            else_kw,
            colon,
            suite,
        ])
        self.else_kw = else_kw
        self.colon = colon
        self.suite = suite

class IfStmt(Tree):
    __slots__ = ('if_suite', 'elif_suites', 'else_suite')

    def __init__(self,
            if_suite: IfSuite,
            elif_suites: _List[ElifSuite],
            else_suite: _Optional[ElseSuite],
    ) -> None:
        super().__init__([
            if_suite,
            TreeList(elif_suites),
            else_suite,
        ])
        self.if_suite = if_suite
        self.elif_suites = elif_suites
        self.else_suite = else_suite

class WhileSuite(Tree):
    __slots__ = ('while_kw', 'condition', 'colon', 'body_suite')

    def __init__(self,
            while_kw: KwWhile,
            condition: Test,
            colon: OpColon,
            body_suite: Suite,
    ) -> None:
        super().__init__([
            while_kw,
            condition,
            colon,
            body_suite,
        ])
        self.while_kw = while_kw
        self.condition = condition
        self.colon = colon
        self.body_suite = body_suite

class WhileStmt(Tree):
    __slots__ = ('while_suite', 'else_suite')

    def __init__(self,
            while_suite: WhileSuite,
            else_suite: _Optional[ElseSuite],
    ) -> None:
        super().__init__([
            while_suite,
            else_suite,
        ])
        self.while_suite = while_suite
        self.else_suite = else_suite

class ForSuite(Tree):
    __slots__ = ('for_kw', 'targets', 'in_kw', 'tests', 'colon', 'suite')

    def __init__(self,
            for_kw: KwFor,
            targets: TargetList,
            in_kw: KwIn,
            tests: TestList,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            for_kw,
            targets,
            in_kw,
            tests,
            colon,
            suite,
        ])
        self.for_kw = for_kw
        self.targets = targets
        self.in_kw = in_kw
        self.tests = tests
        self.colon = colon
        self.suite = suite

class ForStmt(Tree):
    __slots__ = ('for_suite', 'else_suite')

    def __init__(self,
            for_suite: ForSuite,
            else_suite: _Optional[ElseSuite],
    ) -> None:
        super().__init__([
            for_suite,
            else_suite,
        ])
        self.for_suite = for_suite
        self.else_suite = else_suite

class AsyncForStmt(Tree):
    __slots__ = ('async_kw', 'for_stmt')

    def __init__(self,
            async_kw: KwAsync,
            for_stmt: ForStmt,
    ) -> None:
        super().__init__([
            async_kw,
            for_stmt,
        ])
        self.async_kw = async_kw
        self.for_stmt = for_stmt

class ExceptClause(Tree):
    __slots__ = ('except_kw', 'test', 'as_kw', 'name')

    def __init__(self,
            except_kw: KwExcept,
            test: _Optional[Test],
            as_kw: _Optional[KwAsOrOpComma],
            name: _Optional[TokName],
    ) -> None:
        super().__init__([
            except_kw,
            test,
            as_kw,
            name,
        ])
        self.except_kw = except_kw
        self.test = test
        self.as_kw = as_kw
        self.name = name

class TrySuite(Tree):
    __slots__ = ('try_kw', 'colon', 'suite')

    def __init__(self,
            try_kw: KwTry,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            try_kw,
            colon,
            suite,
        ])
        self.try_kw = try_kw
        self.colon = colon
        self.suite = suite

class ExceptSuite(Tree):
    __slots__ = ('except_clause', 'colon', 'suite')

    def __init__(self,
            except_clause: ExceptClause,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            except_clause,
            colon,
            suite,
        ])
        self.except_clause = except_clause
        self.colon = colon
        self.suite = suite

class FinallySuite(Tree):
    __slots__ = ('finally_kw', 'colon', 'suite')

    def __init__(self,
            finally_kw: KwFinally,
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            finally_kw,
            colon,
            suite,
        ])
        self.finally_kw = finally_kw
        self.colon = colon
        self.suite = suite

class TryStmt(Tree):
    __slots__ = ()

    def __init__(self,
            try_suite: TrySuite,
            except_suites: _List[ExceptSuite],
            else_suite: _Optional[ElseSuite],
            finally_suite: _Optional[FinallySuite],
    ) -> None:
        assert except_suites or finally_suite
        super().__init__([
            try_suite,
            TreeList(except_suites),
            else_suite,
            finally_suite,
        ])
        self.try_suite = try_suite
        self.except_suites = except_suites
        self.else_suite = else_suite
        self.finally_suite = finally_suite

class WithItem(Tree):
    __slots__ = ('test', 'as_kw', 'target')

    def __init__(self,
            test: Test,
            as_kw: _Optional[KwAs],
            target: _Optional[Target],
    ) -> None:
        super().__init__([
            test,
            as_kw,
            target,
        ])
        self.test = test
        self.as_kw = as_kw
        self.target = target

class WithItemAndOpComma(Tree):
    __slots__ = ('with_item', 'comma')

    def __init__(self,
            with_item: WithItem,
            comma: _Optional[OpComma],
    ) -> None:
        super().__init__([
            with_item,
            comma,
        ])
        self.with_item = with_item
        self.comma = comma

class WithStmt(Tree):
    __slots__ = ('with_kw', 'with_items', 'colon', 'suite')

    def __init__(self,
            with_kw: KwWith,
            with_items: _List[WithItemAndOpComma],
            colon: OpColon,
            suite: Suite,
    ) -> None:
        super().__init__([
            with_kw,
            TreeList(with_items),
            colon,
            suite,
        ])
        self.with_kw = with_kw
        self.with_items = with_items
        self.colon = colon
        self.suite = suite

class AsyncWithStmt(Tree):
    __slots__ = ('async_kw', 'with_stmt')

    def __init__(self,
            async_kw: KwAsync,
            with_stmt: WithStmt,
    ) -> None:
        super().__init__([
            async_kw,
            with_stmt,
        ])
        self.async_kw = async_kw
        self.with_stmt = with_stmt

class ComplexSuite(Tree):
    __slots__ = ('newline', 'indent', 'stmts', 'dedent')

    def __init__(self,
            newline: TokNewline,
            indent: TokIndent,
            stmts: _List[Stmt],
            dedent: TokDedent,
    ) -> None:
        super().__init__([
            newline,
            indent,
            TreeList(stmts),
            dedent,
        ])
        self.newline = newline
        self.indent = indent
        self.stmts = stmts
        self.dedent = dedent


class FileInput(Tree):
    __slots__ = ('stmts', 'eof')

    def __init__(self,
            stmts: _List[Stmt],
            eof: TokEof,
    ) -> None:
        super().__init__([
            TreeList(stmts),
            eof,
        ])
        self.stmts = stmts
        self.eof = eof

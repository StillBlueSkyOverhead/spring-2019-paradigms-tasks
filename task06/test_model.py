#!/usr/bin/env python3
import sys
import pytest
from model import *
from io import StringIO


def test_scope_in_dict():
    a = object()
    scope = Scope()
    scope['foo'] = a
    assert scope['foo'] is a


def test_scope_only_in_parent():
    a = object()
    grandparent = Scope()
    parent = Scope(grandparent)
    child = Scope(parent)
    grandparent['foo'] = a
    assert child['foo'] is a


def test_scope_not_found():
    a, b = object(), object()
    parent = Scope()
    child = Scope(parent)
    parent['foo'] = a
    child['bar'] = b
    with pytest.raises(KeyError, match='baz'):
        _ = child['baz']


def test_function_definition():
    scope = Scope()
    fun = Function(['bar'], [Number(1)])
    fun_def = FunctionDefinition('foo', fun)
    fun_def.evaluate(scope)
    assert scope['foo'] == fun


def test_conditional_true():
    scope = Scope()
    cond_true = Conditional(Number(1), [Number(2)], [Number(3)])
    assert cond_true.evaluate(scope) == Number(2)


def test_conditional_false():
    scope = Scope()
    cond_false = Conditional(Number(0), [Number(1)], [Number(2)])
    assert cond_false.evaluate(scope) == Number(2)


def test_print(capsys):
    scope = Scope()
    print_expr = Print(Number(239))
    assert print_expr.evaluate(scope) == Number(239)
    assert capsys.readouterr().out == '239\n'


def test_read(monkeypatch):
    monkeypatch.setattr(sys, 'stdin', StringIO('239'))
    scope = Scope()
    read_expr = Read('foo')
    assert read_expr.evaluate(scope) == Number(239)
    assert scope['foo'] == Number(239)


def test_function_call():
    scope = Scope()
    fun = Function(['foo'], [Reference('foo')])
    fun_call = FunctionCall(fun, [Number(239)])
    assert fun_call.evaluate(scope) == Number(239)


def test_reference():
    scope = Scope()
    scope['foo'] = Number(239)
    ref = Reference('foo')
    assert ref.evaluate(scope) == Number(239)


def test_binary_operation():
    scope = Scope()
    assert BinaryOperation(Number(300), '+', Number(22)).evaluate(scope)\
        == Number(322)
    assert BinaryOperation(Number(323), '-', Number(1)).evaluate(scope)\
        == Number(322)
    assert BinaryOperation(Number(3222), '/', Number(10)).evaluate(scope)\
        == Number(322)
    assert BinaryOperation(Number(322), '%', Number(10)).evaluate(scope)\
        == Number(2)
    assert BinaryOperation(Number(322), '==', Number(239)).evaluate(scope)\
        == Number(0)
    assert BinaryOperation(Number(322), '>=', Number(239)).evaluate(scope)\
        == Number(1)
    assert BinaryOperation(Number(0), '||', Number(1)).evaluate(scope)\
        == Number(1)
    assert BinaryOperation(Number(0), '&&', Number(1)).evaluate(scope)\
        == Number(0)


def test_unary_operation():
    scope = Scope()
    assert UnaryOperation('-', Number(239)).evaluate(scope) == Number(-239)
    assert UnaryOperation('!', Number(1)).evaluate(scope) == Number(0)


def test_factorial():
    factorial_fun = Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ])
    factorial_def = FunctionDefinition('fac', factorial_fun)
    scope = Scope()
    factorial_def.evaluate(scope)
    assert FunctionCall(Reference('fac'),
                        [Number(6)]).evaluate(scope) == Number(720)


if __name__ == "__main__":
    pytest.main()

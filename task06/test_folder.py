import pytest
from folder import *


def type_and_vars_eq(self, other):
    return type(self) == type(other) and vars(self) == vars(other)


@pytest.fixture(scope='function', autouse=True)
def mock_eq_method(monkeypatch):
    monkeypatch.setattr(ASTNode, '__eq__', type_and_vars_eq)


def test_bin_op_numbers():
    assert (fold_constants(BinaryOperation(Number(6), '*', Number(7)))
            == Number(42))
    assert (fold_constants(BinaryOperation(Number(13), '%', Number(5)))
            == Number(3))


def test_un_op_number():
    assert fold_constants(UnaryOperation('-', Number(42))) == Number(-42)
    assert fold_constants(UnaryOperation('!', Number(42))) == Number(0)


def test_zero_times_reference():
    assert (fold_constants(BinaryOperation(Number(0), '*', Reference('x')))
            == Number(0))
    assert (fold_constants(BinaryOperation(Reference('x'), '*', Number(0)))
            == Number(0))


def test_ref_minus_ref():
    assert fold_constants(
        BinaryOperation(Reference('x'), '-', Reference('x'))) == Number(0)


def test_ref_minus_ref_name_mismatch():
    assert (fold_constants(
        BinaryOperation(Reference('x'), '-', Reference('y')))
            == BinaryOperation(Reference('x'), '-', Reference('y')))


def test_end_to_end_from_readme():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)


if __name__ == '__main__':
    pytest.main()
#!/usr/bin/env python
#
# Unit tests for Mox.
#
# Copyright 2008 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
import io
import re
import sys
import unittest

from mox import mox

from . import mox_test_helper
from .test_helpers.subpackage.faraway import FarAwayClass

OS_LISTDIR = mox_test_helper.os.listdir


class ExpectedMethodCallsErrorTest(unittest.TestCase):
    """Test creation and string conversion of ExpectedMethodCallsError."""

    def test_at_least_one_method(self):
        self.assertRaises(ValueError, mox.ExpectedMethodCallsError, [])

    def test_one_error(self):
        method = mox.MockMethod("test_method", [], [], False)
        method(1, 2).AndReturn("output")
        e = mox.ExpectedMethodCallsError([method])
        self.assertEqual(
            "Verify: Expected methods never called:\n  0.  test_method(1, 2) -> 'output'",
            str(e),
        )

    def test_many_errors(self):
        method1 = mox.MockMethod("test_method", [], [], False)
        method1(1, 2).AndReturn("output")
        method2 = mox.MockMethod("test_method", [], [], False)
        method2(a=1, b=2, c="only named")
        method3 = mox.MockMethod("test_method2", [], [], False)
        method3().AndReturn(44)
        method4 = mox.MockMethod("test_method", [], [], False)
        method4(1, 2).AndReturn("output")
        e = mox.ExpectedMethodCallsError([method1, method2, method3, method4])
        self.assertEqual(
            "Verify: Expected methods never called:\n"
            "  0.  test_method(1, 2) -> 'output'\n"
            "  1.  test_method(a=1, b=2, c='only named') -> None\n"
            "  2.  test_method2() -> 44\n"
            "  3.  test_method(1, 2) -> 'output'",
            str(e),
        )


class OrTest(unittest.TestCase):
    """Test Or correctly chains Comparators."""

    def test_valid_or(self):
        """Or should be True if either Comparator returns True."""
        self.assertEqual(mox.Or(mox.IsA(dict), mox.IsA(str)), {})
        self.assertEqual(mox.Or(mox.IsA(dict), mox.IsA(str)), "test")
        self.assertEqual(mox.Or(mox.IsA(str), mox.IsA(str)), "test")

    def test_invalid_or(self):
        """Or should be False if both Comparators return False."""
        self.assertFalse(mox.Or(mox.IsA(dict), mox.IsA(str)) == 0)


class AndTest(unittest.TestCase):
    """Test And correctly chains Comparators."""

    def test_valid_and(self):
        """And should be True if both Comparators return True."""
        self.assertEqual(mox.And(mox.IsA(str), mox.IsA(str)), "1")

    def test_clause_one_fails(self):
        """And should be False if the first Comparator returns False."""

        self.assertNotEqual(mox.And(mox.IsA(dict), mox.IsA(str)), "1")

    def test_advanced_usage(self):
        """And should work with other Comparators.

        Note: this test is reliant on In and ContainsKeyValue.
        """
        test_dict = {"mock": "obj", "testing": "isCOOL"}
        self.assertTrue(mox.And(mox.In("testing"), mox.ContainsKeyValue("mock", "obj")) == test_dict)

    def test_advanced_usage_fails(self):
        """Note: this test is reliant on In and ContainsKeyValue."""
        test_dict = {"mock": "obj", "testing": "isCOOL"}
        self.assertFalse(mox.And(mox.In("NOTFOUND"), mox.ContainsKeyValue("mock", "obj")) == test_dict)


class FuncTest(unittest.TestCase):
    """Test Func correctly evaluates based upon true-false return."""

    def test_func_true_false_evaluation(self):
        """Should return True if the validating function returns True."""

        def equals_one(x):
            return x == 1

        def always_none(x):
            return None

        self.assertEqual(mox.Func(equals_one), 1)
        self.assertNotEqual(mox.Func(equals_one), 0)

        self.assertNotEqual(mox.Func(always_none), 1)
        self.assertNotEqual(mox.Func(always_none), 0)
        self.assertFalse(mox.Func(always_none) is None)

    def test_func_exception_propagation(self):
        """Exceptions within the validating function should propagate."""

        class TestException(Exception):
            pass

        def raise_exception_on_not_one(value):
            if value != 1:
                raise TestException
            else:
                return True

        self.assertEqual(mox.Func(raise_exception_on_not_one), 1)
        self.assertRaises(TestException, mox.Func(raise_exception_on_not_one).__eq__, 2)


class SameElementsAsTest(unittest.TestCase):
    """Test SameElementsAs correctly identifies sequences with same elements."""

    def test_sorted_lists(self):
        """Should return True if two lists are exactly equal."""
        self.assertTrue(mox.SameElementsAs([1, 2.0, "c"]) == [1, 2.0, "c"])

    def test_unsorted_lists(self):
        """Should return True if two lists are unequal but have same elements."""
        self.assertTrue(mox.SameElementsAs([1, 2.0, "c"]) == [2.0, "c", 1])

    def test_unhashable_lists(self):
        """Should return True if two lists have the same unhashable elements."""
        self.assertTrue(mox.SameElementsAs([{"a": 1}, {2: "b"}]) == [{2: "b"}, {"a": 1}])

    def test_empty_lists(self):
        """Should return True for two empty lists."""
        self.assertTrue(mox.SameElementsAs([]) == [])

    def test_unequal_lists(self):
        """Should return False if the lists are not equal."""
        self.assertFalse(mox.SameElementsAs([1, 2.0, "c"]) == [2.0, "c"])

    def test_unequal_unhashable_lists(self):
        """Should return False if two lists with unhashable elements are
        unequal."""
        self.assertFalse(mox.SameElementsAs([{"a": 1}, {2: "b"}]) == [{2: "b"}])

    def test_actual_is_not_a_sequence(self):
        """Should return False if the actual object is not a sequence."""
        self.assertFalse(mox.SameElementsAs([1]) == object())

    def test_one_unhashable_object_in_actual(self):
        """Store the entire iterator for a correct comparison.

        In a previous version of SameElementsAs, iteration stopped when an
        unhashable object was encountered and then was restarted, so the actual
        list
        appeared smaller than it was.
        """
        self.assertFalse(mox.SameElementsAs([1, 2]) == iter([{}, 1, 2]))


class ContainsKeyValueTest(unittest.TestCase):
    """Test ContainsKeyValue correctly identifies key/value pairs in a dict."""

    def test_valid_pair(self):
        """Should return True if the key value is in the dict."""
        self.assertTrue(mox.ContainsKeyValue("key", 1) == {"key": 1})

    def test_invalid_value(self):
        """Should return False if the value is not correct."""
        self.assertFalse(mox.ContainsKeyValue("key", 1) == {"key": 2})

    def test_invalid_key(self):
        """Should return False if they key is not in the dict."""
        self.assertFalse(mox.ContainsKeyValue("qux", 1) == {"key": 2})


class ContainsAttributeValueTest(unittest.TestCase):
    """Test ContainsAttributeValue correctly identifies properties in an
    object."""

    def setUp(self):
        """Create an object to test with."""

        class TestObject(object):
            key = 1

        self.test_object = TestObject()

    def test_valid_pair(self):
        """Should return True if the object has the key attribute and it
        matches."""
        self.assertTrue(mox.ContainsAttributeValue("key", 1) == self.test_object)

    def test_invalid_value(self):
        """Should return False if the value is not correct."""
        self.assertFalse(mox.ContainsKeyValue("key", 2) == self.test_object)

    def test_invalid_key(self):
        """Should return False if they the object doesn't have the property."""
        self.assertFalse(mox.ContainsKeyValue("qux", 1) == self.test_object)


class InTest(unittest.TestCase):
    """Test In correctly identifies a key in a list/dict"""

    def test_item_in_list(self):
        """Should return True if the item is in the list."""
        self.assertTrue(mox.In(1) == [1, 2, 3])

    def test_key_in_dict(self):
        """Should return True if the item is a key in a dict."""
        self.assertTrue(mox.In("test") == {"test": "module"})

    def test_item_in_tuple(self):
        """Should return True if the item is in the list."""
        self.assertTrue(mox.In(1) == (1, 2, 3))

    def test_tuple_in_tuple_of_tuples(self):
        self.assertTrue(mox.In((1, 2, 3)) == ((1, 2, 3), (1, 2)))

    def test_item_not_in_list(self):
        self.assertFalse(mox.In(1) == [2, 3])

    def test_tuple_not_in_tuple_of_tuples(self):
        self.assertFalse(mox.In((1, 2)) == ((1, 2, 3), (4, 5)))


class NotTest(unittest.TestCase):
    """Test Not correctly identifies False predicates."""

    def test_item_in_list(self):
        """Should return True if the item is NOT in the list."""
        self.assertTrue(mox.Not(mox.In(42)) == [1, 2, 3])

    def test_key_in_dict(self):
        """Should return True if the item is NOT a key in a dict."""
        self.assertTrue(mox.Not(mox.In("foo")) == {"key": 42})

    def test_invalid_key_with_not(self):
        """Should return False if they key is NOT in the dict."""
        self.assertTrue(mox.Not(mox.ContainsKeyValue("qux", 1)) == {"key": 2})


class StrContainsTest(unittest.TestCase):
    """Test StrContains correctly checks for substring occurrence of a
    parameter."""

    def test_valid_substring_at_start(self):
        """Should return True if the substring is at the start of the
        string."""
        self.assertTrue(mox.StrContains("hello") == "hello world")

    def test_valid_substring_in_middle(self):
        """Should return True if the substring is in the middle of the
        string."""
        self.assertTrue(mox.StrContains("lo wo") == "hello world")

    def test_valid_substring_at_end(self):
        """Should return True if the substring is at the end of the string."""
        self.assertTrue(mox.StrContains("ld") == "hello world")

    def test_invaild_substring(self):
        """Should return False if the substring is not in the string."""
        self.assertFalse(mox.StrContains("AAA") == "hello world")

    def test_multiple_matches(self):
        """Should return True if there are multiple occurances of substring."""
        self.assertTrue(mox.StrContains("abc") == "ababcabcabcababc")


class RegexTest(unittest.TestCase):
    """Test Regex correctly matches regular expressions."""

    def test_identify_bad_syntax_during_init(self):
        """The user should know immediately if a regex has bad syntax."""
        self.assertRaises(re.error, mox.Regex, "(a|b")

    def test_pattern_in_middle(self):
        """Should return True if the pattern matches at the middle of the
        string.

        This ensures that re.search is used (instead of re.find).
        """
        self.assertTrue(mox.Regex(r"a\s+b") == "x y z a b c")

    def test_non_match_pattern(self):
        """Should return False if the pattern does not match the string."""
        self.assertFalse(mox.Regex(r"a\s+b") == "x y z")

    def test_flags_passed_correctly(self):
        """Should return True as we pass IGNORECASE flag."""
        self.assertTrue(mox.Regex(r"A", re.IGNORECASE) == "a")

    def test_repr_without_flags(self):
        """repr should return the regular expression pattern."""
        self.assertEqual(repr(mox.Regex(rb"a\s+b")), r"<regular expression 'a\s+b'>")

    def test_repr_with_flags(self):
        """repr should return the regular expression pattern and flags."""
        self.assertEqual(repr(mox.Regex(rb"a\s+b", flags=4)), r"<regular expression 'a\s+b', flags=4>")


class IsTest(unittest.TestCase):
    """Verify Is correctly checks equality based upon identity, not value"""

    class AlwaysComparesTrue(object):
        def __eq__(self, other):
            return True

        def __cmp__(self, other):
            return 0

        def __ne__(self, other):
            return False

    def test_equality_valid(self):
        o1 = self.AlwaysComparesTrue()
        self.assertTrue(mox.Is(o1), o1)

    def test_equality_invalid(self):
        o1 = self.AlwaysComparesTrue()
        o2 = self.AlwaysComparesTrue()
        self.assertTrue(o1 == o2)
        # but...
        self.assertFalse(mox.Is(o1) == o2)

    def test_inequality_valid(self):
        o1 = self.AlwaysComparesTrue()
        o2 = self.AlwaysComparesTrue()
        self.assertTrue(mox.Is(o1) != o2)

    def test_inequality_invalid(self):
        o1 = self.AlwaysComparesTrue()
        self.assertFalse(mox.Is(o1) != o1)

    def test_equality_in_list_valid(self):
        o1 = self.AlwaysComparesTrue()
        o2 = self.AlwaysComparesTrue()
        isa_list = [mox.Is(o1), mox.Is(o2)]
        str_list = [o1, o2]
        self.assertTrue(isa_list == str_list)

    def test_equailty_in_list_invalid(self):
        o1 = self.AlwaysComparesTrue()
        o2 = self.AlwaysComparesTrue()
        isa_list = [mox.Is(o1), mox.Is(o2)]
        mixed_list = [o2, o1]
        self.assertFalse(isa_list == mixed_list)


class IsATest(unittest.TestCase):
    """Verify IsA correctly checks equality based upon class type, not
    value."""

    def test_equality_valid(self):
        """Verify that == correctly identifies objects of the same type."""
        self.assertTrue(mox.IsA(str) == "test")

    def test_equality_invalid(self):
        """Verify that == correctly identifies objects of different types."""
        self.assertFalse(mox.IsA(str) == 10)

    def test_inequality_valid(self):
        """Verify that != identifies objects of different type."""
        self.assertTrue(mox.IsA(str) != 10)

    def test_inequality_invalid(self):
        """Verify that != correctly identifies objects of the same type."""
        self.assertFalse(mox.IsA(str) != "test")

    def test_equality_in_list_valid(self):
        """Verify list contents are properly compared."""
        isa_list = [mox.IsA(str), mox.IsA(str)]
        str_list = ["abc", "def"]
        self.assertTrue(isa_list == str_list)

    def test_equailty_in_list_invalid(self):
        """Verify list contents are properly compared."""
        isa_list = [mox.IsA(str), mox.IsA(str)]
        mixed_list = ["abc", 123]
        self.assertFalse(isa_list == mixed_list)

    def test_special_types(self):
        """Verify that IsA can handle objects like cStringIO.StringIO."""
        isA = mox.IsA(io.StringIO())
        stringIO = io.StringIO()
        self.assertTrue(isA == stringIO)


class IsAlmostTest(unittest.TestCase):
    """Verify IsAlmost correctly checks equality of floating point numbers."""

    def test_equality_valid(self):
        """Verify that == correctly identifies nearly equivalent floats."""
        self.assertEqual(mox.IsAlmost(1.8999999999), 1.9)

    def test_equality_invalid(self):
        """Verify that == correctly identifies non-equivalent floats."""
        self.assertNotEqual(mox.IsAlmost(1.899), 1.9)

    def test_equality_with_places(self):
        """Verify that specifying places has the desired effect."""
        self.assertNotEqual(mox.IsAlmost(1.899), 1.9)
        self.assertEqual(mox.IsAlmost(1.899, places=2), 1.9)

    def test_non_numeric_types(self):
        """Verify that IsAlmost handles non-numeric types properly."""

        self.assertNotEqual(mox.IsAlmost(1.8999999999), "1.9")
        self.assertNotEqual(mox.IsAlmost("1.8999999999"), 1.9)
        self.assertNotEqual(mox.IsAlmost("1.8999999999"), "1.9")


class ValueRememberTest(unittest.TestCase):
    """Verify comparing argument against remembered value."""

    def test_value_equal(self):
        """Verify that value will compare to stored value."""
        value = mox.Value()
        value.store_value("hello world")
        self.assertEqual(value, "hello world")

    def test_no_value(self):
        """Verify that uninitialized value does not compare to "empty"
        values."""
        value = mox.Value()
        self.assertNotEqual(value, None)
        self.assertNotEqual(value, False)
        self.assertNotEqual(value, 0)
        self.assertNotEqual(value, "")
        self.assertNotEqual(value, ())
        self.assertNotEqual(value, [])
        self.assertNotEqual(value, {})
        self.assertNotEqual(value, object())
        self.assertNotEqual(value, set())

    def test_remember_value(self):
        """Verify that comparing against remember will store argument."""
        value = mox.Value()
        remember = mox.Remember(value)
        # value not yet stored.
        self.assertNotEqual(value, "hello world")

        # store value here.
        self.assertEqual(remember, "hello world")

        # compare against stored value.
        self.assertEqual(value, "hello world")


class MockMethodTest(unittest.TestCase):
    """Test class to verify that the MockMethod class is working correctly."""

    def setUp(self):
        self.expected_method = mox.MockMethod("test_method", [], [], False)(["original"])
        self.mock_method = mox.MockMethod("test_method", [self.expected_method], [], True)

    def test_name_attribute(self):
        """Should provide a __name__ attribute."""
        self.assertEqual("test_method", self.mock_method.__name__)

    def test_and_return_none_by_default(self):
        """Should return None by default."""
        return_value = self.mock_method(["original"])
        self.assertTrue(return_value is None)

    def test_and_return_value(self):
        """Should return a specificed return value."""
        expected_return_value = "test"
        self.expected_method.AndReturn(expected_return_value)
        return_value = self.mock_method(["original"])
        self.assertEqual(return_value, expected_return_value)

    def test_and_raise_exception(self):
        """Should raise a specified exception."""
        expected_exception = Exception("test exception")
        self.expected_method.AndRaise(expected_exception)
        self.assertRaises(Exception, self.mock_method)

    def test_with_side_effects(self):
        """Should call state modifier."""
        local_list = ["original"]

        def modifier(mutable_list):
            self.assertTrue(local_list is mutable_list)
            mutable_list[0] = "mutation"

        self.expected_method.WithSideEffects(modifier).AndReturn(1)
        self.mock_method(local_list)
        self.assertEqual("mutation", local_list[0])

    def test_with_returning_side_effects(self):
        """Should call state modifier and propagate its return value."""
        local_list = ["original"]
        expected_return = "expected_return"

        def modifier_with_return(mutable_list):
            self.assertTrue(local_list is mutable_list)
            mutable_list[0] = "mutation"
            return expected_return

        self.expected_method.WithSideEffects(modifier_with_return)
        actual_return = self.mock_method(local_list)
        self.assertEqual("mutation", local_list[0])
        self.assertEqual(expected_return, actual_return)

    def test_with_returning_side_effects_with_and_return(self):
        """Should call state modifier and ignore its return value."""
        local_list = ["original"]
        expected_return = "expected_return"
        unexpected_return = "unexpected_return"

        def modifier_with_return(mutable_list):
            self.assertTrue(local_list is mutable_list)
            mutable_list[0] = "mutation"
            return unexpected_return

        self.expected_method.WithSideEffects(modifier_with_return).AndReturn(expected_return)
        actual_return = self.mock_method(local_list)
        self.assertEqual("mutation", local_list[0])
        self.assertEqual(expected_return, actual_return)

    def test_equality_no_params_equal(self):
        """Methods with the same name and without params should be equal."""
        expected_method = mox.MockMethod("test_method", [], [], False)
        self.assertEqual(self.mock_method, expected_method)

    def test_equality_no_params_not_equal(self):
        """Methods with different names and without params should not be
        equal."""
        expected_method = mox.MockMethod("otherMethod", [], [], False)
        self.assertNotEqual(self.mock_method, expected_method)

    def test_equality_params_equal(self):
        """Methods with the same name and parameters should be equal."""
        params = [1, 2, 3]
        expected_method = mox.MockMethod("test_method", [], [], False)
        expected_method._params = params

        self.mock_method._params = params
        self.assertEqual(self.mock_method, expected_method)

    def test_equality_params_not_equal(self):
        """Methods with the same name and different params should not be
        equal."""
        expected_method = mox.MockMethod("test_method", [], [], False)
        expected_method._params = [1, 2, 3]

        self.mock_method._params = ["a", "b", "c"]
        self.assertNotEqual(self.mock_method, expected_method)

    def test_equality_named_params_equal(self):
        """Methods with the same name and same named params should be equal."""
        named_params = {"input1": "test", "input2": "params"}
        expected_method = mox.MockMethod("test_method", [], [], False)
        expected_method._named_params = named_params

        self.mock_method._named_params = named_params
        self.assertEqual(self.mock_method, expected_method)

    def test_equality_named_params_not_equal(self):
        """Methods with the same name and diffnamed params should not be
        equal."""
        expected_method = mox.MockMethod("test_method", [], [], False)
        expected_method._named_params = {"input1": "test", "input2": "params"}

        self.mock_method._named_params = {"input1": "test2", "input2": "params2"}
        self.assertNotEqual(self.mock_method, expected_method)

    def test_equality_wrong_type(self):
        """Method should not be equal to an object of a different type."""
        self.assertNotEqual(self.mock_method, "string?")

    def test_object_equality(self):
        """Equality of objects should work without a Comparator"""
        inst_a = TestClass()
        inst_b = TestClass()

        params = [
            inst_a,
        ]
        expected_method = mox.MockMethod("test_method", [], [], False)
        expected_method._params = params

        self.mock_method._params = [
            inst_b,
        ]
        self.assertEqual(self.mock_method, expected_method)

    def test_str_conversion(self):
        method = mox.MockMethod("f", [], [], False)
        method(1, 2, "st", n1=8, n2="st2")
        self.assertEqual(str(method), "f(1, 2, 'st', n1=8, n2='st2') -> None")

        method = mox.MockMethod("test_method", [], [], False)
        method(1, 2, "only positional")
        self.assertEqual(str(method), "test_method(1, 2, 'only positional') -> None")

        method = mox.MockMethod("test_method", [], [], False)
        method(a=1, b=2, c="only named")
        self.assertEqual(str(method), "test_method(a=1, b=2, c='only named') -> None")

        method = mox.MockMethod("test_method", [], [], False)
        method()
        self.assertEqual(str(method), "test_method() -> None")

        method = mox.MockMethod("test_method", [], [], False)
        method(x="only 1 parameter")
        self.assertEqual(str(method), "test_method(x='only 1 parameter') -> None")

        method = mox.MockMethod("test_method", [], [], False)
        method().AndReturn("return_value")
        self.assertEqual(str(method), "test_method() -> 'return_value'")

        method = mox.MockMethod("test_method", [], [], False)
        method().AndReturn(("a", {1: 2}))
        self.assertEqual(str(method), "test_method() -> ('a', {1: 2})")


class MockAnythingTest(unittest.TestCase):
    """Verify that the MockAnything class works as expected."""

    def setUp(self):
        self.mock_object = mox.MockAnything()

    def test_repr(self):
        """Calling repr on a MockAnything instance must work."""
        self.assertEqual("<MockAnything instance>", repr(self.mock_object))

    def test_can_mock_str(self):
        self.mock_object.__str__().AndReturn("foo")
        self.mock_object._Replay()
        actual = str(self.mock_object)
        self.mock_object._Verify()
        self.assertEqual("foo", actual)

    def test_setup_mode(self):
        """Verify the mock will accept any call."""
        self.mock_object.NonsenseCall()
        self.assertEqual(len(self.mock_object._expected_calls_queue), 1)

    def test_replay_with_expected_call(self):
        """Verify the mock replays method calls as expected."""
        self.mock_object.ValidCall()  # setup method call
        self.mock_object._Replay()  # start replay mode
        self.mock_object.ValidCall()  # make method call

    def test_replay_with_unexpected_call(self):
        """Unexpected method calls should raise UnexpectedMethodCallError."""
        self.mock_object.ValidCall()  # setup method call
        self.mock_object._Replay()  # start replay mode
        self.assertRaises(mox.UnexpectedMethodCallError, self.mock_object.other_valid_call)

    def test_verify_with_complete_replay(self):
        """Verify should not raise an exception for a valid replay."""
        self.mock_object.ValidCall()  # setup method call
        self.mock_object._Replay()  # start replay mode
        self.mock_object.ValidCall()  # make method call
        self.mock_object._Verify()

    def test_verify_with_incomplete_replay(self):
        """Verify should raise an exception if the replay was not complete."""
        self.mock_object.ValidCall()  # setup method call
        self.mock_object._Replay()  # start replay mode
        # ValidCall() is never made
        self.assertRaises(mox.ExpectedMethodCallsError, self.mock_object._Verify)

    def test_special_class_method(self):
        """Verify should not raise an exception when special methods are
        used."""
        self.mock_object[1].AndReturn(True)
        self.mock_object._Replay()
        returned_val = self.mock_object[1]
        self.assertTrue(returned_val)
        self.mock_object._Verify()

    def test_nonzero(self):
        """You should be able to use the mock object in an if."""
        self.mock_object._Replay()
        if self.mock_object:
            pass

    def test_not_none(self):
        """Mock should be comparable to None."""
        self.mock_object._Replay()
        if self.mock_object is not None:
            pass

        if self.mock_object is None:
            pass

    def test_equal(self):
        """A mock should be able to compare itself to another object."""
        self.mock_object._Replay()
        self.assertEqual(self.mock_object, self.mock_object)

    def test_equal_mock_failure(self):
        """Verify equals identifies unequal objects."""
        self.mock_object.SillyCall()
        self.mock_object._Replay()
        self.assertNotEqual(self.mock_object, mox.MockAnything())

    def test_equal_instance_failure(self):
        """Verify equals identifies that objects are different instances."""
        self.mock_object._Replay()
        self.assertNotEqual(self.mock_object, TestClass())

    def test_not_equal(self):
        """Verify not equals works."""
        self.mock_object._Replay()
        self.assertFalse(self.mock_object != self.mock_object)

    def test_nested_mock_calls_recorded_serially(self):
        """Test that nested calls work when recorded serially."""
        self.mock_object.CallInner().AndReturn(1)
        self.mock_object.CallOuter(1)
        self.mock_object._Replay()

        self.mock_object.CallOuter(self.mock_object.CallInner())

        self.mock_object._Verify()

    def test_nested_mock_calls_recorded_nested(self):
        """Test that nested cals work when recorded in a nested fashion."""
        self.mock_object.CallOuter(self.mock_object.CallInner().AndReturn(1))
        self.mock_object._Replay()

        self.mock_object.CallOuter(self.mock_object.CallInner())

        self.mock_object._Verify()

    def test_is_callable(self):
        """Test that MockAnything can even mock a simple callable.

        This is handy for "stubbing out" a method in a module with a mock, and
        verifying that it was called.
        """
        self.mock_object().AndReturn("mox0rd")
        self.mock_object._Replay()

        self.assertEqual("mox0rd", self.mock_object())

        self.mock_object._Verify()

    def test_is_reprable(self):
        """Test that MockAnythings can be repr'd without causing a failure."""
        self.assertIn("MockAnything", repr(self.mock_object))


class MethodCheckerTest(unittest.TestCase):
    """Tests MockMethod's use of MethodChecker method."""

    def test_no_parameters(self):
        method = mox.MockMethod("no_parameters", [], [], False, CheckCallTestClass.no_parameters)
        method()
        self.assertRaises(AttributeError, method, 1)
        self.assertRaises(AttributeError, method, 1, 2)
        self.assertRaises(AttributeError, method, a=1)
        self.assertRaises(AttributeError, method, 1, b=2)

    def test_one_parameter(self):
        method = mox.MockMethod("one_parameter", [], [], False, CheckCallTestClass.one_parameter)
        self.assertRaises(AttributeError, method)
        method(1)
        method(a=1)
        self.assertRaises(AttributeError, method, b=1)
        self.assertRaises(AttributeError, method, 1, 2)
        self.assertRaises(AttributeError, method, 1, a=2)
        self.assertRaises(AttributeError, method, 1, b=2)

    def test_two_parameters(self):
        method = mox.MockMethod("two_parameters", [], [], False, CheckCallTestClass.two_parameters)
        self.assertRaises(AttributeError, method)
        self.assertRaises(AttributeError, method, 1)
        self.assertRaises(AttributeError, method, a=1)
        self.assertRaises(AttributeError, method, b=1)
        method(1, 2)
        method(1, b=2)
        method(a=1, b=2)
        method(b=2, a=1)
        self.assertRaises(AttributeError, method, b=2, c=3)
        self.assertRaises(AttributeError, method, a=1, b=2, c=3)
        self.assertRaises(AttributeError, method, 1, 2, 3)
        self.assertRaises(AttributeError, method, 1, 2, 3, 4)
        self.assertRaises(AttributeError, method, 3, a=1, b=2)

    def test_one_default_value(self):
        method = mox.MockMethod("one_default_value", [], [], False, CheckCallTestClass.one_default_value)
        method()
        method(1)
        method(a=1)
        self.assertRaises(AttributeError, method, b=1)
        self.assertRaises(AttributeError, method, 1, 2)
        self.assertRaises(AttributeError, method, 1, a=2)
        self.assertRaises(AttributeError, method, 1, b=2)

    def test_two_default_values(self):
        method = mox.MockMethod("two_default_values", [], [], False, CheckCallTestClass.two_default_values)
        self.assertRaises(AttributeError, method)
        self.assertRaises(AttributeError, method, c=3)
        self.assertRaises(AttributeError, method, 1)
        self.assertRaises(AttributeError, method, 1, d=4)
        self.assertRaises(AttributeError, method, 1, d=4, c=3)
        method(1, 2)
        method(a=1, b=2)
        method(1, 2, 3)
        method(1, 2, 3, 4)
        method(1, 2, c=3)
        method(1, 2, c=3, d=4)
        method(1, 2, d=4, c=3)
        method(d=4, c=3, a=1, b=2)
        self.assertRaises(AttributeError, method, 1, 2, 3, 4, 5)
        self.assertRaises(AttributeError, method, 1, 2, e=9)
        self.assertRaises(AttributeError, method, a=1, b=2, e=9)

    def test_args(self):
        method = mox.MockMethod("args", [], [], False, CheckCallTestClass.args)
        self.assertRaises(AttributeError, method)
        self.assertRaises(AttributeError, method, 1)
        method(1, 2)
        method(a=1, b=2)
        method(1, 2, 3)
        method(1, 2, 3, 4)
        self.assertRaises(AttributeError, method, 1, 2, a=3)
        self.assertRaises(AttributeError, method, 1, 2, c=3)

    def test_kwargs(self):
        method = mox.MockMethod("kwargs", [], [], False, CheckCallTestClass.kwargs)
        self.assertRaises(AttributeError, method)
        method(1)
        method(1, 2)
        method(a=1, b=2)
        method(b=2, a=1)
        self.assertRaises(AttributeError, method, 1, 2, 3)
        self.assertRaises(AttributeError, method, 1, 2, a=3)
        method(1, 2, c=3)
        method(a=1, b=2, c=3)
        method(c=3, a=1, b=2)
        method(a=1, b=2, c=3, d=4)
        self.assertRaises(AttributeError, method, 1, 2, 3, 4)

    def test_args_and_kwargs(self):
        method = mox.MockMethod("args_and_kwargs", [], [], False, CheckCallTestClass.args_and_kwargs)
        self.assertRaises(AttributeError, method)
        method(1)
        method(1, 2)
        method(1, 2, 3)
        method(a=1)
        method(1, b=2)
        self.assertRaises(AttributeError, method, 1, a=2)
        method(b=2, a=1)
        method(c=3, b=2, a=1)
        method(1, 2, c=3)

    def test_far_away_class_with_instantiated_object(self):
        obj = FarAwayClass()
        method = mox.MockMethod("distant_method", [], [], False, obj.distant_method)
        self.assertRaises(AttributeError, method, 1)
        self.assertRaises(AttributeError, method, a=1)
        self.assertRaises(AttributeError, method, b=1)
        self.assertRaises(AttributeError, method, 1, 2)
        self.assertRaises(AttributeError, method, 1, b=2)
        self.assertRaises(AttributeError, method, a=1, b=2)
        self.assertRaises(AttributeError, method, b=2, a=1)
        self.assertRaises(AttributeError, method, b=2, c=3)
        self.assertRaises(AttributeError, method, a=1, b=2, c=3)
        self.assertRaises(AttributeError, method, 1, 2, 3)
        self.assertRaises(AttributeError, method, 1, 2, 3, 4)
        self.assertRaises(AttributeError, method, 3, a=1, b=2)
        method()


class CheckCallTestClass(object):
    def no_parameters(self):
        pass

    def one_parameter(self, a):
        pass

    def two_parameters(self, a, b):
        pass

    def one_default_value(self, a=1):
        pass

    def two_default_values(self, a, b, c=1, d=2):
        pass

    def args(self, a, b, *args):
        pass

    def kwargs(self, a, b=2, **kwargs):
        pass

    def args_and_kwargs(self, a, *args, **kwargs):
        pass


class MockObjectTest(unittest.TestCase):
    """Verify that the MockObject class works as exepcted."""

    def setUp(self):
        self.mock_object = mox.MockObject(TestClass)

    def test_description(self):
        self.assertEqual(self.mock_object._description, "TestClass")

        mock_object = mox.MockObject(FarAwayClass.distant_method)
        self.assertEqual(mock_object._description, "FarAwayClass.distant_method")

        mock_object = mox.MockObject(mox_test_helper.MyTestFunction)
        self.assertEqual(mock_object._description, "function test.mox_test_helper.MyTestFunction")

    def test_description_mocked_object(self):
        obj = FarAwayClass()
        mock = mox.Mox()

        mock.StubOutWithMock(obj, "distant_method")
        obj.distant_method().AndReturn(True)

        mock.ReplayAll()
        self.assertEqual(obj.distant_method._description, "FarAwayClass.distant_method")

    def test_description_module_function(self):
        mock = mox.Mox()

        mock.StubOutWithMock(mox_test_helper, "MyTestFunction")
        mox_test_helper.MyTestFunction(one=1, two=2).AndReturn(True)

        mock.ReplayAll()
        self.assertEqual(
            mox_test_helper.MyTestFunction._description,
            "function test.mox_test_helper.MyTestFunction",
        )

    def test_description_mocked_class(self):
        obj = FarAwayClass()
        mock = mox.Mox()

        mock.StubOutWithMock(FarAwayClass, "distant_method")
        obj.distant_method().AndReturn(True)

        mock.ReplayAll()
        self.assertEqual(obj.distant_method._description, "FarAwayClass.distant_method")

    def test_description_class_method(self):
        obj = mox_test_helper.SpecialClass()
        mock = mox.Mox()

        mock.StubOutWithMock(mox_test_helper.SpecialClass, "class_method")
        mox_test_helper.SpecialClass.class_method().AndReturn(True)

        mock.ReplayAll()
        self.assertEqual(obj.class_method._description, "SpecialClass.class_method")

    def test_description_static_method_mock_class(self):
        mock = mox.Mox()

        mock.StubOutWithMock(mox_test_helper.SpecialClass, "static_method")
        mox_test_helper.SpecialClass.static_method().AndReturn(True)

        mock.ReplayAll()
        self.assertIn(
            mox_test_helper.SpecialClass.static_method._description,
            ["SpecialClass.static_method", "function test.mox_test_helper.static_method"],
        )

    def test_description_static_method_mock_instance(self):
        obj = mox_test_helper.SpecialClass()
        mock = mox.Mox()

        mock.StubOutWithMock(obj, "static_method")
        obj.static_method().AndReturn(True)

        mock.ReplayAll()
        self.assertIn(
            obj.static_method._description,
            ["SpecialClass.static_method", "function test.mox_test_helper.static_method"],
        )

    def test_setup_mode_with_valid_call(self):
        """Verify the mock object properly mocks a basic method call."""
        self.mock_object.valid_call()
        self.assertEqual(len(self.mock_object._expected_calls_queue), 1)

    def test_setup_mode_with_invalid_call(self):
        """UnknownMethodCallError should be raised if a non-member method is
        called."""
        # Note: assertRaises does not catch exceptions thrown by MockObject's
        # __getattr__
        try:
            self.mock_object.invalid_call()
            self.fail("No exception thrown, expected UnknownMethodCallError")
        except mox.UnknownMethodCallError:
            pass
        except Exception:
            self.fail("Wrong exception type thrown, expected UnknownMethodCallError")

    def test_replay_with_invalid_call(self):
        """UnknownMethodCallError should be raised if a non-member method is
        called."""
        self.mock_object.valid_call()  # setup method call
        self.mock_object._Replay()  # start replay mode
        # Note: assertRaises does not catch exceptions thrown by MockObject's
        # __getattr__
        try:
            self.mock_object.invalid_call()
            self.fail("No exception thrown, expected UnknownMethodCallError")
        except mox.UnknownMethodCallError:
            pass
        except Exception:
            self.fail("Wrong exception type thrown, expected UnknownMethodCallError")

    def test_is_instance(self):
        """Mock should be able to pass as an instance of the mocked class."""
        self.assertIsInstance(self.mock_object, TestClass)

    def test_find_valid_methods(self):
        """Mock should be able to mock all public methods."""
        self.assertIn("valid_call", self.mock_object._known_methods)
        self.assertIn("other_valid_call", self.mock_object._known_methods)
        self.assertIn("my_class_method", self.mock_object._known_methods)
        self.assertIn("my_static_method", self.mock_object._known_methods)
        self.assertIn("_protected_call", self.mock_object._known_methods)
        self.assertNotIn("__private_call", self.mock_object._known_methods)
        self.assertIn("_TestClass__private_call", self.mock_object._known_methods)

    def test_finds_superclass_methods(self):
        """Mock should be able to mock superclasses methods."""
        self.mock_object = mox.MockObject(ChildClass)
        self.assertIn("valid_call", self.mock_object._known_methods)
        self.assertIn("other_valid_call", self.mock_object._known_methods)
        self.assertIn("my_class_method", self.mock_object._known_methods)
        self.assertIn("child_valid_call", self.mock_object._known_methods)

    def test_access_class_variables(self):
        """Class variables should be accessible through the mock."""
        self.assertIn("SOME_CLASS_VAR", self.mock_object._known_vars)
        self.assertIn("SOME_CLASS_SET", self.mock_object._known_vars)
        self.assertIn("_PROTECTED_CLASS_VAR", self.mock_object._known_vars)
        self.assertEqual("test_value", self.mock_object.SOME_CLASS_VAR)
        self.assertEqual({"a", "b", "c"}, self.mock_object.SOME_CLASS_SET)

    def test_equal(self):
        """A mock should be able to compare itself to another object."""
        self.mock_object._Replay()
        self.assertEqual(self.mock_object, self.mock_object)

    def test_equal_mock_failure(self):
        """Verify equals identifies unequal objects."""
        self.mock_object.valid_call()
        self.mock_object._Replay()
        self.assertNotEqual(self.mock_object, mox.MockObject(TestClass))

    def test_equal_instance_failure(self):
        """Verify equals identifies that objects are different instances."""
        self.mock_object._Replay()
        self.assertNotEqual(self.mock_object, TestClass())

    def test_not_equal(self):
        """Verify not equals works."""
        self.mock_object._Replay()
        self.assertFalse(self.mock_object != self.mock_object)

    def test_mock_set_item__expected_set_item__success(self):
        """Test that __setitem__() gets mocked in Dummy.

        In this test, _Verify() succeeds.
        """
        dummy = mox.MockObject(TestClass)
        dummy["X"] = "Y"

        dummy._Replay()

        dummy["X"] = "Y"

        dummy._Verify()

    def test_mock_set_item__expected_set_item__no_success(self):
        """Test that __setitem__() gets mocked in Dummy.

        In this test, _Verify() fails.
        """
        dummy = mox.MockObject(TestClass)
        dummy["X"] = "Y"

        dummy._Replay()

        # NOT doing dummy['X'] = 'Y'

        self.assertRaises(mox.ExpectedMethodCallsError, dummy._Verify)

    def test_mock_set_item__expected_no_set_item__success(self):
        """Test that __setitem__() gets mocked in Dummy."""
        dummy = mox.MockObject(TestClass)
        # NOT doing dummy['X'] = 'Y'

        dummy._Replay()

        def call():
            dummy["X"] = "Y"

        self.assertRaises(mox.UnexpectedMethodCallError, call)

    def test_mock_set_item__expected_no_set_item__no_success(self):
        """Test that __setitem__() gets mocked in Dummy.

        In this test, _Verify() fails.
        """
        dummy = mox.MockObject(TestClass)
        # NOT doing dummy['X'] = 'Y'

        dummy._Replay()

        # NOT doing dummy['X'] = 'Y'

        dummy._Verify()

    def test_mock_set_item__expected_set_item__nonmatching_parameters(self):
        """Test that __setitem__() fails if other parameters are expected."""
        dummy = mox.MockObject(TestClass)
        dummy["X"] = "Y"

        dummy._Replay()

        def call():
            dummy["wrong"] = "Y"

        self.assertRaises(mox.UnexpectedMethodCallError, call)

        self.assertRaises(mox.SwallowedExceptionError, dummy._Verify)

    def test_mock_set_item__with_sub_class_of_new_style_class(self):
        class NewStyleTestClass(object):
            def __init__(self):
                self.my_dict = {}

            def __setitem__(self, key, value):
                self.my_dict[key], value

        class TestSubClass(NewStyleTestClass):
            pass

        dummy = mox.MockObject(TestSubClass)
        dummy[1] = 2
        dummy._Replay()
        dummy[1] = 2
        dummy._Verify()

    def test_mock_get_item__expected_get_item__success(self):
        """Test that __getitem__() gets mocked in Dummy.

        In this test, _Verify() succeeds.
        """
        dummy = mox.MockObject(TestClass)
        dummy["X"].AndReturn("value")

        dummy._Replay()

        self.assertEqual(dummy["X"], "value")

        dummy._Verify()

    def test_mock_get_item__expected_get_item__no_success(self):
        """Test that __getitem__() gets mocked in Dummy.

        In this test, _Verify() fails.
        """
        dummy = mox.MockObject(TestClass)
        dummy["X"].AndReturn("value")

        dummy._Replay()

        # NOT doing dummy['X']

        self.assertRaises(mox.ExpectedMethodCallsError, dummy._Verify)

    def test_mock_get_item__expected_no_get_item__no_success(self):
        """Test that __getitem__() gets mocked in Dummy."""
        dummy = mox.MockObject(TestClass)
        # NOT doing dummy['X']

        dummy._Replay()

        def call():
            return dummy["X"]

        self.assertRaises(mox.UnexpectedMethodCallError, call)

    def test_mock_get_item__expected_get_item__nonmatching_parameters(self):
        """Test that __getitem__() fails if other parameters are expected."""
        dummy = mox.MockObject(TestClass)
        dummy["X"].AndReturn("value")

        dummy._Replay()

        def call():
            return dummy["wrong"]

        self.assertRaises(mox.UnexpectedMethodCallError, call)

        self.assertRaises(mox.SwallowedExceptionError, dummy._Verify)

    def test_mock_get_item__with_sub_class_of_new_style_class(self):
        class NewStyleTestClass(object):
            def __getitem__(self, key):
                return {1: "1", 2: "2"}[key]

        class TestSubClass(NewStyleTestClass):
            pass

        dummy = mox.MockObject(TestSubClass)
        dummy[1].AndReturn("3")

        dummy._Replay()
        self.assertEqual("3", dummy.__getitem__(1))
        dummy._Verify()

    def test_mock_iter__expected_iter__success(self):
        """Test that __iter__() gets mocked in Dummy.

        In this test, _Verify() succeeds.
        """
        dummy = mox.MockObject(TestClass)
        iter(dummy).AndReturn(iter(["X", "Y"]))

        dummy._Replay()

        self.assertEqual([x for x in dummy], ["X", "Y"])

        dummy._Verify()

    def test_mock_contains__expected_contains__success(self):
        """Test that __contains__ gets mocked in Dummy.

        In this test, _Verify() succeeds.
        """
        dummy = mox.MockObject(TestClass)
        dummy.__contains__("X").AndReturn(True)

        dummy._Replay()

        self.assertIn("X", dummy)

        dummy._Verify()

    def test_mock_contains__expected_contains__no_success(self):
        """Test that __contains__() gets mocked in Dummy.

        In this test, _Verify() fails.
        """
        dummy = mox.MockObject(TestClass)
        dummy.__contains__("X").AndReturn("True")

        dummy._Replay()

        # NOT doing 'X' in dummy

        self.assertRaises(mox.ExpectedMethodCallsError, dummy._Verify)

    def test_mock_contains__expected_contains__nonmatching_parameter(self):
        """Test that __contains__ fails if other parameters are expected."""
        dummy = mox.MockObject(TestClass)
        dummy.__contains__("X").AndReturn(True)

        dummy._Replay()

        def call():
            return "Y" in dummy

        self.assertRaises(mox.UnexpectedMethodCallError, call)

        self.assertRaises(mox.SwallowedExceptionError, dummy._Verify)

    def test_mock_iter__expected_iter__no_success(self):
        """Test that __iter__() gets mocked in Dummy.

        In this test, _Verify() fails.
        """
        dummy = mox.MockObject(TestClass)
        iter(dummy).AndReturn(iter(["X", "Y"]))

        dummy._Replay()

        # NOT doing self.assertEqual([x for x in dummy], ['X', 'Y'])

        self.assertRaises(mox.ExpectedMethodCallsError, dummy._Verify)

    def test_mock_iter__expected_no_iter__no_success(self):
        """Test that __iter__() gets mocked in Dummy."""
        dummy = mox.MockObject(TestClass)
        # NOT doing iter(dummy)

        dummy._Replay()

        def call():
            return [x for x in dummy]

        self.assertRaises(mox.UnexpectedMethodCallError, call)

    def test_mock_iter__expected_get_item__success(self):
        """Test that __iter__() gets mocked in Dummy using getitem."""
        dummy = mox.MockObject(SubscribtableNonIterableClass)
        dummy[0].AndReturn("a")
        dummy[1].AndReturn("b")
        dummy[2].AndRaise(IndexError)

        dummy._Replay()
        self.assertEqual(["a", "b"], [x for x in dummy])
        dummy._Verify()

    def test_mock_iter__expected_no_get_item__no_success(self):
        """Test that __iter__() gets mocked in Dummy using getitem."""
        dummy = mox.MockObject(SubscribtableNonIterableClass)
        # NOT doing dummy[index]

        dummy._Replay()

        def function():
            return [x for x in dummy]

        self.assertRaises(mox.UnexpectedMethodCallError, function)

    def test_mock_get_iter__with_sub_class_of_new_style_class(self):
        class NewStyleTestClass(object):
            def __iter__(self):
                return iter([1, 2, 3])

        class TestSubClass(NewStyleTestClass):
            pass

        dummy = mox.MockObject(TestSubClass)
        iter(dummy).AndReturn(iter(["a", "b"]))
        dummy._Replay()
        self.assertEqual(["a", "b"], [x for x in dummy])
        dummy._Verify()

    def test_instantiation_with_additional_attributes(self):
        mock_object = mox.MockObject(TestClass, attrs={"attr1": "value"})
        self.assertEqual(mock_object.attr1, "value")

    def test_cant_override_methods_with_attributes(self):
        self.assertRaises(ValueError, mox.MockObject, TestClass, attrs={"valid_call": "value"})

    def test_cant_mock_non_public_attributes(self):
        self.assertRaises(
            mox.PrivateAttributeError,
            mox.MockObject,
            TestClass,
            attrs={"_protected": "value"},
        )
        self.assertRaises(
            mox.PrivateAttributeError,
            mox.MockObject,
            TestClass,
            attrs={"__private": "value"},
        )


class MoxTest(unittest.TestCase):
    """Verify Mox works correctly."""

    def setUp(self):
        self.mox = mox.Mox()

    def test_create_object(self):
        """Mox should create a mock object."""
        self.mox.CreateMock(TestClass)

    def test_create_object_using_simple_imported_module(self):
        """Mox should create a mock object for a class from a module imported
        using a simple 'import module' statement"""
        self.mox.CreateMock(mox_test_helper.ExampleClass)

    def test_create_object_using_simple_imported_module_class_method(self):
        """Mox should create a mock object for a class from a module imported
        using a simple 'import module' statement"""
        example_obj = self.mox.CreateMock(mox_test_helper.ExampleClass)

        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "class_method")
        mox_test_helper.ExampleClass.class_method().AndReturn(example_obj)

        def call_helper_class_method():
            return mox_test_helper.ExampleClass.class_method()

        self.mox.ReplayAll()
        expected_obj = call_helper_class_method()
        self.mox.VerifyAll()

        self.assertEqual(expected_obj, example_obj)

    def test_create_mock_of_type(self):
        self.mox.CreateMock(type)

    def test_create_mock_with_bogus_attr(self):
        class BogusAttrClass(object):
            __slots__ = ("no_such_attr",)

        foo = BogusAttrClass()
        self.mox.CreateMock(foo)

    def test_verify_object_with_complete_replay(self):
        """Mox should replay and verify all objects it created."""
        mock_obj = self.mox.CreateMock(TestClass)
        mock_obj.valid_call()
        mock_obj.valid_call_with_args(mox.IsA(TestClass))
        self.mox.ReplayAll()
        mock_obj.valid_call()
        mock_obj.valid_call_with_args(TestClass("some_value"))
        self.mox.VerifyAll()

    def test_verify_object_with_incomplete_replay(self):
        """Mox should raise an exception if a mock didn't replay completely."""
        mock_obj = self.mox.CreateMock(TestClass)
        mock_obj.valid_call()
        self.mox.ReplayAll()
        # valid_call() is never made
        self.assertRaises(mox.ExpectedMethodCallsError, self.mox.VerifyAll)

    def test_entire_workflow(self):
        """Test the whole work flow."""
        mock_obj = self.mox.CreateMock(TestClass)
        mock_obj.valid_call().AndReturn("yes")
        self.mox.ReplayAll()

        ret_val = mock_obj.valid_call()
        self.assertEqual("yes", ret_val)
        self.mox.VerifyAll()

    def test_signature_matching_with_comparator_as_first_arg(self):
        """Test that the first argument can be a comparator."""

        def verify_len(val):
            """This will raise an exception when not given a list.

            This exception will be raised when trying to infer/validate the
            method signature.
            """
            return len(val) != 1

        mock_obj = self.mox.CreateMock(TestClass)
        # This intentionally does not name the 'nine' param, so it triggers
        # deeper inspection.
        mock_obj.method_with_args(mox.Func(verify_len), mox.IgnoreArg(), None)
        self.mox.ReplayAll()

        mock_obj.method_with_args([1, 2], "foo", None)

        self.mox.VerifyAll()

    def test_callable_object(self):
        """Test recording calls to a callable object works."""
        mock_obj = self.mox.CreateMock(CallableClass)
        mock_obj("foo").AndReturn("qux")
        self.mox.ReplayAll()

        ret_val = mock_obj("foo")
        self.assertEqual("qux", ret_val)
        self.mox.VerifyAll()

    def test_inherited_callable_object(self):
        """Test recording calls to an object inheriting from a callable
        object."""
        mock_obj = self.mox.CreateMock(InheritsFromCallable)
        mock_obj("foo").AndReturn("qux")
        self.mox.ReplayAll()

        ret_val = mock_obj("foo")
        self.assertEqual("qux", ret_val)
        self.mox.VerifyAll()

    def test_call_on_non_callable_object(self):
        """Test that you cannot call a non-callable object."""

        class NonCallable(object):
            pass

        noncallable = NonCallable()
        self.assertNotIn("__call__", dir(noncallable))
        mock_obj = self.mox.CreateMock(noncallable)
        self.assertRaises(TypeError, mock_obj)

    def test_callable_object_with_bad_call(self):
        """Test verifying calls to a callable object works."""
        mock_obj = self.mox.CreateMock(CallableClass)
        mock_obj("foo").AndReturn("qux")
        self.mox.ReplayAll()

        self.assertRaises(mox.UnexpectedMethodCallError, mock_obj, "ZOOBAZ")

    def test_callable_object_verifies_signature(self):
        mock_obj = self.mox.CreateMock(CallableClass)
        # Too many arguments
        self.assertRaises(AttributeError, mock_obj, "foo", "bar")

    def test_unordered_group(self):
        """Test that using one unordered group works."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Method(1).InAnyOrder()
        mock_obj.Method(2).InAnyOrder()
        self.mox.ReplayAll()

        mock_obj.Method(2)
        mock_obj.Method(1)

        self.mox.VerifyAll()

    def test_unordered_groups_inline(self):
        """Unordered groups should work in the context of ordered calls."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(1).InAnyOrder()
        mock_obj.Method(2).InAnyOrder()
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        mock_obj.Method(2)
        mock_obj.Method(1)
        mock_obj.Close()

        self.mox.VerifyAll()

    def test_multiple_unorderd_groups(self):
        """Multiple unoreded groups should work."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Method(1).InAnyOrder()
        mock_obj.Method(2).InAnyOrder()
        mock_obj.Foo().InAnyOrder("group2")
        mock_obj.Bar().InAnyOrder("group2")
        self.mox.ReplayAll()

        mock_obj.Method(2)
        mock_obj.Method(1)
        mock_obj.Bar()
        mock_obj.Foo()

        self.mox.VerifyAll()

    def test_multiple_unorderd_groups_out_of_order(self):
        """Multiple unordered groups should maintain external order"""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Method(1).InAnyOrder()
        mock_obj.Method(2).InAnyOrder()
        mock_obj.Foo().InAnyOrder("group2")
        mock_obj.Bar().InAnyOrder("group2")
        self.mox.ReplayAll()

        mock_obj.Method(2)
        self.assertRaises(mox.UnexpectedMethodCallError, mock_obj.Bar)

    def test_unordered_group_with_return_value(self):
        """Unordered groups should work with return values."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(1).InAnyOrder().AndReturn(9)
        mock_obj.Method(2).InAnyOrder().AndReturn(10)
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        actual_two = mock_obj.Method(2)
        actual_one = mock_obj.Method(1)
        mock_obj.Close()

        self.assertEqual(9, actual_one)
        self.assertEqual(10, actual_two)

        self.mox.VerifyAll()

    def test_unordered_group_with_comparator(self):
        """Unordered groups should work with comparators"""

        def verify_one(cmd):
            if not isinstance(cmd, str):
                self.fail("Unexpected type passed to comparator: " + str(cmd))
            return cmd == "test"

        def verify_two(cmd):
            return True

        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Foo(["test"], mox.Func(verify_one), bar=1).InAnyOrder().AndReturn("yes test")
        mock_obj.Foo(["test"], mox.Func(verify_two), bar=1).InAnyOrder().AndReturn("anything")

        self.mox.ReplayAll()

        mock_obj.Foo(["test"], "anything", bar=1)
        mock_obj.Foo(["test"], "test", bar=1)

        self.mox.VerifyAll()

    def test_multiple_times(self):
        """Test if MultipleTimesGroup works."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Method(1).MultipleTimes().AndReturn(9)
        mock_obj.Method(2).AndReturn(10)
        mock_obj.Method(3).MultipleTimes().AndReturn(42)
        self.mox.ReplayAll()

        actual_one = mock_obj.Method(1)
        second_one = mock_obj.Method(1)  # This tests MultipleTimes.
        actual_two = mock_obj.Method(2)
        actual_three = mock_obj.Method(3)
        mock_obj.Method(3)
        mock_obj.Method(3)

        self.mox.VerifyAll()

        self.assertEqual(9, actual_one)

        # Repeated calls should return same number.
        self.assertEqual(9, second_one)
        self.assertEqual(10, actual_two)
        self.assertEqual(42, actual_three)

    def test_multiple_times_using_is_a_parameter(self):
        """Test if MultipleTimesGroup works with a IsA parameter."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(mox.IsA(str)).MultipleTimes("IsA").AndReturn(9)
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        actual_one = mock_obj.Method("1")
        second_one = mock_obj.Method("2")  # This tests MultipleTimes.
        mock_obj.Close()

        self.mox.VerifyAll()

        self.assertEqual(9, actual_one)

        # Repeated calls should return same number.
        self.assertEqual(9, second_one)

    def test_mutliple_times_using_func(self):
        """Test that the Func is not evaluated more times than necessary.

        If a Func() has side effects, it can cause a passing test to fail.
        """

        self.counter = 0

        def my_func(actual_str):
            """Increment the counter if actual_str == 'foo'."""
            if actual_str == "foo":
                self.counter += 1
            return True

        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(mox.Func(my_func)).MultipleTimes()
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        mock_obj.Method("foo")
        mock_obj.Method("foo")
        mock_obj.Method("not-foo")
        mock_obj.Close()

        self.mox.VerifyAll()

        self.assertEqual(2, self.counter)

    def test_multiple_times_three_methods(self):
        """Test if MultipleTimesGroup works with three or more methods."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(1).MultipleTimes().AndReturn(9)
        mock_obj.Method(2).MultipleTimes().AndReturn(8)
        mock_obj.Method(3).MultipleTimes().AndReturn(7)
        mock_obj.Method(4).AndReturn(10)
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        actual_three = mock_obj.Method(3)
        mock_obj.Method(1)
        actual_two = mock_obj.Method(2)
        mock_obj.Method(3)
        actual_one = mock_obj.Method(1)
        actual_four = mock_obj.Method(4)
        mock_obj.Close()

        self.assertEqual(9, actual_one)
        self.assertEqual(8, actual_two)
        self.assertEqual(7, actual_three)
        self.assertEqual(10, actual_four)

        self.mox.VerifyAll()

    def test_multiple_times_missing_one(self):
        """Test if MultipleTimesGroup fails if one method is missing."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(1).MultipleTimes().AndReturn(9)
        mock_obj.Method(2).MultipleTimes().AndReturn(8)
        mock_obj.Method(3).MultipleTimes().AndReturn(7)
        mock_obj.Method(4).AndReturn(10)
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        mock_obj.Method(3)
        mock_obj.Method(2)
        mock_obj.Method(3)
        mock_obj.Method(3)
        mock_obj.Method(2)

        self.assertRaises(mox.UnexpectedMethodCallError, mock_obj.Method, 4)

    def test_multiple_times_two_groups(self):
        """Test if MultipleTimesGroup works with a group after a
        MultipleTimesGroup.
        """
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(1).MultipleTimes().AndReturn(9)
        mock_obj.Method(3).MultipleTimes("nr2").AndReturn(42)
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        actual_one = mock_obj.Method(1)
        mock_obj.Method(1)
        actual_three = mock_obj.Method(3)
        mock_obj.Method(3)
        mock_obj.Close()

        self.assertEqual(9, actual_one)
        self.assertEqual(42, actual_three)

        self.mox.VerifyAll()

    def test_multiple_times_two_groups_failure(self):
        """Test if MultipleTimesGroup fails with a group after a
        MultipleTimesGroup.
        """
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        mock_obj.Method(1).MultipleTimes().AndReturn(9)
        mock_obj.Method(3).MultipleTimes("nr2").AndReturn(42)
        mock_obj.Close()
        self.mox.ReplayAll()

        mock_obj.Open()
        mock_obj.Method(1)
        mock_obj.Method(1)
        mock_obj.Method(3)

        self.assertRaises(mox.UnexpectedMethodCallError, mock_obj.Method, 1)

    def test_with_side_effects(self):
        """Test side effect operations actually modify their target objects."""

        def modifier(mutable_list):
            mutable_list[0] = "mutated"

        mock_obj = self.mox.CreateMockAnything()
        mock_obj.ConfigureInOutParameter(["original"]).WithSideEffects(modifier)
        mock_obj.WorkWithParameter(["mutated"])
        self.mox.ReplayAll()

        local_list = ["original"]
        mock_obj.ConfigureInOutParameter(local_list)
        mock_obj.WorkWithParameter(local_list)

        self.mox.VerifyAll()

    def test_with_side_effects_exception(self):
        """Test side effect operations actually modify their target objects."""

        def modifier(mutable_list):
            mutable_list[0] = "mutated"

        mock_obj = self.mox.CreateMockAnything()
        method = mock_obj.ConfigureInOutParameter(["original"])
        method.WithSideEffects(modifier).AndRaise(Exception("exception"))
        mock_obj.WorkWithParameter(["mutated"])
        self.mox.ReplayAll()

        local_list = ["original"]
        self.assertRaises(Exception, mock_obj.ConfigureInOutParameter, local_list)
        mock_obj.WorkWithParameter(local_list)

        self.mox.VerifyAll()

    def test_stub_out_method(self):
        """Test that a method is replaced with a MockObject."""
        test_obj = TestClass()
        method_type = type(test_obj.other_valid_call)
        # Replace other_valid_call with a mock.
        self.mox.StubOutWithMock(test_obj, "other_valid_call")
        self.assertTrue(isinstance(test_obj.other_valid_call, mox.MockObject))
        self.assertFalse(type(test_obj.other_valid_call) is method_type)

        test_obj.other_valid_call().AndReturn("foo")
        self.mox.ReplayAll()

        actual = test_obj.other_valid_call()

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertEqual("foo", actual)
        self.assertTrue(type(test_obj.other_valid_call) is method_type)

    def test_stub_out_method__unbound__comparator(self):
        instance = TestClass()
        self.mox.StubOutWithMock(TestClass, "other_valid_call")

        TestClass.other_valid_call(mox.IgnoreArg()).AndReturn("foo")
        self.mox.ReplayAll()

        actual = TestClass.other_valid_call(instance)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertEqual("foo", actual)

    def test_stub_out_method__unbound__subclass__comparator(self):
        self.mox.StubOutWithMock(mox_test_helper.TestClassFromAnotherModule, "Value")
        mox_test_helper.TestClassFromAnotherModule.Value(
            mox.IsA(mox_test_helper.ChildClassFromAnotherModule)
        ).AndReturn("foo")
        self.mox.ReplayAll()

        instance = mox_test_helper.ChildClassFromAnotherModule()
        actual = mox_test_helper.TestClassFromAnotherModule.Value(instance)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertEqual("foo", actual)

    def test_stub_ou_method__unbound__with_optional_params(self):
        self.mox = mox.Mox()
        self.mox.StubOutWithMock(TestClass, "optional_args")
        TestClass.optional_args(mox.IgnoreArg(), foo=2)
        self.mox.ReplayAll()

        t = TestClass()
        TestClass.optional_args(t, foo=2)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__unbound__actual_instance(self):
        instance = TestClass()
        self.mox.StubOutWithMock(TestClass, "other_valid_call")

        TestClass.other_valid_call(instance).AndReturn("foo")
        self.mox.ReplayAll()

        actual = TestClass.other_valid_call(instance)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertEqual("foo", actual)

    def test_stub_out_method__unbound__different_instance(self):
        instance = TestClass()
        self.mox.StubOutWithMock(TestClass, "other_valid_call")

        TestClass.other_valid_call(instance).AndReturn("foo")
        self.mox.ReplayAll()

        # This should fail, since the instances are different
        self.assertRaises(mox.UnexpectedMethodCallError, TestClass.other_valid_call, "wrong self")

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)
        self.mox.UnsetStubs()

    def test_stub_out_method__unbound__named_using_positional(self):
        """Check positional parameters can be matched to keyword arguments."""
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "named_params")
        instance = mox_test_helper.ExampleClass()
        mox_test_helper.ExampleClass.named_params(instance, "foo", baz=None)
        self.mox.ReplayAll()

        mox_test_helper.ExampleClass.named_params(instance, "foo", baz=None)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__unbound__named_using_positional__some_positional(self):
        """Check positional parameters can be matched to keyword arguments."""
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "test_method")
        instance = mox_test_helper.ExampleClass()
        mox_test_helper.ExampleClass.test_method(instance, "one", "two", "nine")
        self.mox.ReplayAll()

        mox_test_helper.ExampleClass.test_method(instance, "one", "two", "nine")

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__unbound__special_args(self):
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "special_args")
        instance = mox_test_helper.ExampleClass()
        mox_test_helper.ExampleClass.special_args(instance, "foo", None, bar="bar")
        self.mox.ReplayAll()

        mox_test_helper.ExampleClass.special_args(instance, "foo", None, bar="bar")

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__bound__simple_test(self):
        t = self.mox.CreateMock(TestClass)

        t.method_with_args(mox.IgnoreArg(), mox.IgnoreArg()).AndReturn("foo")
        self.mox.ReplayAll()

        actual = t.method_with_args(None, None)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertEqual("foo", actual)

    def test_stub_out_method__bound__named_using_positional(self):
        """Check positional parameters can be matched to keyword arguments."""
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "named_params")
        instance = mox_test_helper.ExampleClass()
        instance.named_params("foo", baz=None)
        self.mox.ReplayAll()

        instance.named_params("foo", baz=None)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__bound__named_using_positional__some_positional(self):
        """Check positional parameters can be matched to keyword arguments."""
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "test_method")
        instance = mox_test_helper.ExampleClass()
        instance.test_method(instance, "one", "two", "nine")
        self.mox.ReplayAll()

        instance.test_method(instance, "one", "two", "nine")

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__bound__special_args(self):
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "special_args")
        instance = mox_test_helper.ExampleClass()
        instance.special_args(instance, "foo", None, bar="bar")
        self.mox.ReplayAll()

        instance.special_args(instance, "foo", None, bar="bar")

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_method__func__propgates_exceptions(self):
        """Errors in a Func comparator should propagate to the calling
        method."""

        class TestException(Exception):
            pass

        def raise_exception_on_not_one(value):
            if value == 1:
                return True
            else:
                raise TestException

        test_obj = TestClass()
        self.mox.StubOutWithMock(test_obj, "method_with_args")
        test_obj.method_with_args(mox.IgnoreArg(), mox.Func(raise_exception_on_not_one)).AndReturn(1)
        test_obj.method_with_args(mox.IgnoreArg(), mox.Func(raise_exception_on_not_one)).AndReturn(1)
        self.mox.ReplayAll()

        self.assertEqual(test_obj.method_with_args("ignored", 1), 1)
        self.assertRaises(TestException, test_obj.method_with_args, "ignored", 2)

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stubout__method__explicit_contains__for__set(self):
        """Test that explicit __contains__() for a set gets mocked with
        success."""
        self.mox.StubOutWithMock(TestClass, "SOME_CLASS_SET")
        TestClass.SOME_CLASS_SET.__contains__("x").AndReturn(True)

        dummy = TestClass()

        self.mox.ReplayAll()

        result = "x" in dummy.SOME_CLASS_SET

        self.mox.VerifyAll()

        self.assertTrue(result)

    def test_stub_out__signature_matching_init_(self):
        self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "__init__")
        mox_test_helper.ExampleClass.__init__(mox.IgnoreArg())
        self.mox.ReplayAll()

        # Create an instance of a child class, which calls the parent
        # __init__
        mox_test_helper.ChildExampleClass()

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

    def test_stub_out_class__old_style(self):
        """Test a mocked class whose __init__ returns a Mock."""
        self.mox.StubOutWithMock(mox_test_helper, "TestClassFromAnotherModule")
        self.assertIsInstance(mox_test_helper.TestClassFromAnotherModule, mox.MockObject)

        mock_instance = self.mox.CreateMock(mox_test_helper.TestClassFromAnotherModule)
        mox_test_helper.TestClassFromAnotherModule().AndReturn(mock_instance)
        mock_instance.Value().AndReturn("mock instance")

        self.mox.ReplayAll()

        a_mock = mox_test_helper.TestClassFromAnotherModule()
        actual = a_mock.Value()

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertEqual("mock instance", actual)

    def test_stub_out_class(self):
        self.mox.StubOutClassWithMocks(mox_test_helper, "CallableClass")

        # Instance one
        mock_one = mox_test_helper.CallableClass(1, 2)
        mock_one.Value().AndReturn("mock")

        # Instance two
        mock_two = mox_test_helper.CallableClass(8, 9)
        mock_two("one").AndReturn("called mock")

        self.mox.ReplayAll()

        one = mox_test_helper.CallableClass(1, 2)
        actual_one = one.Value()

        two = mox_test_helper.CallableClass(8, 9)
        actual_two = two("one")

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

        # Verify the correct mocks were returned
        self.assertEqual(mock_one, one)
        self.assertEqual(mock_two, two)

        # Verify
        self.assertEqual("mock", actual_one)
        self.assertEqual("called mock", actual_two)

    def test_stub_out_class_with_meta_class(self):
        self.mox.StubOutClassWithMocks(mox_test_helper, "ChildClassWithMetaClass")

        mock_one = mox_test_helper.ChildClassWithMetaClass(kw=1)
        mock_one.Value().AndReturn("mock")

        self.mox.ReplayAll()

        one = mox_test_helper.ChildClassWithMetaClass(kw=1)
        actual_one = one.Value()

        self.mox.VerifyAll()
        self.mox.UnsetStubs()

        # Verify the correct mocks were returned
        self.assertEqual(mock_one, one)

        # Verify
        self.assertEqual("mock", actual_one)
        self.assertEqual("meta", one.x)

    try:
        import abc

        # I'd use the unittest skipping decorators for this but I want to
        # support older versions of Python that don't have them.

        def test_stub_out_class__a_b_c_meta(self):
            self.mox.StubOutClassWithMocks(mox_test_helper, "CallableSubclassOfMyDictABC")
            mock_foo = mox_test_helper.CallableSubclassOfMyDictABC(foo="!mock bar")
            mock_foo["foo"].AndReturn("mock bar")
            mock_spam = mox_test_helper.CallableSubclassOfMyDictABC(spam="!mock eggs")
            mock_spam("beans").AndReturn("called mock")

            self.mox.ReplayAll()

            foo = mox_test_helper.CallableSubclassOfMyDictABC(foo="!mock bar")
            actual_foo_bar = foo["foo"]

            spam = mox_test_helper.CallableSubclassOfMyDictABC(spam="!mock eggs")
            actual_spam = spam("beans")

            self.mox.VerifyAll()
            self.mox.UnsetStubs()

            # Verify the correct mocks were returned
            self.assertEqual(mock_foo, foo)
            self.assertEqual(mock_spam, spam)

            # Verify
            self.assertEqual("mock bar", actual_foo_bar)
            self.assertEqual("called mock", actual_spam)

    except ImportError:
        print("testStubOutClass_ABCMeta. ... Skipped - no abc module", file=sys.stderr)

    def test_stub_out_class__not_a_class(self):
        self.assertRaises(TypeError, self.mox.StubOutClassWithMocks, mox_test_helper, "MyTestFunction")

    def test_stub_out_class_not_enough_created(self):
        self.mox.StubOutClassWithMocks(mox_test_helper, "CallableClass")

        mox_test_helper.CallableClass(1, 2)
        mox_test_helper.CallableClass(8, 9)

        self.mox.ReplayAll()
        mox_test_helper.CallableClass(1, 2)

        self.assertRaises(mox.ExpectedMockCreationError, self.mox.VerifyAll)
        self.mox.UnsetStubs()

    def test_stub_out_class_wrong_signature(self):
        self.mox.StubOutClassWithMocks(mox_test_helper, "CallableClass")

        self.assertRaises(AttributeError, mox_test_helper.CallableClass)

        self.mox.UnsetStubs()

    def test_stub_out_class_wrong_parameters(self):
        self.mox.StubOutClassWithMocks(mox_test_helper, "CallableClass")

        mox_test_helper.CallableClass(1, 2)

        self.mox.ReplayAll()

        self.assertRaises(mox.UnexpectedMethodCallError, mox_test_helper.CallableClass, 8, 9)
        self.mox.UnsetStubs()

    def test_stub_out_class_too_many_created(self):
        self.mox.StubOutClassWithMocks(mox_test_helper, "CallableClass")

        mox_test_helper.CallableClass(1, 2)

        self.mox.ReplayAll()
        mox_test_helper.CallableClass(1, 2)
        self.assertRaises(mox.UnexpectedMockCreationError, mox_test_helper.CallableClass, 8, 9)

        self.mox.UnsetStubs()

    def test_warns_user_if_mocking_mock(self):
        """Test that user is warned if they try to stub out a MockAnything."""
        self.mox.StubOutWithMock(TestClass, "my_static_method")
        self.assertRaises(TypeError, self.mox.StubOutWithMock, TestClass, "my_static_method")

    def test_stub_out_first_class_method_verifies_signature(self):
        self.mox.StubOutWithMock(mox_test_helper, "MyTestFunction")

        # Wrong number of arguments
        self.assertRaises(AttributeError, mox_test_helper.MyTestFunction, 1)
        self.mox.UnsetStubs()

    def _test_method_signature_verification(self, stubClass):
        # If stubClass is true, the test is run against an a stubbed out class,
        # else the test is run against a stubbed out instance.
        if stubClass:
            self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "test_method")
            obj = mox_test_helper.ExampleClass()
        else:
            obj = mox_test_helper.ExampleClass()
            self.mox.StubOutWithMock(mox_test_helper.ExampleClass, "test_method")
        self.assertRaises(AttributeError, obj.test_method)
        self.assertRaises(AttributeError, obj.test_method, 1)
        self.assertRaises(AttributeError, obj.test_method, nine=2)
        obj.test_method(1, 2)
        obj.test_method(1, 2, 3)
        obj.test_method(1, 2, nine=3)
        self.assertRaises(AttributeError, obj.test_method, 1, 2, 3, 4)
        self.mox.UnsetStubs()

    def test_stub_out_class_method_verifies_signature(self):
        self._test_method_signature_verification(stubClass=True)

    def test_stub_out_object_method_verifies_signature(self):
        self._test_method_signature_verification(stubClass=False)

    def test_stub_out_object(self):
        """Test than object is replaced with a Mock."""

        class Foo(object):
            def __init__(self):
                self.obj = TestClass()

        foo = Foo()
        self.mox.StubOutWithMock(foo, "obj")
        self.assertIsInstance(foo.obj, mox.MockObject)
        foo.obj.valid_call()
        self.mox.ReplayAll()

        foo.obj.valid_call()

        self.mox.VerifyAll()
        self.mox.UnsetStubs()
        self.assertNotIsInstance(foo.obj, mox.MockObject)

    def test_stub_out_re_works(self):
        self.mox.StubOutWithMock(re, "search")

        re.search("a", "ivan").AndReturn("true")

        self.mox.ReplayAll()
        result = TestClass().re_search()
        self.mox.VerifyAll()
        self.mox.UnsetStubs()

        self.assertEqual(result, "true")

    def test_forgot_replay_helpful_message(self):
        """If there is an AttributeError on a MockMethod, give users a helpful
        msg."""
        foo = self.mox.CreateMockAnything()
        bar = self.mox.CreateMockAnything()
        foo.GetBar().AndReturn(bar)
        bar.ShowMeTheMoney()
        # Forgot to replay!
        try:
            foo.GetBar().ShowMeTheMoney()
        except AttributeError as e:
            self.assertEqual(
                'MockMethod has no attribute "ShowMeTheMoney". '
                "Did you remember to put your mocks in replay "
                "mode?",
                str(e),
            )

    def test_swallowed_unknown_method_call(self):
        """Test that a swallowed UnknownMethodCallError will be re-raised."""
        dummy = self.mox.CreateMock(TestClass)
        dummy._Replay()

        def call():
            try:
                dummy.invalid_call()
            except mox.UnknownMethodCallError:
                pass

        # UnknownMethodCallError swallowed
        call()

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)

    def test_swallowed_unexpected_mock_creation(self):
        """Test that a swallowed UnexpectedMockCreationError will be
        re-raised."""
        self.mox.StubOutClassWithMocks(mox_test_helper, "CallableClass")
        self.mox.ReplayAll()

        def call():
            try:
                mox_test_helper.CallableClass(1, 2)
            except mox.UnexpectedMockCreationError:
                pass

        # UnexpectedMockCreationError swallowed
        call()

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)
        self.mox.UnsetStubs()

    def test_swallowed_unexpected_method_call__wrong_method(self):
        """Test that a swallowed UnexpectedMethodCallError will be re-raised.

        This case is an extraneous method call."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        self.mox.ReplayAll()

        def call():
            mock_obj.Open()
            try:
                mock_obj.Close()
            except mox.UnexpectedMethodCallError:
                pass

        # UnexpectedMethodCall swallowed
        call()

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)

    def test_swallowed_unexpected_method_call__wrong_arguments(self):
        """Test that a swallowed UnexpectedMethodCallError will be re-raised.

        This case is an extraneous method call."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open()
        self.mox.ReplayAll()

        def call():
            try:
                mock_obj.Open(1)
            except mox.UnexpectedMethodCallError:
                pass

        # UnexpectedMethodCall swallowed
        call()

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)

    def test_swallowed_unexpected_method_call__unordered_group(self):
        """Test that a swallowed UnexpectedMethodCallError will be re-raised.

        This case is an extraneous method call in an unordered group."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open().InAnyOrder()
        mock_obj.Close().InAnyOrder()
        self.mox.ReplayAll()

        def call():
            mock_obj.Close()
            try:
                mock_obj.Open(1)
            except mox.UnexpectedMethodCallError:
                pass

        # UnexpectedMethodCall swallowed
        call()

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)

    def test_swallowed_unexpected_method_call__multiple_times_group(self):
        """Test that a swallowed UnexpectedMethodCallError will be re-raised.

        This case is an extraneous method call in a multiple times group."""
        mock_obj = self.mox.CreateMockAnything()
        mock_obj.Open().MultipleTimes()
        self.mox.ReplayAll()

        def call():
            try:
                mock_obj.Open(1)
            except mox.UnexpectedMethodCallError:
                pass

        # UnexpectedMethodCall swallowed
        call()

        self.assertRaises(mox.SwallowedExceptionError, self.mox.VerifyAll)


class ReplayTest(unittest.TestCase):
    """Verify Replay works properly."""

    def test_replay(self):
        """Replay should put objects into replay mode."""
        mock_obj = mox.MockObject(TestClass)
        self.assertFalse(mock_obj._replay_mode)
        mox.Replay(mock_obj)
        self.assertTrue(mock_obj._replay_mode)


class MoxTestBaseTest(unittest.TestCase):
    """Verify that all tests in a class derived from MoxTestBase are
    wrapped."""

    def setUp(self):
        self.mox = mox.Mox()
        self.test_mox = mox.Mox()
        self.test_stubs = mox.stubout.StubOutForTesting()
        self.result = unittest.TestResult()

    def tearDown(self):
        self.mox.UnsetStubs()
        self.test_mox.UnsetStubs()
        self.test_stubs.UnsetAll()
        self.test_stubs.SmartUnsetAll()

    def _setUpTestClass(self):
        """Replacement for setUp in the test class instance.

        Assigns a mox.Mox instance as the mox attribute of the test class
        instance. This replacement Mox instance is under our control before
        setUp is called in the test class instance.
        """
        self.test.mox = self.test_mox
        self.test.stubs = self.test_stubs

    def _create_test(self, test_name):
        """Create a test from our example mox class.

        The created test instance is assigned to this instances test attribute.
        """
        self.test = mox_test_helper.ExampleMoxTest(test_name)
        self.mox.stubs.Set(self.test, "setUp", self._setUpTestClass)

    def _verify_success(self):
        """Run the checks to confirm test method completed successfully."""
        self.mox.StubOutWithMock(self.test_mox, "UnsetStubs")
        self.mox.StubOutWithMock(self.test_mox, "VerifyAll")
        self.mox.StubOutWithMock(self.test_stubs, "UnsetAll")
        self.mox.StubOutWithMock(self.test_stubs, "SmartUnsetAll")
        self.test_mox.UnsetStubs()
        self.test_mox.VerifyAll()
        self.test_stubs.UnsetAll()
        self.test_stubs.SmartUnsetAll()
        self.mox.ReplayAll()
        self.test.run(result=self.result)
        self.assertTrue(self.result.wasSuccessful())
        self.mox.VerifyAll()
        self.mox.UnsetStubs()  # Needed to call the real VerifyAll() below.
        self.test_mox.VerifyAll()

    def test_success(self):
        """Successful test method execution test."""
        self._create_test("testSuccess")
        self._verify_success()

    def test_success_no_mocks(self):
        """Let testSuccess() unset all the mocks, and verify they've been
        unset."""
        self._create_test("testSuccess")
        self.test.run(result=self.result)
        self.assertTrue(self.result.wasSuccessful())
        self.assertEqual(OS_LISTDIR, mox_test_helper.os.listdir)

    def test_stubs(self):
        """Test that "self.stubs" is provided as is useful."""
        self._create_test("testHasStubs")
        self._verify_success()

    def test_raises_with_statement(self):
        self._create_test("testRaisesWithStatement")
        self._verify_success()

    def test_stubs_no_mocks(self):
        """Let testHasStubs() unset the stubs by itself."""
        self._create_test("testHasStubs")
        self.test.run(result=self.result)
        self.assertTrue(self.result.wasSuccessful())
        self.assertEqual(OS_LISTDIR, mox_test_helper.os.listdir)

    def test_expected_not_called(self):
        """Stubbed out method is not called."""
        self._create_test("testExpectedNotCalled")
        self.mox.StubOutWithMock(self.test_mox, "UnsetStubs")
        self.mox.StubOutWithMock(self.test_stubs, "UnsetAll")
        self.mox.StubOutWithMock(self.test_stubs, "SmartUnsetAll")
        # Don't stub out VerifyAll - that's what causes the test to fail
        self.test_mox.UnsetStubs()
        self.test_stubs.UnsetAll()
        self.test_stubs.SmartUnsetAll()
        self.mox.ReplayAll()
        self.test.run(result=self.result)
        self.assertFalse(self.result.wasSuccessful())
        self.mox.VerifyAll()

    def test_expected_not_called_no_mocks(self):
        """Let testExpectedNotCalled() unset all the mocks by itself."""
        self._create_test("testExpectedNotCalled")
        self.test.run(result=self.result)
        self.assertFalse(self.result.wasSuccessful())
        self.assertEqual(OS_LISTDIR, mox_test_helper.os.listdir)

    def test_unexpected_call(self):
        """Stubbed out method is called with unexpected arguments."""
        self._create_test("testUnexpectedCall")
        self.mox.StubOutWithMock(self.test_mox, "UnsetStubs")
        self.mox.StubOutWithMock(self.test_stubs, "UnsetAll")
        self.mox.StubOutWithMock(self.test_stubs, "SmartUnsetAll")
        # Ensure no calls are made to VerifyAll()
        self.mox.StubOutWithMock(self.test_mox, "VerifyAll")
        self.test_mox.UnsetStubs()
        self.test_stubs.UnsetAll()
        self.test_stubs.SmartUnsetAll()
        self.mox.ReplayAll()
        self.test.run(result=self.result)
        self.assertFalse(self.result.wasSuccessful())
        self.mox.VerifyAll()

    def test_failure(self):
        """Failing assertion in test method."""
        self._create_test("testFailure")
        self.mox.StubOutWithMock(self.test_mox, "UnsetStubs")
        self.mox.StubOutWithMock(self.test_stubs, "UnsetAll")
        self.mox.StubOutWithMock(self.test_stubs, "SmartUnsetAll")
        # Ensure no calls are made to VerifyAll()
        self.mox.StubOutWithMock(self.test_mox, "VerifyAll")
        self.test_mox.UnsetStubs()
        self.test_stubs.UnsetAll()
        self.test_stubs.SmartUnsetAll()
        self.mox.ReplayAll()
        self.test.run(result=self.result)
        self.assertFalse(self.result.wasSuccessful())
        self.mox.VerifyAll()

    def test_mixin(self):
        """Run test from mix-in test class, ensure it passes."""
        self._create_test("testStat")
        self._verify_success()

    def test_mixin_again(self):
        """Run same test as above but from the current test class.

        This ensures metaclass properly wrapped test methods from all base
        classes. If unsetting of stubs doesn't happen, this will fail.
        """
        self._create_test("testStatOther")
        self._verify_success()


class VerifyTest(unittest.TestCase):
    """Verify Verify works properly."""

    def test_verify(self):
        """Verify should be called for all objects.

        This should throw an exception because the expected behavior did not
        occur."""
        mock_obj = mox.MockObject(TestClass)
        mock_obj.valid_call()
        mock_obj._Replay()
        self.assertRaises(mox.ExpectedMethodCallsError, mox.Verify, mock_obj)


class ResetTest(unittest.TestCase):
    """Verify Reset works properly."""

    def test_reset(self):
        """Should empty all queues and put mocks in record mode."""
        mock_obj = mox.MockObject(TestClass)
        mock_obj.valid_call()
        self.assertFalse(mock_obj._replay_mode)
        mock_obj._Replay()
        self.assertTrue(mock_obj._replay_mode)
        self.assertEqual(1, len(mock_obj._expected_calls_queue))

        mox.Reset(mock_obj)
        self.assertFalse(mock_obj._replay_mode)
        self.assertEqual(0, len(mock_obj._expected_calls_queue))


class MyTestCase(unittest.TestCase):
    """Simulate the use of a fake wrapper around Python's unittest library."""

    def setUp(self):
        super(MyTestCase, self).setUp()
        self.critical_variable = 42
        self.another_critical_variable = 42

    def test_method_override(self):
        """Should be properly overriden in a derived class."""
        self.assertEqual(42, self.another_critical_variable)
        self.another_critical_variable += 1


class MoxTestBaseMultipleInheritanceTest(mox.MoxTestBase, MyTestCase):
    """Test that multiple inheritance can be used with MoxTestBase."""

    def setUp(self):
        super(MoxTestBaseMultipleInheritanceTest, self).setUp()
        self.another_critical_variable = 99

    def test_multiple_inheritance(self):
        """Should be able to access members created by all parent setUp()."""
        self.assertIsInstance(self.mox, mox.Mox)
        self.assertEqual(42, self.critical_variable)

    def test_method_override(self):
        """Should run before MyTestCase.test_method_override."""
        self.assertEqual(99, self.another_critical_variable)
        self.another_critical_variable = 42
        super(MoxTestBaseMultipleInheritanceTest, self).test_method_override()
        self.assertEqual(43, self.another_critical_variable)


class MoxTestDontMockProperties(MoxTestBaseTest):
    def test_properties_arent_mocked(self):
        mock_class = self.mox.CreateMock(ClassWithProperties)
        self.assertRaises(mox.UnknownMethodCallError, lambda: mock_class.prop_attr)


class TestClass:
    """This class is used only for testing the mock framework"""

    SOME_CLASS_SET = {"a", "b", "c"}
    SOME_CLASS_VAR = "test_value"
    _PROTECTED_CLASS_VAR = "protected value"

    def __init__(self, ivar=None):
        self.__ivar = ivar

    def __eq__(self, rhs):
        return self.__ivar == rhs

    def __ne__(self, rhs):
        return not self.__eq__(rhs)

    def valid_call(self):
        pass

    def method_with_args(self, one, two, nine=None):
        pass

    def other_valid_call(self):
        pass

    def optional_args(self, foo="boom"):
        pass

    def valid_call_with_args(self, *args, **kwargs):
        pass

    @classmethod
    def my_class_method(cls):
        pass

    @staticmethod
    def my_static_method():
        pass

    def _protected_call(self):
        pass

    def __private_call(self):
        pass

    def __do_not_mock(self):
        pass

    def __getitem__(self, key):
        """Return the value for key."""
        return self.d[key]

    def __setitem__(self, key, value):
        """Set the value for key to value."""
        self.d[key] = value

    def __contains__(self, key):
        """Returns True if d contains the key."""
        return key in self.d

    def __iter__(self):
        pass

    def re_search(self):
        return re.search("a", "ivan")


class ChildClass(TestClass):
    """This inherits from TestClass."""

    def __init__(self):
        TestClass.__init__(self)

    def child_valid_call(self):
        pass


class CallableClass(object):
    """This class is callable, and that should be mockable!"""

    def __init__(self):
        pass

    def __call__(self, param):
        return param


class ClassWithProperties(object):
    def setter_attr(self, value):
        pass

    def getter_attr(self):
        pass

    prop_attr = property(getter_attr, setter_attr)


class SubscribtableNonIterableClass(object):
    def __getitem__(self, index):
        raise IndexError


class InheritsFromCallable(CallableClass):
    """This class should also be mockable; it inherits from a callable class."""

    pass


if __name__ == "__main__":
    unittest.main()

## Tutorial [1]

### Basics

As said before, with Pymox you should set expectations and then enter in replay mode. Here is a basic example:

```python
class Duck:
    def quack(self, times=1):
        return ['quack'] * times

    def walk(self):
        return ['walking']

    def walk_and_quack(self, times=1):
        return self.walk() + self.quack(times=times)
```
Here is a `Duck` class. Let's play with our 🦆 and Pymox!

```python
import mox


class TestDuck:

    def test_quack(self):
        m = mox.Mox()
        m_duck = m.CreateMock(Duck)

        # expects quack to be called with `times=1`
        m_duck.quack(times=1).returns(['new quack'])

        m.replay_all()
        assert m_duck.quack(times=1) == ['new quack']
        m.verify_all()
```
Let's change the test a little bit:

```python
    # [...]
    def test_quack_2(self):
        m = mox.Mox()
        m_duck = m.CreateMock(Duck)

        # expects quack to be called with `times=1`
        m_duck.quack(times=1).returns(['new quack'])

        m.replay_all()
        assert m_duck.walk() == ['walking']
        assert m_duck.quack(times=1) == ['new quack']
        m.verify_all()
```
The test above will fail with the following error:
```python-traceback
    E           mox.mox.UnexpectedMethodCallError: Unexpected method call.  unexpected:-  expected:+
    E           - Duck.walk() -> None
    E           + Duck.quack(times=1) -> ['new quack']
```

Since you expected quack to be called and walk was called instead. You can add an expectation for walk:

```python
    def test_quack_3(self):
        m = mox.Mox()
        m_duck = m.CreateMock(Duck)

        # expects quack to be called with `times=1`
        m_duck.quack(times=1).returns(['new quack'])
        m_duck.walk().returns(['pretending to be walking'])

        m.replay_all()
        assert m_duck.quack(times=1) == ['new quack']
        assert m_duck.walk() == ['pretending to be walking']
        m.verify_all()
```

You can also stub out `quack` method only and mox won't care about the other methods:

```python
    def test_quack_4(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(duck, 'quack')
        """
        You can also do with the class:
        m.stubout(Duck, 'quack')
        """

        # expects quack to be called with `times=1`
        duck.quack(times=1).returns(['new quack'])

        m.replay_all()
        assert duck.quack(times=1) == ['new quack']
        assert duck.walk() == ['walking']
        m.verify_all()
```

The order matters, so if you do:

```python
    def test_quack_5(self):
        m = mox.Mox()
        m_duck = m.CreateMock(Duck)

        # expects quack to be called with `times=1`
        m_duck.quack(times=1).returns(['new quack'])
        m_duck.walk().returns(['pretending to be walking'])

        m.replay_all()
        assert m_duck.walk() == ['pretending to be walking']
        assert m_duck.quack(times=1) == ['new quack']
        m.verify_all()
```
It fails with:
```python-traceback
    E           mox.mox.UnexpectedMethodCallError: Unexpected method call.  unexpected:-  expected:+
    E           - Duck.walk() -> None
    E           + Duck.quack(times=1) -> ['new quack']
```

To fix that you can use `any_order()`:

```python
    def test_quack_6(self):
        m = mox.Mox()
        m_duck = m.CreateMock(Duck)

        # expects quack to be called with `times=1`
        m_duck.quack(times=1).any_order().returns(['new quack'])
        m_duck.walk().any_order().returns(['pretending to be walking'])

        m.replay_all()
        assert m_duck.walk() == ['pretending to be walking']
        assert m_duck.quack(times=1) == ['new quack']
```

### Comparators

You can use comparators when you are unsure of the arguments of a method call.

```python
    def test_quack_7(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        def validate_arg(arg):
         if arg in [1, 2, 3]:
          return True
         return False

        duck.quack(times=mox.is_a(int)).returns(['new quack'])
        duck.quack(times=mox.not_(mox.is_(4))).returns(['new quack'])
        duck.quack(times=mox.func(validate_arg)).returns(['new quack'])
        duck.quack(times=mox.or_(mox.Is(1), mox.is_(2), mox.is_(3))).returns(['new quack'])

        duck.quack(times=mox.ignore_arg()).returns(['new quack'])
        duck.quack(times=mox.is_almost(1.00003, places=4)).returns(['new quack'])

        m.replay_all()
        assert duck.quack(times=random.choice([1, 2, 3])) == ['new quack']
        assert duck.quack(times=random.choice([1, 2, 3])) == mox.in_('new quack')
        assert duck.quack(times=random.choice([1, 2, 3]))[0] == mox.str_contains('quack')
        assert duck.quack(times=random.choice([1, 2, 3])) == mox.same_elements_as({'new quack'})

        assert duck.quack(times=random.choice([1, 2, 3])) == ['new quack']
        assert duck.quack(times=1) == ['new quack']
        m.verify_all()
```

All the assertions for the test above should pass.
There are other cool comparators, like: `and`, `contains_attribute_value`, `contains_key_value`.

For more comparators, see: https://pymox.readthedocs.io/en/latest/reference.html#comparators

### Remember

It's possible to also remember a value that might be changed in your code. See the test below:

```python
    def test_quack_8(self):

        class StopQuackingDuck:

            def _do_quack(self, choices=None):
                return choices

            def quack(self, choices=[], less=False):
                if less:
                    choices.pop()
                self._do_quack(choices=choices)

        m = mox.Mox()
        duck = StopQuackingDuck()

        m.stubout(StopQuackingDuck, '_do_quack')

        choices_1 = mox.value()
        choices_2 = mox.value()
        duck._do_quack(choices=mox.remember(choices_1))
        duck._do_quack(choices=mox.remember(choices_2))
        duck._do_quack(choices=mox.remember(choices_2))
        duck._do_quack(choices=mox.remember(choices_2))

        all_choices = ['quack', 'new quack', 'newest quack']

        m.replay_all()
        duck.quack(all_choices, less=False)
        assert choices_1 == ['quack', 'new quack', 'newest quack']

        duck.quack(all_choices, less=True)
        assert choices_2 == ['quack', 'new quack']

        duck.quack(all_choices, less=True)
        assert choices_2 == ['quack']

        duck.quack(all_choices, less=True)
        assert choices_2 == []
        m.verify_all()
```

### Other

You can also make a method return a different value the second time it's called:

```python
    def test_walk_and_quack_0(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        duck.quack(times=1).returns(['new quack'])
        duck.quack(times=1).returns(['newest quack'])

        m.replay_all()
        assert duck.walk_and_quack() == ['walking', 'new quack']
```

But since we didn't use m.verify_all(), it didn't require the second call to happen. Let's add the verify and see
what happens:

```python
    def test_walk_and_quack_1(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        duck.quack(times=1).returns(['new quack'])
        duck.quack(times=1).returns(['newest quack'])

        m.replay_all()
        assert duck.walk_and_quack() == ['walking', 'new quack']
        m.verify_all()
```

It fails with:

```python-traceback
    E           mox.mox.ExpectedMethodCallsError: Verify: Expected methods never called:
    E             0.  Duck.quack.__call__(times=1) -> ['newest quack']
```

Let's fix it by adding a second call:

```python
    def test_walk_and_quack_2(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        duck.quack(times=1).returns(['new quack'])
        duck.quack(times=1).returns(['newest quack'])

        m.replay_all()
        assert duck.walk_and_quack() == ['walking', 'new quack']
        assert duck.walk_and_quack() == ['walking', 'new quack']
        m.verify_all()
```

Now you get the following error, since in the second time it returns ['newest quack'].

```python-traceback
    E       AssertionError: assert ['walking', 'newest quack'] == ['walking', 'new quack']
    E         At index 1 diff: 'newest quack' != 'new quack'
    E         Full diff:
    E         - ['walking', 'new quack']
    E         + ['walking', 'newest quack']
    E         ?                 +++
```

Let's fix it:

```python
    def test_walk_and_quack_3(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        duck.quack(times=1).returns(['new quack'])
        duck.quack(times=1).returns(['newest quack'])

        m.replay_all()
        assert duck.walk_and_quack() == ['walking', 'new quack']
        assert duck.walk_and_quack() == ['walking', 'newest quack']
        m.verify_all()
```

Let's now see how we can mock and assert calls in the context of a loop:

```python
    def test_walk_and_quack_4(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        duck.quack(times=1).returns(['new quack'])

        m.replay_all()
        assert duck.walk() == ['walking']
        for _ in range(3):
            assert duck.walk_and_quack() == ['walking', 'new quack']
        m.verify_all()
```

If you run the test above, you get the following:

```python-traceback
    E           mox.mox.UnexpectedMethodCallError: Unexpected method call Duck.quack.__call__(times=1) -> None
```

Let's fix by using the `multiple_times` group.

```python
    def test_walk_and_quack_5(self):
        m = mox.Mox()
        duck = Duck()

        m.stubout(Duck, 'quack')

        duck.quack(times=1).multiple_times().returns(['new quack'])

        m.replay_all()
        assert duck.walk() == ['walking']
        for _ in range(3):
            assert duck.walk_and_quack() == ['walking', 'new quack']
        m.verify_all()
```

If you know exactly how many calls are made, you can add an argument: `.multiple_times(3)`.

Python mocking on steroids.

|package-version| |python-versions| |Documentation Status|

What
====

Pymox is mocking on steroids. It’s a powerful mock object framework for
Python, providing many tools to help with your unit tests, so you can
write them in an easy, quick and intuitive way.

Why
===

Why ``Pymox``? Python already has batteries included. It has its own
``mock`` library, widely used in Python applications.

So, why ``pytest``, given we have Python’s ``unittest``? Why ``arrow``
or ``pendulum`` given we have Python’s ``datetime``? Why ``X``, given we
have Python’s ``Y``?

You got it 😉!

Coming Soon \* Async support \* Decorator \* Ignore args \* String
imports

How
===

Install
-------

.. code:: bash

   pip install pymox

Cool Stuff
----------

New Elegant Way
~~~~~~~~~~~~~~~

.. code:: python

   # conftest.py
   pytest_plugins = ("mox.testing.pytest_mox",)


   # test.py
   from mox import expect, stubout


   class TestOs:
        def test_getcwd(self):
           with stubout(os, 'getcwd') as m_getcwd, expect:
               m_getcwd.to_be.called_with().and_return('/mox/path')

           assert os.getcwd() == '/mox/path'
           mox.verify(m_getcwd)

If you want to be less verbose:

.. code:: python

   class TestOs:
        def test_getcwd(self):
           with stubout(os, 'getcwd') as m_getcwd:
               m_getcwd().returns('/mox/path')

           assert os.getcwd() == '/mox/path'
           mox.verify(m_getcwd)

Anything you put inside the context manager is a call expectation, so to
not expect any call you can:

.. code:: python

   class TestOs:
        def test_getcwd(self):
           with stubout(os, 'getcwd') as m_getcwd:
               pass

           # will raise a UnexpectedMethodCallError
           assert os.getcwd() == '/mox/path'
           mox.verify(m_getcwd)

Dict Access
~~~~~~~~~~~

.. code:: python

   class TestDict:
       def test_dict_access(self):
           config = {'env': 'dev', 'reload': True}

           # doing in another way using create, but you can do with stubout too
           mock_config = mox.create(config)

           mock_config['env'].returns('prod')
           mock_config['reload'].returns(False)

           mox.replay(mock_config)
           assert mock_config['env'] == 'prod'
           assert mock_config['reload'] is False
           mox.verify(mock_config)

Comparators
~~~~~~~~~~~

.. code:: python

   class Client:
       def get(self, url, params):
           return requests.get(url, params)


   class Service:
       def get_contacts(self):
           url = 'https://my.reallylong.service/api/v1/contacts/'
           params = {'added': '7days', 'order_by': '-created'}
           return Client().get(url, params)


   class TestSevice:
       def test_get_contacts_comparators_str_and_key_value(self):
           with stubout(Client, 'get') as m_get:
               url = mox.str_contains('/api/v1/contacts')
               params = mox.contains_key_value('added', '7days')
               m_get(url, params).returns({})

           service = Service()
           assert service.get_contacts() == {}
           mox.verify(m_get)

       def test_get_contacts_comparators_and_func_in_is_a(self):
           with stubout(Client, 'get') as m_get:
               url = mox.func(lambda v: str.startswith('https://my.reallylong.service/'))
               params = mox.and_(
                   mox.is_a(dict),
                   mox.in_('added'),
               )
               m_get(url, params).returns({})

           service = Service()
           assert service.get_contacts() == {}
           mox.verify(m_get)

       def test_get_contacts_comparators_ignore_arg_not(self):
           with stubout(Client, 'get') as m_get:
               url = mox.ignore_arg
               params = mox.not_(None)
               m_get(url, params).returns({})

           service = Service()
           assert service.get_contacts() == {}
           mox.verify(m_get)

Other comparators: ``contains_attribute_value``, ``in_``, ``is_``,
``is_almost``, ``or_``, ``same_elements_as``, ``regex``

And Raises
~~~~~~~~~~

.. code:: python

   class TestOs:
        def test_getcwd(self):
           with stubout(os, 'getcwd') as m_getcwd:
               # .and_raise(..) also works
               os.getcwd().raises(Exception('error'))

           with pytest.raises(Exception, match='error'):
               os.getcwd()
           mox.verify(m_getcwd)

Multiple Times
~~~~~~~~~~~~~~

.. code:: python

   class TestOs:
        def test_getcwd(self):
           with stubout(os, 'getcwd') as m_getcwd:
               m_getcwd().returns('/mox/path')
               # the second call will return a different value
               m_getcwd().returns('/mox/another/path')
               # the three subsequent calls will return "/"
               # if no argument is passed, multiple_times doesn't limit the number of calls
               m_getcwd().multiple_times(3).returns('/')

           assert os.getcwd() == '/mox/path'
           assert os.getcwd() == '/mox/another/path'
           mox.verify(m_getcwd)

Any order
~~~~~~~~~

If you stub out multiple, the order os calls is enforced, unless you use
``any_order``

.. code:: python

   class TestOs:
       def test_getcwd(self):
           with stubout.many([os, 'getcwd'], [os, 'cpu_count']) as (m_getcwd, m_cpu_count):
               m_getcwd().returns('/mox/path')
               m_cpu_count().returns('10')

           # will raise a UnexpectedMethodCallError
           assert os.cpu_count() == '10'
           assert os.getcwd() == '/mox/path'
           mox.verify(m_getcwd, m_cpu_count)

       def test_getcwd_anyorder(self):
           with stubout.many([os, 'getcwd'], [os, 'cpu_count']) as (m_getcwd, m_cpu_count):
               m_getcwd().any_order().returns('/mox/path')
               m_cpu_count().any_order().returns('10')

           assert os.cpu_count() == '10'
           assert os.getcwd() == '/mox/path'
           mox.verify(m_getcwd, m_cpu_count)

Remember/Value
~~~~~~~~~~~~~~

The Remember and Value are comparators, but they deserve their own
section. They can be useful to retrieve some values from deeper levels
of your codebase, and bring to the test for comparison. Let’s see an
example:

.. code:: python

   class Handler:
       def modify(self, d):
           # any integer key less than 5 is removed from the dict
           keys_to_remove = [key for key in d if isinstance(key, int) and key < 5]
           for key in keys_to_remove:
               del d[key]
           return d

       def send(self, d):
           return d


   class Manager:
       def __init__(self, handlers=None):
           self.handlers = handlers or []

       def process(self, d):
           for handler in self.handlers:
               modified = handler.modify(d)
               handler.send(modified)


   class TestList(mox.MoxTestBase):
       def test_getcwd(self):
           mydict = {1: "apple", 4: "banana", 6: {2: 3, 4: {1: "orange", 7: 8}}, 8: 3}
           myvalue = mox.value()

           with mox.stubout(Handler, 'send') as mock_send:
               # so we use remember in the send call, and its value then the function is
               # called will go to `myvalue`
               mock_send(mox.remember(myvalue))

           Manager([Handler()]).process(mydict)
           mox.verify(mock_send)

           # now we can compare myvalue with what we think its value must be
           assert myvalue == {6: {2: 3, 4: {1: 'orange', 7: 8}}, 8: 3}

Classic Way
~~~~~~~~~~~

.. code:: python

   import mox
   import os

   class TestOs:
       def test_getcwd(self):
           m = mox.Mox()

           m.stubout(os, 'getcwd')
           # calls
           os.getcwd().returns('/mox/path')

           m.replay_all()
           assert os.getcwd() == '/mox/path'
           m.verify_all()


   if __name__ == '__main__':
       import unittest
       unittest.main()

Jurassic Way
~~~~~~~~~~~~

.. code:: python

   import mox
   import os


   class TestOs(mox.MoxTestBase):
       def test_getcwd(self):
           self.mox.StubOutWithMock(os, 'getcwd')
           # calls
           os.getcwd().AndReturn('/mox/path')

           self.mox.ReplayAll()
           self.assertEqual(os.getcwd(), '/mox/path')
           self.mox.VerifyAll()


   if __name__ == '__main__':
       import unittest
       unittest.main()

Next
~~~~

That’s it for now! For a more comprehensive tutorial, see:
https://pymox.readthedocs.io/en/latest/tutorial.html

For more examples, see:
https://pymox.readthedocs.io/en/latest/examples.html

For the API reference, see:
https://pymox.readthedocs.io/en/latest/reference.html

Documentation
-------------

For full documentation, including installation, tutorials and PDF
documents, please see http://pymox.rtfd.io/.

.. |package-version| image:: https://badge.fury.io/py/pymox.svg
.. |python-versions| image:: https://img.shields.io/pypi/pyversions/pymox.svg
.. |Documentation Status| image:: https://readthedocs.org/projects/pymox/badge/?version=latest

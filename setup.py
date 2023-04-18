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
from os.path import dirname
from os.path import join

from setuptools import find_packages

try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup


def read(*names, **kwargs):
    return io.open(
        join(dirname(__file__), *names),
        encoding=kwargs.get('encoding', 'utf8')
    ).read()


classifiers = """
Environment :: Console
Development Status :: 5 - Production/Stable
Framework :: Pytest
Intended Audience :: Developers
License :: OSI Approved :: Apache Software License
Operating System :: OS Independent
Programming Language :: Python
Programming Language :: Python :: 3.3
Programming Language :: Python :: 3.4
Programming Language :: Python :: 3.5
Programming Language :: Python :: 3.6
Programming Language :: Python :: 3.7
Programming Language :: Python :: 3.8
Programming Language :: Python :: 3.9
Programming Language :: Python :: 3.10
Programming Language :: Python :: 3.11
Topic :: Software Development :: Quality Assurance
Topic :: Software Development :: Testing
Programming Language :: Python :: Implementation :: CPython
Programming Language :: Python :: Implementation :: PyPy
Topic :: Utilities
"""

classifier_list = [c for c in classifiers.split("\n") if c]
requirements_path = join(dirname(__file__), 'requirements.txt')
with open(requirements_path, "r") as f:
    install_reqs = f.readlines()

setup(
    name='pymox',
    url='http://pymox.rtfd.io',
    license='Apache License, Version 2.0',
    description='Mock object framework',
    include_package_data=True,
    use_scm_version=True,
    setup_requires=['pytest-runner', 'setuptools_scm'],
    install_requires=install_reqs,
    tests_require=['tox', 'pytest', 'pytest-cov', 'coverage'],
    long_description=('%s\n%s' % (
        read('README.rst'),
        re.sub(':[a-z]+:`~?(.*?)`', r'``\1``', read('CHANGELOG.rst')))
    ),
    author='Ivan Neto',
    author_email='ivan.cr.neto@gmail.com',
    packages=find_packages('.'),
    package_dir={'': '.'},
    zip_safe=False,
    classifiers=classifier_list,
    keywords=[
        'mox', 'mock', 'test', 'mocking', 'unittest', 'pymox',
    ],
    python_requires='>=3.3',
    extras_require={},
)

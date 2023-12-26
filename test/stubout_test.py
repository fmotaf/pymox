#!/usr/bin/python2.4
#
# Unit tests for stubout.
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

# Python imports
import unittest

# Internal imports
import mox
from mox import stubbingout

from . import stubout_testee


class StubOutForTestingTest(unittest.TestCase):
    def setUp(self):
        self.mox = mox.Mox()
        self.sample_function_backup = stubout_testee.sample_function

    def tearDown(self):
        stubout_testee.SampleFunction = self.sample_function_backup

    def testSmartSetOnModule(self):
        mock_function = self.mox.create_mock_anything()
        mock_function()

        stubber = stubbingout.stubout()
        stubber.smart_set(stubout_testee, "sample_function", mock_function)

        self.mox.replay_all()

        stubout_testee.sample_function()

        self.mox.verify_all()


if __name__ == "__main__":
    unittest.main()

class Create:
    def __init__(self, mox_obj):
        self.mox = mox_obj

    def __enter__(self):
        self.mox._mock_objects = []
        return self.mox

    def __exit__(self, exc_type, exc_value, exc_tb):
        pass


class Expect:
    def __init__(self, mox_obj):
        self.mox = mox_obj

    def __enter__(self):
        return self.mox

    def __exit__(self, exc_type, exc_value, exc_tb):
        self.mox.replay_all()


class MockObjectExpect:
    def __init__(self, mock_obj):
        self.mock_object = mock_obj

    def __enter__(self):
        return self.mock_object

    def __exit__(self, exc_type, exc_value, exc_tb):
        self.mock_object._replay()

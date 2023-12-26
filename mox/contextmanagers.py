class Create:
    def __init__(self):
        import mox

        self.m = mox.Mox()

    def __call__(self, class_to_mock, attrs=None):
        return self.m.create_mock(class_to_mock, attrs)

    def any(self, description=None):
        return self.m.create_mock_anything(description=description)


create = Create()


class Stubout:
    def __init__(self, *stub, _class=False):
        import mox

        self.stubs = [stub] if stub else []
        self._class = _class
        self.m = mox.Mox()

    def __enter__(self):
        stub_objs = []
        if self._class:
            method = getattr(self.m, "stubout_class")
        else:
            method = getattr(self.m, "stubout")

        for stub_args in self.stubs:
            obj, attr_name, *use_mock_anything = stub_args
            kwargs = {} if self._class else {"use_mock_anything": bool(use_mock_anything)}

            stub_obj = method(obj=obj, attr_name=attr_name, **kwargs)
            stub_objs.append(stub_obj)

        if len(stub_objs) == 1:
            return stub_objs[0]
        return stub_objs

    def __exit__(self, exc_type, exc_value, exc_tb):
        self.m.replay_all()

    @classmethod
    def many(cls, *stubs):
        cm = cls()
        cm.stubs = stubs
        return cm

    @classmethod
    def klass(cls, *stub):
        cm = cls(*stub, _class=True)
        return cm


stubout = Stubout


class Expect:
    def __init__(self, *stub, mox_obj=None):
        self.stubs = [*stub] if stub else []
        self.mox_obj = mox_obj

    def __enter__(self):
        import mox

        mox.reset(*self.stubs)
        if self.mox_obj:
            self.mox_obj.reset_all()

        if len(self.stubs) == 1:
            return self.stubs[0]
        return self.stubs

    def __exit__(self, exc_type, exc_value, exc_tb):
        import mox

        if not self.mox_obj and not self.stubs:
            mox.Mox.global_replay()
            return

        if self.mox_obj:
            self.mox_obj.replay_all()
        if self.stubs:
            mox.replay(*self.stubs)

    def __call__(self, *stubs):
        self.stubs = stubs
        return self

    @classmethod
    def from_mox(cls, mox_obj):
        cm = cls(mox_obj=mox_obj)
        return cm


expect = Expect()

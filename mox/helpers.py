# Python imports
import pkgutil
import sys
from functools import partial, wraps

# Internal imports
from mox.exceptions import ObjectResolutionError


def resolve_object(func):
    """Resolves an object and its attribute before calling the function in case a reference/path is pased."""

    def import_object(path):
        try:
            obj, attribute = path.rsplit(".", 1)
        except (TypeError, ValueError, AttributeError):
            raise ObjectResolutionError(path)

        if sys.version_info < (3, 9):
            raise ObjectResolutionError(path)
        return partial(pkgutil.resolve_name, obj), attribute

    @wraps(func)
    def wrapper(self, *args, **kwargs):
        obj, attr_name = None, None

        if len(args) >= 1:
            obj = args[0]

        if len(args) >= 2:
            attr_name = args[1]

        if "obj" in kwargs:
            obj = kwargs.pop("obj")
        if "attr_name" in kwargs:
            attr_name = kwargs.pop("attr_name")

        if isinstance(obj, str):
            path = f"{obj.attr_name}" if attr_name else obj
            obj, attr_name = import_object(path)
            obj = obj()

        result = func(self, obj, attr_name, *args[2:], **kwargs)
        return result

    return wrapper

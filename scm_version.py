# Python imports
import pathlib

# Pip imports
from hatchling.metadata.core import ProjectMetadata
from hatchling.plugin.manager import PluginManager


def get_version():
    root = pathlib.Path(__file__).parent
    plugin_manager = PluginManager()
    metadata = ProjectMetadata(root, plugin_manager)

    source = metadata.hatch.version.source

    version_data = source.get_version_data()
    return version_data["version"]


def write_toml(filepath="pyproject.toml", reset=True):
    new_contents = []
    with open(filepath, "r") as infile:
        contents = infile.readlines()

    version_contents = (
        'version = "0.0.0"  # DO NOT CHANGE VERSION HERE\n' if reset else f'version = "{get_version()}"\n'
    )

    for line in contents:
        if line.startswith("version ="):
            new_contents.append(version_contents)
        else:
            new_contents.append(line)
    with open(filepath, "w") as outfile:
        outfile.writelines(new_contents)


def main():
    print(get_version())


if __name__ == "__main__":
    main()

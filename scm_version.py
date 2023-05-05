from setuptools_scm import get_version


def write_toml(filepath="pyproject.toml", reset=True):
    new_contents = []
    with open(filepath, "r") as infile:
        contents = infile.readlines()

    version_contents = (
        f'version = "0.0.0"  # DO NOT CHANGE VERSION HERE\n'
        if reset
        else f'version = "{get_version()}"\n'
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

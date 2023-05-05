from setuptools_scm import get_version


def write_toml(filepath="pyproject.toml", reset=True):
    contents = []
    new_contents = []
    with open(filepath, "r") as infile:
        contents = infile.readlines()
    for line in contents:
        if line.startswith("version ="):
            if reset:
                new_contents.append(f'version = "0.0.0"  # DO NOT CHANGE VERSION HERE\n')
            else:
                new_contents.append(f'version = "{get_version()}"\n')
        else:
            new_contents.append(line)
    with open(filepath, "w") as outfile:
        outfile.writelines(new_contents)


def main():
    print(get_version())


if __name__ == "__main__":
    main()
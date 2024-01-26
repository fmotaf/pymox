#!/usr/bin/env bash
if [[ -z "$1" ]]; then
  echo "Missing argument. Enter <version_number>,minor,patch,major,releace,rc or a combination of them."
  exit 1
fi

poetry run python -c "import tools.versioning; tools.versioning.write_toml(source='semver')"
version=$(poetry run hatch version "$1" 2>/dev/null)
if [ $? -eq 0 ]; then
  version=$(poetry run hatch version 2>/dev/null)
    if [ $? -eq 0 ]; then
      if [ -n "$version" ]; then
        echo "new version: $version"
        git tag "v$version"
      else
        echo "Error: Unable to extract version number."
        exit 1
      fi
    else
      echo "Error: poetry run hatch version command failed."
    fi
else
    echo "Error: poetry run hatch version $1 command failed."
    exit 1
fi

poetry run python -c "import tools.versioning; tools.versioning.write_toml(source='scm')"
poetry run python -m hatch build --hooks-only

#!/usr/bin/env bash
/usr/bin/env poetry run python -c "import scm_version; scm_version.write_toml(reset=False)"
/usr/bin/env poetry run python -m hatch build
/usr/bin/env poetry run python -c "import scm_version; scm_version.write_toml(reset=True)"

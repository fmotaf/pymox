#!/usr/bin/env bash
/usr/bin/env python -m setuptools_scm
/usr/bin/env python -c "import scm_version; scm_version.write_toml(reset=False)"
/usr/bin/env poetry build
/usr/bin/env python -c "import scm_version; scm_version.write_toml(reset=True)"

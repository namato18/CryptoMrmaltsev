library(reticulate)

virtualenv_create("my-python", python_version = "3.8.2")

use_virtualenv("my-python", required = TRUE)
virtualenv_install(envname = "my-python", "pandas", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "my-python", "requests", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "my-python", "datetime", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "my-python", "numpy", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "my-python", "matplotlib", ignore_installed = FALSE, pip_options = character())

sourcepy
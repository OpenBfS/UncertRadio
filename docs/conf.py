# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'UncertRadio'
copyright = '2024, Günter Kanisch'
author = 'Günter Kanisch, Florian Ober, Marc-Oliver Aust'
version = '2.5.1'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['myst_parser']

exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

language = 'en'  # default language

html_static_path = ['_static']
html_theme_options = {
    'logo': 'UR2_logo.png',
    'github_user': 'OpenBfS',
    'github_repo': 'UncertRadio',
    'logo_name': True,
    'github_banner': True,
    'page_width': '1200px',
    }

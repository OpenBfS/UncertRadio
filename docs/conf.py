# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

import sys
import subprocess


def copy_file_remove_lines(input_file, output_file, lines_to_remove):
    with open(input_file, 'r') as file, open(output_file, 'w') as output:
        lines = file.readlines()
        lines_to_remove = [line - 1 for line in lines_to_remove]  # Convert to 0-indexed
        lines_to_remove.sort(reverse=True)  # Sort in descending order to avoid index shifting
        for line in lines_to_remove:
            if line < len(lines):
                lines.pop(line)
        output.writelines(lines)


project = 'UncertRadio'
copyright = '2024, Günter Kanisch'
author = 'Günter Kanisch, Florian Ober, Marc-Oliver Aust'
version = subprocess.check_output(['git', 'describe', '--tags', '--abbrev=0']).decode('utf-8').strip()

copy_file_remove_lines("../README.md", "README.md", [1, 2])

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['myst_parser']

exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

language = 'en'  # default language

html_static_path = ['_static']
html_css_files = ['css/alabaster_custom.css']

html_theme_options = {
    'logo': 'UR2_logo.png',
    'github_user': 'OpenBfS',
    'github_repo': 'UncertRadio',
    'logo_name': True,
    'github_banner': True,
    'page_width': '1200px',
    # 'html_favicon': 'UR2_logo.png'
    }

if 'htmlhelp' in sys.argv:
    extensions.append('sphinx.ext.imgmath')
    imgmath_font_size = 14
    html_theme_options['github_banner'] = False
    html_theme_options['page_width'] = '900px'

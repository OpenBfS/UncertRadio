# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

import sys
import os

project = 'UncertRadio'
copyright = '2026, Günter Kanisch'
author = 'Günter Kanisch, Florian Ober, Marc-Oliver Aust'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

html_static_path = ['_static']
templates_path = ['_templates']
html_css_files = ['css/custom.css', 'css/language_version_selector_styles.css']
extensions = ['myst_parser', 'sphinx.ext.autosectionlabel']
locale_dirs = ['locale/']
gettext_compact = False
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'tbd']
numfig = True
numfig_secnum_depth = 2

# available languages atm
AVAIL_LANGUAGES = os.environ.get('AVAIL_LANGUAGES', 'en').split(',')
AVAIL_VERSIONS = os.environ.get('AVAIL_VERSIONS', 'latest').split(',')

# set version
version_now = os.environ.get('ver_now', AVAIL_VERSIONS[0])

# set language
language = os.environ.get('lang_now', AVAIL_LANGUAGES[0])

language_dict = dict()
for lang in AVAIL_LANGUAGES:
    if lang == 'en':
        if language == 'en':
            language_dict['en'] = ''
        else:
            language_dict['en'] = '../'
    else:
        if lang == language:
            language_dict[lang] = ''
        elif language == 'en':
            language_dict[lang] = lang + '/'
        else:
            language_dict[lang] = '../' + lang + '/'

version_dict = dict()
for ver in AVAIL_VERSIONS:
    if language == 'en':
        version_dict[ver] = '../' + ver + '/'
    else:
        version_dict[ver] = '../../' + ver + '/' + language + '/'


html_context = {
    'version_now': version_now,
    'versions': version_dict,
    'languages': language_dict,
    'language_now': language
}

html_theme = "alabaster"

html_sidebars = {
    '**': [
        'about.html',
        'language_version_selector.html',
        'navigation.html',
        'searchfield.html',
    ]
}

html_theme_options = {
    'fixed_sidebar': True,
    'logo': 'ur2_symbol.png',
    'github_user': 'OpenBfS',
    'github_repo': 'UncertRadio',
    'github_banner': True,
    'page_width': '1200px',
    'sidebar_includehidden': True,
    }

# html_theme = "sphinx_rtd_theme"

# html_theme_options = {

#     'vcs_pageview_mode': '',
#     # Toc options
#     'collapse_navigation': True,
#     'sticky_navigation': True,
#     'navigation_depth': 4,
#     'includehidden': True,
#     'titles_only': False
# }


if 'htmlhelp' in sys.argv:
    extensions.append('sphinx.ext.imgmath')
    imgmath_font_size = 14
    html_theme_options['github_banner'] = False
    html_theme_options['page_width'] = '900px'
    # html_theme_options['show_powered_by'] = False
    html_show_sphinx = False
    html_show_sourcelink = False

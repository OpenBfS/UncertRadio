# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

import sys
import os

project = 'UncertRadio'
copyright = '2025, Günter Kanisch'
author = 'Günter Kanisch, Florian Ober, Marc-Oliver Aust'
version = os.environ.get('version', '2.5.x')

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

html_static_path = ['_static']
templates_path = ['_templates']
html_css_files = ['css/custom.css', 'css/language_version_selector_styles.css']
extensions = ['myst_parser']
locale_dirs = ['locale/']
gettext_compact = False
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'tbd']

# available languages atm
AVAIL_LANGUAGES = os.environ.get('AVAIL_LANGUAGES', '').split(',')

# set language and use 'en' as fallback
language_now = os.environ.get('lang', AVAIL_LANGUAGES[0])
if language_now not in AVAIL_LANGUAGES:
    language_now = 'en'

language_dic = dict()
for lang in AVAIL_LANGUAGES:

    if lang == 'en':
        if language_now == 'en':
            language_dic['en'] = ''
        else:
            language_dic['en'] = '../'
    else:
        if lang == language_now:
            language_dic[lang] = ''
        elif language_now == 'en':
            language_dic[lang] = lang + '/'
        else:
            language_dic[lang] = '../' + lang + '/'

html_context = {
    'languages': language_dic,
    'default_language': language_now
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
    'logo': 'UR2_logo.png',
    'github_user': 'OpenBfS',
    'github_repo': 'UncertRadio',
    'github_banner': True,
    'page_width': '1200px',
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

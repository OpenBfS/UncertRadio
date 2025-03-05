#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# use this python script to create every docs (supported languages (and versions)) for UR.
#
import subprocess
import os
import shutil


def build_doc(version, lang, builder='html'):
    if version == 'main':
        os.environ["version"] = 'latest'
    else:
        os.environ["version"] = version

    # subprocess.run(['git', 'checkout', version])
    os.environ["lang"] = lang

    os.environ['SPHINXOPTS'] = f"-D language='{lang}'"
    subprocess.run(['make', builder])


# set global Variables
OUTPUT_DIR = "final"
BUILD_DIR = "_build"
# BUILDERS = ['html', 'htmlhelp', 'latexpdf']
BUILDERS = ['html']

# check the locale directory for supported languages
locale_dir = 'locale'
avail_languages = ['en']

for dir_name in os.listdir(locale_dir):
    dir_path = os.path.join(locale_dir, dir_name)
    if os.path.isdir(dir_path):
        if dir_name != 'en':
            avail_languages.append(dir_name)

os.environ['AVAIL_LANGUAGES'] = ','.join(avail_languages)

# first remove all old docs
if os.path.exists(BUILD_DIR):
    subprocess.run(['make', 'clean'])

if os.path.exists(OUTPUT_DIR):
    shutil.rmtree(OUTPUT_DIR)


# copy all icons
# check if the destination directory already exists
if os.path.exists('icons'):
    # If it exists, remove it to avoid FileExistsError
    shutil.rmtree('icons')

shutil.copytree('../icons', 'icons')
shutil.copy('icons/ur2_symbol.png', '_static/ur2_symbol.png')

# now iterate over the different builders:
for lang in avail_languages:
    for builder in BUILDERS:
        # create docs for the latest main branch
        build_doc(version='main', lang=lang, builder=builder)
        if lang == 'en':
            shutil.move(BUILD_DIR + '/' + builder, OUTPUT_DIR + '/' + builder)
        else:
            shutil.move(BUILD_DIR + '/' + builder, OUTPUT_DIR + '/' + builder + '/' + lang)

        # versions = ["v2.5.3", "v2.6.0", "main"]
        # versions = ["v2.5.3"]
        # for version in versions:
        #     version = versions[0]

        #     # checkout the selected version

        #     for lang in languages:
        #

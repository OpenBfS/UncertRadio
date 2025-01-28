#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# use this python script to create every docs (supported languages (and versions)) for UR.
#
import subprocess
import os
import shutil

OUTPUT_DIR = "final"
BUILD_DIR = "_build"


def copy_file_remove_lines(input_file, output_file, lines_to_remove):
    with open(input_file, 'r') as file, open(output_file, 'w') as output:
        lines = file.readlines()
        lines_to_remove = [line - 1 for line in lines_to_remove]  # Convert to 0-indexed
        lines_to_remove.sort(reverse=True)  # Sort in descending order to avoid index shifting
        for line in lines_to_remove:
            if line < len(lines):
                lines.pop(line)
        output.writelines(lines)


def build_doc(version, lang, builder='html'):
    if version == 'main':
        os.environ["version"] = 'latest'
    else:
        os.environ["version"] = version

    # subprocess.run(['git', 'checkout', version])
    os.environ["lang"] = lang

    os.environ['SPHINXOPTS'] = f"-D language='{lang}'"
    subprocess.run(['make', builder])


# first remove all old docs
if os.path.exists(BUILD_DIR):
    subprocess.run(['make', 'clean'])

if os.path.exists(OUTPUT_DIR):
    shutil.rmtree(OUTPUT_DIR)
# copy the corresponding README file and remove the first two lines
copy_file_remove_lines("../README.md", "README.md", [1, 2])

# copy the README image
shutil.copy('../icons/UR2MC_EN.png', 'media/UR2MC_EN.png')

# copy the icon to _static
shutil.copy('../icons/ur2_symbol.png', '_static/UR2_logo.png')

# now iterate over the different builders:
BUILDERS = ['html', 'htmlhelp', 'latexpdf']
BUILDERS = ['html']
LANGUAGES = ['en', 'de', 'fr']

for lang in LANGUAGES:
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

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


# now iterate over the different builders:
BUILDERS = ['html', 'htmlhelp', 'latexpdf']
BUILDERS = ['html']
for builder in BUILDERS:
    # create docs for the latest main branch
    build_doc(version='main', lang='en', builder=builder)
    shutil.move(BUILD_DIR + '/' + builder, OUTPUT_DIR + '/' + builder)
    build_doc(version='main', lang='de', builder=builder)
    shutil.move(BUILD_DIR + '/' + builder, OUTPUT_DIR + '/' + builder + '/de')

    # versions = ["v2.5.3", "v2.6.0", "main"]
    # versions = ["v2.5.3"]
    # for version in versions:
    #     version = versions[0]

    #     # checkout the selected version
    #
    #     copy_file_remove_lines("../README.md", "README.md", [1, 2])
    #     for lang in languages:
    #     # copy the corresponding README file and remove the first two lines

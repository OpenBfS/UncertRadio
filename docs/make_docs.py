#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# use this python script to create every docs (supported languages (and versions)) for UR.
#
import subprocess
import os
import shutil
import re


def replace_word_in_file(file_path, target_word, replacement_word):
    """
    Replaces a target word in a specified file with a replacement word.

    Args:
        file_path (str): Path to the file where the replacement should be made.
        target_word (str): The word to be replaced.
        replacement_word (str): The word to replace the target word with.

    Returns:
        None
    """

    try:
        with open(file_path, 'r') as file:
            file_data = file.read()

        # Use re.sub with word boundaries to match whole words, and re.IGNORECASE to ignore case sensitivity
        # Also, escape the target word to prevent it from being treated as a regex pattern
        file_data = re.sub(r'\b' + re.escape(target_word) + r'\b',
                           replacement_word,
                           file_data,
                           flags=re.IGNORECASE)

        with open(file_path, 'w') as file:
            file.write(file_data)

    except FileNotFoundError:
        print(f"File {file_path} not found")
    except Exception as e:
        print(f"An error occurred: {e}")


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
# copy the corresponding README file and remove the first two lines
copy_file_remove_lines("../README.md", "README.md", [1, 2])

# copy the README image
shutil.copy('../icons/UR2MC_EN.png', 'media/UR2MC_EN.png')
# update the icon path
replace_word_in_file("README.md", 'icons', 'media')

# copy the icon to _static
shutil.copy('../icons/ur2_symbol.png', '_static/UR2_logo.png')

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

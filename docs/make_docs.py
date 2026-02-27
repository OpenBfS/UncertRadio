#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# use this python script to create every docs (supported languages (and versions)) for UR.
#
import subprocess
import os
import shutil

OUTPUT_DIR = "final"
BUILD_DIR = "_build"
# BUILDERS = ['html', 'htmlhelp', 'latexpdf']
BUILDERS = ["html"]

# check the locale directory for supported languages
locale_dir = "locale"

# first remove all old docs
if os.path.exists(BUILD_DIR):
    subprocess.run(["make", "clean"])

if os.path.exists(OUTPUT_DIR):
    shutil.rmtree(OUTPUT_DIR)

# copy all icons
# check if the destination directory already exists
if os.path.exists("icons"):
    # If it exists, remove it to avoid FileExistsError
    shutil.rmtree("icons")

shutil.copytree("../icons", "icons")
shutil.copy("icons/ur2_symbol.png", "_static/ur2_symbol.png")

# set global Variables
if os.environ.get("AVAIL_VERSIONS"):
    VERSIONS = os.environ.get("AVAIL_VERSIONS").split(",")
else:
    VERSIONS = ["latest"]
    os.environ["AVAIL_VERSIONS"] = "latest"

# now iterate over the different builders:
for builder in BUILDERS:
    for ver in VERSIONS:
        os.environ["version_now"] = ver
        avail_languages = ["en"]
        for dir_name in os.listdir(locale_dir):
            dir_path = os.path.join(locale_dir, dir_name)
            if os.path.isdir(dir_path):
                if dir_name != "en":
                    avail_languages.append(dir_name)

        os.environ["AVAIL_LANGUAGES"] = ",".join(avail_languages)

        for lang in avail_languages:
            os.environ["lang_now"] = lang
            os.environ["SPHINXOPTS"] = f"-D language='{lang}'"

            # create docs for the latest main branch
            git_version = "main" if ver == "latest" else ver
            if len(VERSIONS) > 1:
                cmd = ["git",
                       "restore",
                       f"--source={git_version}",
                       "--worktree",
                       "--",
                       "doc_files",
                       "locale"]
                subprocess.run(cmd)
            subprocess.run(["make", builder])

            if lang == "en":
                shutil.move(BUILD_DIR + "/" + builder,
                            OUTPUT_DIR + "/" + builder + "/" + ver)
            else:
                shutil.move(BUILD_DIR + "/" + builder,
                            OUTPUT_DIR + "/" + builder + "/" + ver + "/" + lang)
    if builder == 'html':
        shutil.copy("_templates/redirect_index.html", OUTPUT_DIR + "/" + builder + "/index.html")

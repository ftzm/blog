---
title: Syncing Dotfiles in Git
published: 2015-09-23
teaser: Don't waste time fighting muscle memory. With a few easy git commands you can replicate your standard working environment wherever you go.
tags: [ricing, git]
---

One of the greatest joys of working in a Unix environment is the ability to tailor application settings exactly to your liking. However, it can equally be a pain to work on a machine without any of the keybinds and plugins you've become so accustomed to. Syncing your dotfiles across machines using [git](https://github.com/) is an easy way to ensure maximum productivity wherever you work.

## Setup

To create an online repository based on the setting of one machine, first set up a git repository in home directory of that machine:

```bash
$ cd
$ git init
```

In order to make git ignore everything that *isn't* a dotfile, we make use of the .gitignore file. By adding a single wildcard (asterisk) to that file, git will be default ignore all files:

```bash
$ echo "*" >> ~/.gitignore
$ git add .gitignore
```

then, we force add all of the files we want to sync:

```bash
$ git add -f .vimrc
```

To add whole folders, I find it easiest to cd into them and run:

```bash
$ git add -f .
```

When you've added all the files you wish, add the remote repo and push your files:

```bash
$ git remote add origin https://github.com/youruser/dotfiles.git
$ git push origin master
```

## Sync a System with Git for the First Time:

git clone won't work as git needs to clone into an empty directory, which the home directory will invariably not be.

Instead, initialize a git repo in the home directory:

```bash
$ cd
$ git init
```

Then, remote add the origin repository (on github in this case)

```bash
$ git remote add origin https://github.com/youruser/dotfiles.git
```

Fetch the files from the origin repository

```bash
$ git fetch --all
```

Then, reset the current branch to what has just been fetched. 'Git reset' resets the master branch to what's been fetched. The --hard option changes all the files in the working tree to match the files in origin/master."

```bash
$ git reset --hard origin/master
```

This method is preferable to a git pull, as a pull will not overwrite untracked local files, in the event that any local dotfiles will be overwritten by syncing with git.

## Add New Changes

To add new changes you've made on any machine, the procedure is the same as with a normal git project, but with the -f flag as above:

```bash
$ git add -f .Xresources
$ git commit -m 'updated colorscheme'
(first time) $ git push origin master
(thereafter) $ git push
```

## Keeping up to Date

In order to keep up to date, a simple git pull will suffice. You may consider put this into a login script or cron job to keep things current automatically.

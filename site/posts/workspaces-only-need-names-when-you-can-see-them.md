---
title: Flexible workspace names in i3
published: 2015-12-05
teaser: A little trick for more convenient workspace naming in i3 (and potentially other WMs).
tags: [bash, linux, ricing, i3]
---

# Introduction

In the process of configuring i3 with Lemonbar I came up with a trick to simplify naming workspaces in i3, and potentially other WMs with similar configuration patterns.

# The Trick

i3 natively provides the ability to name workspaces, but this is arguably more trouble than it's worth. All keybindings relating to workspaces need to reference them by the assigned name, and workspace names typically need to be prefixed with a number to  ensure they're ordered properly anyways (should a strict order be desired).

If, however, one is using a custom bar script, workspace names can be assigned there rather than by i3. You can simply use numbers within i3's configuration script, but later convert them to names using something like the following:

```Bash
names=(main web dev term mus)

if [ $[num-1] -lt ${#names[@]} ] ; then
    name="${names[num-1]}"
else
    name="${num}"
fi
```

This method allows workspaces to maintain a strict order without number prefixes, and keybindings and other configuration need only reference workspaces by  number, keeping configs simple. Workspace names can then be changed on the fly without        overhauling any configs, which will save a bit of time for chronic re-themers.

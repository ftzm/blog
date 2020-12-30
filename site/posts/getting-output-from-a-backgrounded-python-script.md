---
title: Getting Output from a Backgrounded Python Script
published: 2015-12-02
teaser: A quick note on getting python scripts to print while backgrounded.
tags: [python, bash, linux]
---

I've rarely had occasion to run a python script in the background. I tend to use them for one-off tasks, and if they print to stdout I want the output immediately. I was therefore baffled when [my python script to receive workspace info from i3](custom-workspace-info-with-i3-sockets-and-python) refused to pipe to [Lemonbar](https://github.com/LemonBoy/bar). Shouldn't it be as straightforward as piping output from a bash script? After a little toubleshooting I discovered that it is--so long as the script isn't backgrounded. When backgrounded, however, the default behavior is not to print until the process finishes. This is due to Python's output buffering.

Thankfully, the solution to this problem is simple.  To enable unbuffered, realtime output, run python with the -u flag:

```bash
$ python -u myscript.py
```

With that, everything works as expected.

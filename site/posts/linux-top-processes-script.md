---
title: Linux Top Processes Script
published: 2015-12-04
teaser: I wrote a script to print the processes using the most resources, because a statusbar showing total resource usage alone isn't actually very useful.
tags: [bash, linux, ricing]
---

It's common to see measurements of total cpu and memory usage featuring somewhere on a Linux power user's desktop. At first blush it seems sensible, but I've come to question the usefulness of these numbers. I'll sooner notice high fan noise or poor performance than a percentage sitting at the edge of my screen. Further, I'm seldom concerned with resource usage unless it's excessively high, in which case I'm mostly interested in what's responsible.

I decided to do something a little different in my latest statusbar setup: I put together a bash script to print the top resource-consuming processes. processes from 'ps aux' are reduced to the resource percentage and command, and the command itself is trimmed to show only the key elements. You can specify whether to measure by cpu or memory usage, and how many of the top processes to print. I also added the option to specify a usage threshold under which processes aren't reported, because there's little utility in knowing that X is using 0.5% cpu.

The full script is provided below.

```Bash
#!/bin/bash

#defaults
metric="3,3" # measure cpu
percent="3"
number="3"
cutoff=0

while [[ $# > 0 ]]; do
	key="$1"
	case $key in
		-m|--memory)
			metric="4,4" # measure memory
			percent="4"
			shift
			;;
		-n|--number)
			number=$2
			shift
			;;
		-c|--cutoff)
			cutoff=$2
			shift
			;;
		*)
			shift
			;;
	esac
done

procs=`ps aux | tail -n +2 | sort -n -rk $metric | head -n $number | awk -v \\
	perccol=$percent '{print $perccol" "substr($0, index($0,$11))}'`

IFS=$'\\n'

if [ $cutoff != 0 ]; then
	remaining_procs=''
	for p in $procs; do
	IFS=" "
		set -- $p
		if (( $(bc <<< "$1 > $cutoff") )); then
			remaining_procs+=$(printf "\\n$p")
		fi
	IFS=$'\\n'
	done
	procs=$remaining_procs
fi

line=""
for p in $procs; do

	IFS=" "
	set -- $p
	name2=""

	percent=$1
	shift

	if [[ $1 == /* ]];  then
		if [[ $1 == *bin || $1 == *python ]]; then
			shift
			while [[ $# > 0 ]]; do
				if [[ $1 != -* ]]; then
IFS="/"
	dir=( $1 )
		name1="${dir[-1]}"
	IFS=" "
		break
fi
shift
	done
		elif [[ $2 == /* ]]; then
			IFS="/"
			dir=( $2 )
			name1=${dir[-1]}
			IFS=" "
		else
			IFS="/"
			dir=( $1 )
			name1=${dir[-1]}
			IFS=" "
		fi
	else
		name1=$1
		shift
		while [[ $# > 0 ]]; do
			if [[ $1 != -* ]]; then
				IFS="/"
					dir=( $1 )
						name2="-${dir[-1]}"
							IFS=" "
								break
								fi
								shift
							done
	fi

	line="$line $percent $name1$name2"
done
printf "%s\\n" "$line"
```

Now I can easily keep an eye out for resource hogs without having to run htop all the time. It's a huge improvement over raw percentage totals.

This script will likely see improvements over time, so to guarantee yourself the most up-to-date version head over to my [script repository](https://github.com/ftzm/scripts).

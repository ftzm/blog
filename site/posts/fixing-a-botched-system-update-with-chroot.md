---
title: Fixing a Botched System Update Without Chroot
published: 2015-10-25
teaser: When you find yourself unable to login to a Linux installation to fix it from within, chroot is usually the answer—but not always. Here I show how to repair a broken Arch Linux system upgrade without chroot.
tags: [arch, linux]
---

When you find yourself unable to login to a Linux installation to fix it from within, chroot is usually the answer—but not always. Here I show how to repair a broken Arch Linux system upgrade without chroot.

The other day I erroneously ran an incomplete upgrade to the [Arch Linux](https://www.archlinux.org/) installation on my laptop, which resulted in my system running an upgraded ncurses package with an outdated bash package. Bash is very much dependent on ncurses, so when I next tried to boot up I found myself unable to log in, instead being sent back to the username prompt at every attempt. My first reaction was that I would have to boot up with external media and [chroot](https://wiki.archlinux.org/index.php/Change_root) into the system to complete the update. In some 8 years of using Arch I've managed to avoid such calamity, but from my reading on the forums the first chroot seems almost rite of passage. So I wrote the latest arch iso to a spare usb, booted up, mounted my filesystem...and hit a wall at the chroot. I found that I could no more chroot into my system than log in; bash was broken, so nothing worked. Which really should have been obvious from the start.

But no matter. A quick bit of research revealed that the solution to my problem was [Pacman's](https://wiki.archlinux.org/index.php/Pacman) "--root" option. Forget chrooting--just mount the on-disk filesystem and run pacman in the external media shell, specifying that the disk filesystem is to be used as the root filesystem. This is really as simple as the following two commands:

```bash
$ mount /dev/sdxX /mnt/something

$ pacman --root /dev/something --cachedir=/mnt/something/var/cache/pacman/pkg -Syu
```

Here xX represents the specific disk and partition of your root filesystem. The --cachedir option tells pacman to save downloaded packages on-disk as well, which one will likely want to do either for the ability to downgrade packages later or simply because the external media lacks space.

The above worked perfectly for me, and my system was up and running within minutes. At my current rate it'll be another 8 years before I'll need this information again, but hopefully this information will help someone else in the meantime.

---
title: Easy Haskell/NixOS setup in a VM
---

I set up a more detailed guide to obtain my setup

-   download the [NixOS-14.04 VM appliance](http://nixos.org/releases/nixos/latest-iso-minimal-x86_64-linux).
-   tune the hardware setting in virtualbox, and set the shared clipboard to bidirectional.
-   start the new virtual machine, login into kde with ```demo/demo```.
-   open konsole:
    + install git: ```nix-env -i git```
    + ```git clone https://github.com/meditans/dotFiles.git```
    + ```sudo -i``` (passw ```demo```)
    + ```/home/demo/dotFiles/setUpFirst.sh```
-   The last command will reboot. Login again with ```carlo/carlo```.
-   Open a terminal with ```Alt+Shift+Enter``` and, to not strain your eyes, open konsole again, then:
    + ```git clone https://github.com/meditans/dotFiles.git```
    + ```dotFiles/setUpSecond.sh```
-   Hit ```Alt+Shift+q``` to exit xmonad and login again (```carlo/carlo```).

Here are some shortcuts for your convenience:

- ```Meta+Shift+Enter``` opens konsole
- ```Meta+e``` opens emacs, haskell-mode and ghc-mod are already configured
- ```Meta+f``` opens firefox

Open konsole and install a package, say ```Control.Monad.Operational``` with:

- ```nix-env -iA nixos.pkgs.haskellPackages.operational```

Enter emacs and write a file which imports ```Control.Monad.Operational```.
 I'm not able to convince ghc-mod that that package exist
(although it is included in ```ghc-mod --list```)

It's not only an ```Operational``` issue, either, it happens with many, if
not all, packages outside base.
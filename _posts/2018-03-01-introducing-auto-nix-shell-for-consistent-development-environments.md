---
title: Introducing auto-nix-shell for consistent development environments
date: 20180301T00:00Z
---

<script src="https://asciinema.org/a/dFFhCmPULkqtIPLN8PrBjnjK0.js" id="asciicast-dFFhCmPULkqtIPLN8PrBjnjK0" async></script>

I created [auto-nix-shell](https://github.com/chrismwendt/auto-nix-shell) to automatically enter/exit `nix-shell` when `cd`ing into/our of a directory that has a Nix config file (`default.nix`). It even checks for config changes before executing the next shell command (using [`fish_preexec`](https://github.com/fish-shell/fish-shell/pull/1666) and [bash-preexec](https://github.com/rcaloras/bash-preexec)) and reloads the configuration when you `git checkout <some other commit>`

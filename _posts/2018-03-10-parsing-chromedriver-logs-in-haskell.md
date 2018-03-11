---
layout: post
title: Parsing chromedriver logs in Haskell
categories:
- blog
---

chromedriver (the tool for programmatic interaction with Chrome) doesn't have an output format amenable to analysis, so I wrote a Haskell program to convert it to JSON [chromedriver-logs](https://github.com/chrismwendt/chromedriver-logs), which:

- Outputs JSON
- Keeps track of commands and their response times
- Streams log lines
- Ignores junk lines

<script src="https://asciinema.org/a/44dRKLg0E2Mp9CXrZb113IHAK.js" id="asciicast-44dRKLg0E2Mp9CXrZb113IHAK" async></script>

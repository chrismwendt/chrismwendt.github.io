---
layout: post
title: Auto import for Haskell in VS Code
categories:
- blog
---

Today, I hacked together auto imports for Haskell in VS Code: https://github.com/chrismwendt/Auto-Import based on https://github.com/soates/Auto-Import (which currently only supports TypeScript). The code is very rough, and there is a lot of room for improvement. Currently, it only searches for variables on Hoogle.

It's cool to discover other packages and modules that export the same variable name, for example `concurrently` appears in `async` (what I would expect), but also appears in other libraries that generalize `async`'s types:

![](/assets/concurrently-imports.png) <!-- .element height="50%" width="50%" -->

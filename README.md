# ObserverWeb

**This project is incomplete and is not for production use.** 

ObserverWeb is Erlang observer web frontend.

This repository contains code copied from Erlang/OTP.

## Current Release

0.3.0

## Features

### Currently supported

* System
* Load Charts
* Memory Allocators
* Processes (preview)

## TODO

### Fixes

#### Up to 0.2.0

- [x] Upgrade Cowboy from 1 to 2
- [x] Latest Bootstrap
- [x] Latest jQuery
- [x] Memory Allocators chart fix
- [x] Remove Jiffy (now using jsone)
- [x] Fix modal dialog of Connect Nodes

#### Up to 0.2.1

- [x] Remove jQuery

#### Up to 0.3.0

- [x] Use chart.js, remove highcharts.com dependency
- [x] Basic test on distributed nodes

#### TODO

- [ ] Adjust screen layout

### New features

- [ ] Applications
- [ ] Table viewer
- [ ] Trace Overview 

## Usage

```
rebar3 get-deps
rebar3 compile
rebar3 release
rebar3 shell
````

To start the release in the foreground:

```bash
./_build/default/rel/observerweb/bin/observerweb console
```

Open http://127.0.0.1:8080 in your browser.

## License

Apache License 2.0

### License details

* Copyright (c) 2014-2015 Freecnpro.net
* Copyright (c) 2025 Kenji Rikitake

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License in [LICENSE.txt](LICENSE.txt).

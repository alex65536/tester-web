![Tester Web logo](https://raw.githubusercontent.com/alex65536/tester-web/master/logo/logoWithText.png)

# Table of Contents

* [What is _Tester Web_?](#what-is-tester-web)
* [Advantages and disadvantages of _Tester Web_](#advantages-and-disadvantages-of-tester-web)
* [Pre-requisites](#pre-requisites)
* [Downloading latest release](#downloading-latest-release)
* [Building from sources](#building-from-sources)
* [Installing](#installing)
* [Usage](#usage)
* [License](#license)
* [Libraries used by _Tester Web_](#libraries-used-by-tester-web)

# What is _Tester Web_?

_Tester Web_ is an HTTP web server to hold competitive programming contests. _Tester Web_ is based on [_Tester_](https://github.com/alex65536/tester) testing backend. It is suitable to hold local contests for small amount of participants.

# Advantages and disadvantages of _Tester Web_

The main advantages are:

* _Tester Web_ is very easy to install.
* Simplicity in use (no many options, it's easy to add a problem or a contest to it).
* Problems and contests can be accessed by many users. You can also choose access rights for each user.
* It will work on any machine under _Windows_ and _GNU/Linux_.

The disadvantages are:

* _Tester Web_ is not designed to hold big contests with hundreds of participants and doesn't support distributed testing on many machines.
* No HTTPS support.
* Sometimes _Tester Web_ may be slow (will be optimized in the next versions).
* Currenty there is no sandboxing. _GNU/Linux_ users may try to use the patch for _Linux_ kernel from Ejudge authors [(info)](https://ejudge.ru/wiki/index.php/%D0%9F%D0%B0%D1%82%D1%87_%D0%BA_%D1%8F%D0%B4%D1%80%D1%83_Linux), which can be downloaded at [ejudge.ru](https://ejudge.ru/). This patch should work because _Tester Web_ uses _[Ejudge](https://ejudge.ru/)_ libraries for testing, but I haven't tested this patch yet.  
  **UPD:** If you will build _Tester Web_ yourself and use latest _timerlib_, the Ejudge patch will work, otherwise there will be no sandboxing. Note that is still _GNU/Linux_ only.

# Pre-requisites

## Common

* At first, you will need a computer 😃

* 1 GB RAM (recommended 2 GB or more)

* [_Free Pascal Compiler_](https://freepascal.org/) (for Pascal) and [_GNU GCC Compiler_](https://gcc.gnu.org) (for C/C++) installed. For _Windows_, paths to the compiler binaries must be set in `PATH` environment variable. _Tester web_ **will not launch** without the compliers.

## Windows

* Recommended _Windows_ versions are 7, 8, 8.1 or 10 (64-bit).

## GNU/Linux

* Tested on _Ubuntu 16.04_ (64-bit), but should work on other popular _GNU/Linux_ distros.

* Some checkers for problems may come only in EXE file, so you should install [_Wine_](https://winehq.org) to launch them.

# Downloading latest release

Latest release of _Tester Web_ can be found [here](https://github.com/alex65536/tester-web/releases/latest).

# Building from sources

## Building pre-requisites

### Common

* To build _tester_ from sources, you will need [_Lazarus IDE_](https://www.lazarus-ide.org) (recommended version is 1.6.4) with [_Free Pascal Compiler_](https://freepascal.org/) (recommended version is 3.0.2) and [_GNU GCC Compiler_](https://gcc.gnu.org). Also install _WebLaz_ package into your [_Lazarus IDE_](https://www.lazarus-ide.org).

### Windows

* Use the environment variable `PATH` to specify paths to [_Lazarus IDE_](https://www.lazarus-ide.org) directory and to [_GNU GCC_](https://gcc.gnu.org)'s `bin` directory.

* You will need [_Git Bash_](https://git-for-windows.github.io/).

### Linux

* You will need [_Git_](https://git-scm.com/).

## Building

To build _Tester Web_ from sources, follow these steps:

1. Clone the repository with its submodules:

~~~~
$ git clone https://github.com/alex65536/tester-web
$ cd tester-web
$ git submodule init
$ git submodule update
~~~~

2. Build _Tester_. [Use this reference to do it](https://alex65536.github.io/tester/#building-from-sources). On _GNU/Linux_ **do build** _timerlib_ statically.

3. Build _Tester Web_. Go to `build` directory and run `build_all.sh`. This will build the archive with _Tester Web_ distribution. The archive will be located in `<repo dir>/package.zip`.

# Installing

Installing _Tester Web_ is quite simple:

1. Get the _Tester Web_ distribution archive (download or build it) and unpack it anywhere you want.

2. Run the server program located in `<unpacked archive>/bin/tsweb-...` (the ending of the filename depends on the platform you are using).

3. Now log in as the server owner. Find the username and password in the configuration file stored in `<home dir>/tsweb/data/config.ini`. The keys `defaults.password` and `defaults.userName` in section `[owner]` of the config file store the default username and password. **It's recommended to change the password after your first login.**

4. Now the server it set. Congratulations! 😃

# Usage

Coming soon ...

# License

_Tester Web_ is free software; you can redistribute it and/or modify it under the terms of the [GNU General Public License](https://github.com/alex65536/tester-web/blob/master/LICENSE) as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU General Public License](https://github.com/alex65536/tester-web/blob/master/LICENSE) for more details.

# Libraries used by _Tester Web_

* `fcl-web` which comes with [_Free Pascal Compiler_](https://freepascal.org/), that impements HTTP protocol.

* Some other libraries (can be found in `<repo dir>/third-party`):

	* Hashing library for Pascal, can be found at [http://www.wolfgang-ehrhardt.de/crchash_en.html](http://www.wolfgang-ehrhardt.de/crchash_en.html).

	* [_SyntaxHighlighter_](https://github.com/syntaxhighlighter/syntaxhighlighter), to view sources of submissions.

* [_Tester_](https://github.com/alex65536/tester) also uses some _exec.h_, _exec.c_ and some other source files from [_Ejudge_](https://ejudge.ru/). These files are made up into [_timerlib_](https://github.com/alex65536/tester/tree/master/timerlib).

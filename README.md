Command line utilities written in Common Lisp.

There really isn't anything good here. I'm just fooling around.

The general framework I used was based on Steve Losh's framework that does the same thing.
It can be found here:  https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/

The code here requires [SBCL](https://www.sbcl.org) with [Quicklisp](https://www.quicklisp.org/beta/) installed. SBCL should be on your PATH.

I make sure this works with both macOS and Ubuntu Linux. Running `make` will create
subdirectories (`bin_macos` and `bin_linux`) that will contain the binaries appropriate
to the current operating system. You can then add the entire subdirectory to your PATH
to automagically find those binaries.

If you have Docker installed and want to run the binaries without fooling around with
Common Lisp, check out the scripts in the Docker subdirectory.

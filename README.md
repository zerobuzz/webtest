


WARNING: master is obsolete and 0.2 is unstable.  usable code coming up soon.  please bear with us!



webtest
=======

webtest is a package for web application testing both on HTTP (rest)
and on the application level in the browser.  It makes use of the HTTP
package, the webdriver* package family and Selenium for browser remote
control, and quickcheck for test case generation and shrinking.  It
provides abstract machines that represent states and transitions of an
application.

Both promised features and (particularly) documentation are
incomplete, but are actively improved.  Please let us know when you
are using this!

The structure of the package may change in the future, and things may
be moved into other packages and re-imported here if it makes more
sense.  Suggestions are always welcome!

installation
------------

You need https://github.com/zerobuzz/regex-easy, which is not
available from hackage yet.  Apart from that, the usual
git-clone-cabal-install should work on any system.  please open an
issue if it does not.

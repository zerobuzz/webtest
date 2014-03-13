


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

This software is alpha, and every part of api and internals may keep
changing arbitrarily for a while.

The usual git clone / cabal install should work.  Please open an issue
if it does not.


next steps
----------

Things I hope to work on soon:

 1. Module StaticMachines: support both HTTP and webdriver requests;
    do not rely on types Script and Story from module HTTP.Story, but
    write new analogous types that handle both HTTP and webdriver, so
    that running and compiling to python works for both UI tests and
    rest tests.

 2. Deploy the free package to make implementation of types Script and
    Story more concise and more well-behaved.

 3. Extend implicit testing DSL to an expressive power where
    properties can be translated to python like HTTP already can now.
    (This is not a rational requirement, but one that market forces
    generate: I want webtest to be able to generate comprehensive unit
    test suites so that it can be used as a tool in short-term
    consulting missions in non-haskell projects.

 4. Use sunroof to compile to javascript; replace strings containing
    javascript code from the haskell codebase.



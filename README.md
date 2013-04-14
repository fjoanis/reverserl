reverserl
=========

A practical tutorial about how to program with Erlang.

The goal of this example is to show how to create an Erlang project that isn't
too far from an actual real-world application.

* This is still work in progress. See TODO list at the end. *

License
=======

3-clause BSD

Overview
========

Introducing... reverserl!

Let's build a little Erlang application that does something really simple:
reverse strings.  To show a bit more of what Erlang can do it should also have
a simple web API.

We'll try to cover:

- Some of OTP's principles (gen_server, application, ...)
- Basics of functional programming with Erlang
- The rebar build tool for Erlang
- Generating documentation using rebar and edoc
- Testing using eunit/QuickCheck mini (Proper should work too -
  https://github.com/manopapad/proper)
- ...

Functional Overview
===================

At its core reverserl has a server that handles incoming requests to perform string
reversal operations.

Each request needs to happen within a session:

- Client creates a session with server
- Server responds with a session id
- Client uses session id to call string reversal service
- Client closes the session

On top of that each session should have a timeout to ensure that it is closed
even if the client forgets to do so. Moreover, if a client tries to connect
without a session id or with an invalid session id the server should not allow
it to call its string reversal service.

A simple HTTP-based protocol will be used to implement the exchanges between
the server and its clients.

Architectural Overview
======================

The core of the application is an Erlang gen_server which handles the logic
for:

- Session creation requests
- Session destruction requests
- Dispatch the session "work" - i.e. the reversal algorithm 
- Management of the actual lifetime of the sessions

The last point is important here since it is leveraging Erlang's lightweight
processes: each session will be represented by an actual Erlang process. We
should see that this is an extremely natural fit and that Erlang makes
programming things as they would occur in real life easy because of that
paradigm.

The actual work of reversing the strings is thus done by each session's process
and the dispatching of that work is done by the centralized server..

The HTTP portion uses the cowboy web server - https://github.com/extend/cowboy

Directory Structure
===================

- reverserl
  - deps - Where the dependencies specified by rebar reside
  - src - Where the source code of the application resides
  - test - Where the "unit" tests reside
  - ebin - Where the compiled output resides
  - doc - Where the generated code documentation will reside

Installing Erlang and rebar
===========================

This assumes that you have Erlang installed on your system. You can install it
from source or from pre-compiled binaries (from either http://www.erlang.org or
http://www.erlang-solutions.com).

- Install rebar from github
  - git clone https://github.com/basho/rebar.git
  - cd rebar; ./bootstrap.sh
  - Put the generated rebar executable into your path or directly within the
    project directory we'll create next

Create the core source files using rebar
========================================

Our application resides in its own directory:

    mkdir reverserl

Using rebar, create the core application source files:

    cd reverserl
    rebar create-app appid=reverserl

You should see something like:

    ==> reverserl (create-app)
    Writing src/reverserl.app.src
    Writing src/reverserl_app.erl
    Writing src/reverserl_sup.erl

This created two Erlang source files for two standard OTP components: an
_application_ and a _supervisor_. The third file (.app.src) will contain some
metadata regarding the application. Source files go under the _src_ directory.

An OTP application is the standard way of implemention applications in Erlang.
More information is available here:
http://www.erlang.org/doc/design_principles/applications.html. An OTP
supervisor (http://www.erlang.org/doc/design_principles/sup_princ.html) is a
specialized Erlang process that is responsible to monitor the lifetime of
children processes and act accordingly if they crash (i.e. restart them, ...).

Here we used rebar to have it generate pre-populated source files from its
built-in template files.  Although they should compile file, we'll need to
tweak them a little so that they actually do something.

To compile the current _rebarized_ project:

    rebar compile

You should see something like:

    ==> reverserl (compile)
    Compiled src/reverserl_app.erl
    Compiled src/reverserl_sup.erl

This created compiled Erlang bytecode files (along with the processed .app
file) under the _ebin_ directory:

    /ebin
    ./ebin/reverserl.app
    ./ebin/reverserl_app.beam
    ./ebin/reverserl_sup.beam

On top of that, we also need the following:

- The actual server that will listen for incoming requests
  - That is, _business logic_ requests and not HTTP requests. We'll add those
    later.
- The processes that will be executed when a new session is created. These will
  be spawned by the core server - one per session. They will carry out the
  actual string reversal work.
- A "bridge" between the HTTP server and the main server that listens for the
  requests

Start with the core server. It will use the rebar template for an OTP
gen_server:

    rebar create template=simplesrv srvid="reverserl_server"

You should see:

    ==> reverserl (create)
    Writing src/reverserl_server.erl

Repeat for the session process:

    rebar create template=simplesrv srvid="reverserl_session"

Again, you should see:

    ==> reverserl (create)
    Writing src/reverserl_session.erl

Now everything should still compile:

    rebar clean
    rebar compile

Should yield:

    ==> reverserl (clean)
    ==> reverserl (compile)
    Compiled src/reverserl_session.erl
    Compiled src/reverserl_sup.erl
    Compiled src/reverserl_server.erl
    Compiled src/reverserl_app.erl

Note that not all Erlang source files can be generated using rebar's templates.
For example, the HTTP handler module of reverserl was not. This is simply
because rebar cannot contain templates for every possible module... but it does
contain templates for some really useful ones. 

Implementation
==============

Now is the time to actually get acquainted with some code!

- src/reverserl_app.erl
- src/reverserl_sup.erl
- src/reverserl_server.erl
- src/reverserl_session.erl
- src/reverserl_http_handler.erl
- test/reverserl_http_tests.erl
- test/reverserl_session_tests.erl

For each file listed above, feel free to look at its source. They should also
include useful comments :)

Full Compilation
----------------

    rebar get-deps
    rebar compile

The first command uses git to fetch the source code of the HTTP server we have
listed as a dependency.

Testing
-------

There are two main test files:

    test/reverserl_session_tests.erl
    test/reverserl_http_tests.erl

Both use the eunit testing framework
(http://www.erlang.org/doc/apps/eunit/chapter.html) but the first one also
makes use of QuickCheck Mini (http://www.quviq.com/news100621.html).

QuickCheck Mini implements a framework to do Property Based Testing. There is
also a GPL equivalent called PropEr (https://github.com/manopapad/proper).

To run the tests, make sure everything compiles then run them using rebar:

    rebar compile
    rebar eunit skip_deps=true

The additional argument given to the second command ensures that the unit tests
for the dependencies are not run. If you want to run them - which would be a
good idea - simply remove the skip_dep bit. 

Launching the code manually
---------------------------

    erl -pa ebin -pa deps/ranch/ebin -pa deps/cowboy/ebin

    Erlang R15B03 (erts-5.9.3.1) [source] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
    Eshell V5.9.3.1  (abort with ^G)
    > application:start(crypto).
    ok
    > application:start(ranch).
    ok
    > application:start(cowboy). 
    ok
    > application:start(reverserl).
    Inside reverserl_sup's init function
    Inside reverserl_server's init function
    ok
    > {ok, SessionId1} = reverserl_server:create_session().
    reverserl_session:init
    Creating session "136253516338431"
    {ok,"136253516338431"}
    > reverserl_server:reverse(SessionId1, "Hello World!").
    "!dlroW olleH"
    > reverserl_server:reverse(SessionId1, "Hello World!").
    "!dlroW olleH"
    > reverserl_server:delete_session(SessionId1).         
    Deleting session "136253516338431"
    reverserl_session:terminate - Reason: normal
    ok

TODO
====

- Add more comments to the code
- Complete type specifications -
  http://www.erlang.org/doc/reference_manual/typespec.html
- Show how to run dialyzer
- Test/show that the supervisor actually work if a crash happens in the main
  gen_server.
- Show how to build an Erlang release for the app. A _release_ is like the
  actual package that can be used to distribute the app.
- Show how this would work with an IDE like Erlide


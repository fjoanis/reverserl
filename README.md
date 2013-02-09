practical_erlang_tutorial
=========================

A practical tutorial about how to program with Erlang.

The goal of this example is to show how to create an Erlang project that isn't
too far from an actual real-world application.

License
=======

3-clause BSD

Overview
========

Introducing... reverserl!

Let's build a little Erlang application that does something really simple: reverse strings.
To show a bit more of what Erlang can do it will also have a simple session based web API.

We'll try to cover:

- Some of OTP's principles (gen_server, application, ...)
- Basics of functional programming with Erlang
- The rebar build tool for Erlang
- Testing using eunit/QuickCheck mini (Proper should work too -
  https://github.com/manopapad/proper)

Architecture Overview
=====================

At its core reverserl has 

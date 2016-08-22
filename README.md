Rapid
=====

This is a helper library for rapid prototyping in GHCi sessions.  It
provides hot-reloadable background threads as well as values that can be
reused across module reloads (to save initialisation time).  Main use
cases include the development of long-running applications, especially
those with multiple separable units:  servers, web applications,
interactive user interfaces, etc.

A tutorial-style introduction is contained within the `Rapid` module.

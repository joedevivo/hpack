# hpack

HPACK Implementation for Erlang

[HPACK RFC-7541](http://tools.ietf.org/html/rfc7541)

This implementation was designed for use by
[Chatterbox](http://github.com/joedevivo/chatterbox), but could be
used by any HTTP/2 implementation (or, if something other than HTTP/2
has need for HPACK, this would work as well)

## Why Separate?

* Use by other projects
* A separate RFC seemed like a really clear abstraction

## What's Covered

### Compression Contexts

[RFC-7541 Section 2.2](http://tools.ietf.org/html/rfc7541#section-2.2)

### Dynamic Table Management

[RFC-7541 Section 4](http://tools.ietf.org/html/rfc7541#section-4)

### Primitive Type Representations

[RFC-7541 Section 5](http://tools.ietf.org/html/rfc7541#section-5)

### Binary Format

[RFC-7541 Section 6](http://tools.ietf.org/html/rfc7541#section-6)

* Indexed Header Field Representation
* Literal Header Field Representation
* Dynamic Table Size Update

## What's not covered

### HTTP/2 Frames

An HTTP/2 implementation should be responsible for anything you need
to do in order to read a compressed header block from various
PUSH_PROMISE, HEADERS, and/or CONTINUATION frames. Once you have a
complete block, you can use this library to turn it into something you
can use for fulfilling HTTP requests

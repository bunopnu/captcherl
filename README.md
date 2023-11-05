# CaptchErl

An Erlang library that simplifies the process of verifying CAPTCHA responses for your web applications by providing easy-to-use functions for interacting with **Cloudflare Turnstile**, **reCAPTCHA** and **hCaptcha**.

[![Test Status](https://github.com/bunopnu/captcherl/actions/workflows/test.yml/badge.svg)](https://github.com/bunopnu/captcherl/actions/workflows/test.yml)
[![Hex Version](https://img.shields.io/hexpm/v/captcherl.svg)](https://hex.pm/packages/captcherl)

## Installation

Package can be installed by adding `captcherl` to your list of dependencies:

```erlang
{deps, [{captcherl, "0.1.0"}]}.
```

## Compatibility

This library requires Erlang/OTP version 25 or later.

## Development

### Prerequisites

- [rebar3](https://www.rebar3.org): A widely used build tool for Erlang.
- [efmt](https://github.com/sile/efmt): Code formatter for Erlang.

### Quick Start

```shell
# Clone project
$ git clone https://github.com/bunopnu/captcherl.git
$ cd captcherl

# Build project
$ make build

# Run formatter
$ make format

# Run formatting check, dialyzer and xref
$ make check

# Run tests
$ make test

# Start an Erlang shell
$ make start
1> captcherl:verify(turnstile, {<<"1x0000000000000000000000000000000AA">>, <<"always true">>}).
true
```

## Documentation

Online documentation is available at [HexDocs](https://hexdocs.pm/captcherl).

Alternatively, you can generate documentation locally by running the following command:

```shell
$ make doc
```

## License

CaptchErl is licensed under the MIT license.

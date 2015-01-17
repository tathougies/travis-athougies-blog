---
title: HQuant-State
author: Travis Athougies
slug: hquant-state
summary: An in-memory time-series database written over AcidState, with on-disk or Amazon S3 archival support
language: Haskell
github: https://github.com/tathougies/hquant-state
priority: 1
status: STABLE
version: 1.0.0
---

HQuant-State is a time series database for HQuant, meant to store historical stock and option
quotes and historical balances and incomes. It is meant to be run as a separate service, and it is
expected that it will be written to by a DEQ client handling a quote stream (perhaps from
hdeq-tradeking or some other stock data provider).

HQuant-State is a hybrid in-memory/persistent storage database. Due to the nature of time series, it
is append only, meaning that it can make several optimizations that other databases can't. Most
notably, it only keeps a set amount of historical data in memory, and puts the rest into storage
somewhere else. Currently, this storage can be either the local disk or an Amazon S3 bucket. This
behavior is entirely configurable.

Configuration File
----------------

There is an example configuration file in the Git repository at `config.example`. More documentation coming!

Potential Uses
---------------

* Finance (DIME was originally made for my stock monitoring activities)
* Large sensor networks/Internet of Things
* Data warehousing of historical load data, link clicks, etc.

Questions, Bugs, and Comments
------------------------------

[Redirect](/contact.html) any questions, bugs, comments, and/or complaints to me. Please be nice, I don't do this for money :)

I hope you enjoy using this software.

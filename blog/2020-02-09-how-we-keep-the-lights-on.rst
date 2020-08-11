---
title: How we keep the lights on
author: Troels Henriksen
description: An exciting tour of the various free services we leech on, as well as the dubious server setups that keep the Futhark project running.
---

I have written much about how Futhark is designed and how the compiler
is implemented.  In this I will switch things up a bit and instead
write about how we have built our project infrastructure - from
automatic compiler testing to the web server hosting this blog post.

In the same way that `Futhark's small niche affects the language and
compiler design
<2018-06-18-designing-a-programming-language-for-the-desert.html>`_,
our infrastructure is similarly impacted.  We assume that whoever
maintains it is incompetent, easily distracted, and not very diligent.
I know that this is a fair characterisation, because *I* am the one
who maintains it.  Everything we built must therefore be highly
robust, require almost no ongoing maintenance, and be very cheap (the
money itself is not the biggest problem, but payment setups are always
a potential failure source).  Overall, while our setup is not perfect,
it seems to work pretty well, and some of it might serve as
inspiration to other small language projects.

Our guidelines
--------------

We have two main guidelines for our infrastructure::

1. Avoid stateful servers and daemons that may crash or corrupt
   their state, and prefer batch mode programs that frequently
   restart from scratch.

2. Prefer as *few* systems as possible.

Following guideline 1, we try to make heavy use of "serverless" cloud
services, in particular `Travis CI <https://travis-ci.org/>`_ (I know
I'm abusing the "serverless" term here).  The batch jobs produce
static files that are eventually just served by a simple web server.

Following guideline 2, *all* of those static files are served by the
``futhark-lang.org`` server, which is the exact same system that runs
`my personal website <https://sigkill.dk>`_, my IRC bouncer, my
outgoing email proxy, and a bunch of other things.  The hypothesis
here is that I am very likely to quickly find out if this system
fails.  I do not trust myself to set up reliable monitoring, but I do
trust myself to notice when I cannot get on IRC or send email.  The
system itself is a fairly cheap VPS hosted by `TransIP
<https://www.transip.eu/>`_, and it runs `OpenBSD
<https://www.openbsd.org/>`_ for reasons I have explained `elsewhere
<https://sigkill.dk/blog/2020-02-09-why-i-run-openbsd.html>`_.

I am well aware that putting everything on a single system is not
*best practice*, but best practices are meant for people who are
skilled at their job, and as mentioned earlier, I am not a skilled
systems administrator.  While I could no doubt eventually figure out
how to set up some containerised infrastructure on AWS, I would
probably have forgotten how it works by the time something breaks and
I have to fix it.


Continuous integration
----------------------

Futhark is hosted on GitHub, and we use Travis CI as our primary CI
mechanism.  On every commit and pull request, a Travis job checks
whether the compiler still compiles, and runs our test suite.  The
test suite is executed through a program called `futhark test
<https://futhark.readthedocs.io/en/stable/man/futhark-test.html>`_.
While originally intended only to test the compiler, we now also use
it to test Futhark programs and libraries on their own.

Travis jobs, unfortunately, are limited to 60 minutes at most, and
building the compiler from scratch - including its own dependencies -
and running the test suite can take longer than that, especially on
the small machines that Travis makes available for free.  Our solution
is to break the build into multiple `stages
<https://docs.travis-ci.com/user/build-stages/>`_, each of which has
its own 60 minute cap.  We use one stage to build dependencies, one to
build the compiler itself, and one to run the test suite, and use
Travis's support for build caches to preserve build artefacts across
stages.  All this was rather painful to set up, as I find Travis's
documentation rich on examples and poor on fundamentals, but it works
fairly well for now (`our .travis.yml
<https://github.com/diku-dk/futhark/blob/master/.travis.yml>`_).

When we release a new version of Futhark, it is also a Travis job that
builds the release binary tarball and uploads it to GitHub and `our
web server <https://futhark-lang.org/releases/>`_.  This is done
simply by pushing a Git tag of the format ``vX.Y.Z``.

To hedge our bets, we also run CI on `Azure Pipelines
<https://dev.azure.com/futhark/futhark/_build?definitionId=1>`_.  It
tests only a little more than the Travis job, but without all of the
"stage" complexity, as the Azure timeouts are long enough that
everything can be built from scratch on every commit.  While I find
the Azure Pipelines web UI to be significantly more clunky than
Travis's, the setup was otherwise quite straightforward.

Both Travis and Azure run on Linux.  Since we also want to support
Windows, but none of the Futhark developers actually run it, we also
have a somewhat bare-bones `AppVeyor setup
<https://ci.appveyor.com/project/Athas/futhark>`_.  We have had quite
a bit of trouble with this one being flaky, so perhaps we will migrate
Windows builds to Azure eventually.

Benchmarking
------------

All of the preceding 3(!) CI systems hare the same flaw: they run on
arbitrary cloud hardware with *no GPUs*.  This means they cannot test
the most interesting aspect of the compiler, namely how fast the
generated code runs!  Therefore we also maintain a `Buildbot
<https://buildbot.net/>`_ setup that runs on known hardware physically
located in the university data centre, with servers running `RHEL
<https://www.redhat.com/en/technologies/linux-platforms/enterprise-linux>`_
and primarily maintained (at least at the OS level) by the IT
department.

Buildbot is a CI tool that is written and configured in Python.  It
is less popular than `Jenkins <https://jenkins.io/>`_, a similar tool,
but I greatly prefer configuration through a python API to clicking
around in a user interface.  `Our configuration looks like this
<https://github.com/diku-dk/futhark-buildbot/blob/master/master/master.cfg>`_.

The architecture of Buildbot is straightforward.  A central *master*
server listens to new commits in the Git repository, and schedules
various jobs in response.  These jobs are handled by *workers*, which
in our case are GPU-equipped university machines that connect to the
master.  One nice aspect of this design is that only the master needs
to be publicly accessible on the Internet, while the workers can be
behind NATs or firewalls without any issue.  In our case, we run the
master on a server called `napoleon <http://napoleon.hiperfit.dk>`_,
that we inherited from the `HIPERFIT project <http://hiperfit.dk/>`_,
but that is mostly for historical reasons.  Since all the master does
is coordinate the workers and perform a few bookkeeping jobs, we
should probably migrate it to the same system that runs the web
server.

So, what do the jobs actually do?  They run our test- and benchmark
suite using all of our parallel backends (``opencl``, ``cuda``,
``pyopencl``, ``csopencl``), using the `futhark bench tool
<https://futhark.readthedocs.io/en/stable/man/futhark-bench.html>`_,
and upload the performance results to `a central directory
<https://futhark-lang.org/benchmark-results/>`_, with one file per
commit/backend/machine combination.  This means that we have
historical performance data on (almost) every commit.

What we *don't* currently do, unfortunately, is to *automatically*
compare new results to old ones, to identify performance regressions.
This is still done manually.  We do, of course, have *some* tooling
for performing the analysis.  The simplest is
`compare-compiler-versions.sh
<https://github.com/diku-dk/futhark/blob/master/tools/compare-compiler-versions.sh>`_,
a script that takes two compiler commit IDs and for all benchmarks
shows the difference in performance.  This is useful enough for
gauging the consequences of a single commit.

For more in-depth historical analysis and visualisation, we need a
more complex tool.  This is the `Futhark Benchmark Dashboard
<https://futhark-lang.org/benchmark-dashboard/index.html#/>`_, which
can graph performance changes for a benchmark over time (`example
<https://futhark-lang.org/benchmark-dashboard/index.html#/visualize?selected=%5B%5B%22230%2C25%2C75%22%2C%22opencl%22%2C%22RTX2080%22%2C%22futhark-benchmarks%2Fmisc%2Fheston%2Fheston32.fut%22%2C%22data%2F1062_quotes.in%22%5D%2C%5B%2260%2C180%2C75%22%2C%22opencl%22%2C%22RTX2080%22%2C%22futhark-benchmarks%2Fmisc%2Fheston%2Fheston32.fut%22%2C%22data%2F10000_quotes.in%22%5D%2C%5B%22255%2C225%2C25%22%2C%22opencl%22%2C%22RTX2080%22%2C%22futhark-benchmarks%2Fmisc%2Fheston%2Fheston32.fut%22%2C%22data%2F100000_quotes.in%22%5D%5D&graphType=slowdown&slowdownMax=2&xLeft=0&xRight=100>`_).
Such dashboards are common for compiler projects (`here's Chrome's
<https://chromeperf.appspot.com/report>`_).  However, ours is
carefully designed to require no custom server-side code at all.  The
dashboard is a pure client-side JavaScript application served as
simple static files, reading processed data files also served over
HTTP.

The raw data gathered by benchmarking is organised by commit, while
the dashboard needs the data to be organised by benchmark and dataset
name.  This pre-processing is done by a Buildbot job that runs after
every benchmark job and, as always, `just dumped in a web server
directory <https://futhark-lang.org/benchmark-results-processed/>`_.
The dashboard code itself does not need to be updated.  The advantage
of this is also that we have no dependency on a database, along with
the maintenance complexity that would bring.  The downside is that the
information is not truly structured, and there is no way to query it
using e.g. SQL.

I should note that as I am not brave enough for JavaScript myself,
`the dashboard
<https://github.com/diku-dk/futhark-benchmark-dashboard>`_ was
developed as a bachelor's project by a group of then-undergraduates at
`DIKU <http://diku.dk>`_ (Bo Thomsen, Frederik Lassen, and Emil
Weihe).  Accustomed as they were to mainstream web programming, it
took them a little while to understand why I wanted such an odd
architecture, but they did eventually deliver something that has been
running well, and completely maintenance-free, for years.

Package repository
------------------

Futhark comes with a bare-bones package manager that I have `written
about before
<https://futhark-lang.org/blog/2018-08-03-the-present-futhark-package-manager.html>`_.
As with everything else in Futhark, it is built around free cloud
services and static file hosting.  Specifically, a package is
identified by a repository on GitHub or GitLab, with a tag for each
released version.  Eventually, I wish to extend this so that a package
can be identified by any URL pointing at an appropriately formatted
HTML file containing metadata, but it has not proven necessary just
yet.

Apart from a tool for downloading and upgrading packages, there must
also be a central repository of packages to provide discover-ability.
For Futhark, this is handled by `futhark-docbot
<https://github.com/diku-dk/futhark-docbot>`_, which contains `a file
<https://github.com/diku-dk/futhark-docbot/blob/master/pkgpaths.txt>`_
listing the location of every known Futhark package.  Every day, an
automatically scheduled Travis job then looks at all available
versions of each package, and generates a set of static HTML files
that list all packages, along with documentation generated by
`futhark-doc
<https://futhark.readthedocs.io/en/stable/man/futhark-doc.html>`_, and
uploads the result to `a central location
<https://futhark-lang.org/pkgs/>`_.

Automation via regularly scheduled Travis jobs is a pretty lightweight
way for free software projects to perform such routine tasks.  It does
mean that up to a day may pass before new package versions show up in
the registry (although they can be installed immediately), but this is
a small trade-off in return for not having to write and maintain any
server side code myself.

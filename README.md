# ItalianVerbs

Rules-based conjugation of Italian verbs.

# Goal

The goal is to derive all the inflected forms of a given verb from a limited
set of rules, with a manageable number of exceptions.

The code should be as readable as possible, with the hope that the rules
can be fully grasped, and even memorized, with sufficient study by a human
language learner.

# Code

The core is written in Standard ML. The test harness makes use of the SML/NJ
Library.

# Test harness

There is a test harness, which uses the [JSON database](https://github.com/ian-hamlin/verb-data)
published by @ian-hamlin to generate test cases.

I also plan to generate test cases using the [GLAFF-IT](http://redac.univ-tlse2.fr/lexiques/glaffit.html)
database.

# Limitations

This project is still in its earliest stage, and currently only regular verbs
are supported.

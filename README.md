# Datalog Analysis Tool (DAT)

## Overview
DAT is a framework for doing program analysis in Datalog. It relies on [BAP](https://github.com/BinaryAnalysisPlatform/bap) to generate Datalog rules and thus supports all architectures supported by BAP. For a more formal explanation of the framework, see dat_paper.pdf.

## Installation and Usage

To install the plugin, make sure BAP is downloaded `opam list bap`, then, clone this repository and run `./compile-plugin.sh` to create the BAP plugin.

Once installed run `./dat path/to/file` to create your Datalog facts. Now you can create an analysis, see the `/tests` directory for examples written in [Souffle Datalog](https://souffle-lang.github.io/).

You can also append DAT to your path by running `PATH=$PATH:~/path/to/dat.sh`.

## Facts

`callgraph.facts` enumerates every function call.\
`defs.facts` enumerates every definition (uses ssa).\
`uses.facts` enumerates every use (uses ssa).

## Contributing

If you want to extend the tool, start with `gen_info.ml` to understand the source code, then fork and make a PR.

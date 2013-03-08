% NetLogo clone in Haskell
% Nikolaos Bezirgiannis, Ilias Sakellariou
% 15/12/2012

# What is NetLogo

[NetLogo](http://ccl.northwestern.edu/netlogo/) is the de-facto simulation framework for modelling
multi-agent environments. The software is open-source (GPL) and is actively developed by a research group in the Northwestern University of Chicago, US.

The NetLogo user specifies the multiple-turtle-graphics environment in a variant of the Logo language. The written code
is translated to Scala and then compiled down to Java bytecode, which is consequently executed on the JVM. Besides 
a source-to-source compiler (from NetLogo to Scala), the developers provide an IDE to accommodate the writing of NetLogo code and
a GUI to watch the live execution of the compiled model.

# Idea

Write a NetLogo-clone in Haskell.

# Project Goals

- Be faster. Utilizing Software Transactional Memory (STM), we can accelerate the execution of the multi-agent environments.
Results from early prototyping, conducted by me and my supervisor in Greece,
has shown that we are *50%* faster than NetLogo (on single core) and a *close to linear speedup* when executed on multicore.
- Write an Embedded Domain-specific Language (EDSL) which will be close to the NetLogo syntax, and distribute it as a Haskell library.
- (optional) Write a source-to-source compiler (from a Logo-variant to Haskell) to make it easier to write these programs.
In this way, we may even achieve compatibility with NetLogo-written source code. (alternative) Write Template Haskell code
to express these basic NetLogo constructs.
- (optional) Visit the possible execution of these simulations in a distributed system with Cloud Haskell.
- Benchmark the clone and compare&contrast it against NetLogo.
- Write an IDE and a GUI similar to NetLogo's.

# Proposed Advantages 

- Faster.
- The advantages of having an *Embedded* DSL.
- Lifting limitations of the NetLogo framework (not going into detail right now).
- The NetLogo language lacks a module system. This is implicitly alleviated using Haskell's module system.

# Proposed Disadvantages

- Because of non-deterministic concurrency employed, the simulation runs are not reproducible.
- NetLogo has dynamic typing, whereas Haskell is statically typed. These may make particular programs difficult to
translate from NetLogo to the Haskell clone.

# Timetable

- *2-3 months* Writing the code.
- *1 month* Testing, profiling, optimizing the code. Finally, benchmarking it against NetLogo.
- *1 month* writing the thesis report.

# Current state of affairs

- Finished prototyping.
- Started writing the EDSL.
- Started writing the thesis proposal.

# Questions to Johan Jeuring

- Do you think this idea qualifies as a Master thesis project?
- If so, who do you think, including yourself, is an appropriate professor 
to supervise me? I know that a professor with good Haskell experience would be appropriate,
but, I am also considering that a professor from the AI group, would be most
interested in a NetLogo clone.

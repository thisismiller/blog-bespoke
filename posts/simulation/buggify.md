<!---
title: BUGGIFY
summary: Simulation has built-in support for injecting low-level failures, and thus can productively discover failures of low-level components.  As lower-level abstractions are assembled into a higher-level system, simulation will lose effectiveness in finding high-level failures by injecting low-level bugs.  `BUGGIFY` bridges the gap to allow random injection of abstract, system component failures.
---!>

# BUGGIFY

FoundationDB's correctness is validated with a deterministic simulation framework that productively fuzzes faults against a specification of the system's behavior.

When I've seen discussions of FDB's testing in the wild, the focus has always been on the first part: the deterministic simulation framework.  This is likely due to Will Wilson's fantastic talk on the subject, ["Testing Distributed Systems with Deterministic Simulation"][strangeloop-waw], focusing mainly on that first part.  An important beginning, but not the whole story.

A deterministic simulation framework with random fault injection provides a testing framework that _can_ find bugs.  However, the question is how quickly?  If validating the correctness of a network protocol or storage engine, then network or disk fault injection alone would be sufficient to give a high degree of confidence in correctness.  The types of dangerous conditions that the code must correctly handle, such as network instability or disk corruption, exactly match what the simulator directly produces.

When building full, higher level, distributed systems, there's no longer an exact match between what the simulator can easily produce and the dangerous situations that discover bugs.  What's the chance of random packet loss causes a minimal quorum being used in two consecutive Raft leader elections?  <!-- TODO: MORE EXAMPLES -->  It's still _possible_ for these bugs to be discovered, however, it'd take a tremendous number of simulation runs to stumble upon one of these higher-level dangerous situations.  This poor ratio of testing time to coverage, We've lost the "productively" in our original description of simulation testing.  What is needed is a way to enable the simulator to directly produce failures in higher level components or APIs.

How FoundationDB does this is with the `BUGGIFY` macro.  `BUGGIFY` exists to bias the simulator towards doing dangerous, bug-finding things.  It is the main tool that differentiates FDB's simulation testing from other black box solutions.  Instead of writing FoundationDB and then trying to validated it against a separate blackbox testing solution afterwards, FoundationDB was written to explicitly cooperate with the simulator by instrumenting its code with descriptions of how to cause failures in each component of the system.

`BUGGIFY` has the following rules:

1. `BUGGIFY` only ever evaluates to true when run in simulation.
2. The first time each `BUGGIFY` use is evaluated, it is either enabled or disabled for the entire simulation run.
3. Enabled uses of `BUGGIFY` have a 5% chance of evaluating to true (or custom, e.g. `BUGGIFY_WITH_PROB(0.001)` == 0.1% chance).

`BUGGIFY` only ever being true in 

[strangeloop-waw]: https://youtu.be/4fFDFbi3toc





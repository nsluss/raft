# consensus - a raft implementation

From etcd:
 - Reliable: properly distributed using [Raft][raft]

Additions:
 - API: gRPC, REST?
 - Security

### Consensus Algorithms

You can most easily think of a consensus algorithms arising from the need to
replicate state machines. From this perspective, a state machine would live on a
server, and identical copies of the same state machine would be copied to all other
servers. In this way, even if a server goes down, the state machine can be accessed.
These state machines are typically represented in logs

Logs are the history of actions which the state machine uses to execute commands in
the given order. The logs must be in sync, which turns into the job of theconsensus
algorithm.

Once consensus has been reached on the consistency of the logs/commands and they are
properly replicated, they are executed in order and the output is returned to
clients. In this way, all servers in the cluster give the appearance of a single,
highly reliable state machine.

Consensus algorithms ensure consistency under all non-Byzantine conditions.  This
includes network delays, partitions,and packet loss, duplication, and reordering.

> Byzantine faults, for the uninitiated, are considered the most generaland most
> difficult class of failures to handle and include any fault which presents
> differentsymptoms to different observers. Malicious attacks and software errors can
> cause nodes to exhibit Byzantine behavior.

In order to maintain consensus, there is a prerequisite that a majority of the
servers re available, functional, and can communicate with clients. So a cluster of
threecan tolerate one failure at a time, a cluster of five can tolerate a failure of
two,and so on. Of course these failed nodes can rejoin the cluster at a later point.

Clock synchronization is not something which can be assumed in a distributed system.
Paired with extreme packet lag, availability problems can occur. Because of this, a
consesus algorithm can't be dependent on time.

Often, algorithms are written so that nodes to not need to wait for a full consensus
to agree before commands are run. More often, as soon as a majority of the cluster
has confirmed a round of remote procedure calls, the command can execute and slow
servers do not need to impact the overall system's performance.

[raft]: https://raft.github.io/
[byzantine]:https://en.wikipedia.org/wiki/Byzantine_fault_tolerance
[byzantine-generals-problem]: https://www.andrew.cmu.edu/course/15-749/READINGS/required/resilience/lamport82.pdf
[time-clocks-and-the-ordering]:http://research.microsoft.com/en-us/um/people/lamport/pubs/pubs.html#time-clocks

## The Raft Algorithm

The design of Raft came from decomposing and simplifying the problem of consensus.
Raft is split into sub-problems which can be explained relatively independently. In
the same vein we will build it out this library in an explaination-first
implementation. These sub-problems include leader election, log replication, safety,
and membership changes.

Raft also simplifies the number of states to consider each state machine. This is to
limit the degrees of freedom of states, improve understandablity, and to promote log
consistency. This also helps remove non-determinism which is often times difficult to
understand, however, in some situations non-determinism is actually very beneficial
to this cause. Randomized approaches introduce nondeterminism, however can also take
a complex set of next-actions and reduce it to a single simplification. Leader
election is an example of this, as it doesn't matter which node should be chosen
next, so we can select one from our cluster at random.

###

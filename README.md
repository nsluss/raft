# kvraft - a distributed key-value store with raft

Roadmap:
 - Outlined
 - Spec'd
 - [Raft][raft] implementation
 - gRPC
 - Byantine-Fault Tolerance
 - Security

### Development

For development, we use `stack` and `hspec` under make. Check out our Makefile for
up-to-date commands. `make` will delegate to `stack install`, `make test` will run
`stack test`, and `make watch-tests` will run `stack test --file-watch`.

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

### Overview

From a high-level: raft first elects a leader, then gives that leader complete
responsibility for managing the replicated log. The leader accepts logs from clients,
replicates them on other servers, and tells servers when it is safe to apply log
entries to state machines. If a leader hits a fault or becomes disconnected from the
cluster, a new leader is elected.

At any given time, a server is in one of three states:
 + Leader: as detailed above
 + Follower: a follower only responds to requests from leaders and candidates. it
   issues no requests and simply relays requests from clients to the leader.
 + Candidate: this state is only used to elect a new leader.

![A state machine representation of raft][raft-state-machine]

Normally, there is exactly one leader and no candidates.

Time is divided into _terms_ of arbitrary length and are numbered with monotomicly
incrementing numbers. Each term begins with an election, where any candidates may
attempt to become the leader. If a candidate wins the election, then it serves as
leader for the rest of the term. In the case of a split-vote election, the term will
end with no leader, and a new term will start shortly thereafter.

Servers may observe transitions between terms at different times, a server may not
observe an election, and a server may not observe a full term. Each server stores the
number of its current term, which is exchanged whenever servers communicate:
 + if one server's current term is smaller than the others, then the term is updated
   to the large of the two
 + if a candidate or leader discovers that its term is out-of-date, it immediately
   reverts to a follower state.
 + if a server recieves a request with a stale term number, it rejects the request.

Raft servers communicate using remote procedure calls (RPC). The basic consensus
algorithm requires only two types of RPCs: RequestVote (initiated by candidates
during election), and AppendEntries (initiated by leaders to replicate log entries,
and as a form of heartbeat). A third, optional, RPC is included as a means of
transferring snapshots between servers. Servers retry RPCs if they do not recieve a
response in a timely manner, they also issue RPCs in parallel for the best
performance.

In the following sections we will explain the Raft implementation in more detail,
decomposed into sub-problems outlined in the original paper. These sections are:
 + [Leader Election][#leader-election]: "A new leader must be closen when an existing
   leader fails"
 + [Log Replication][#log-replication]: "The leader must accept log entries from
   clients and replicate them across the cluster, forcing other logs to agree with
   its own."
 + [Safety][#safety]: "If any server has applied a particular log entry to its state
   machine, then no other server may apply a different command for the same log
   index."

[raft-state-machine]:https://adriancolyer.files.wordpress.com/2015/03/raft.jpg

### Leader Election

Each node, upon entering a cluster, begins as a follower. This will continue so long
as it recieves valid RPCs from a leader or candidate. Leaders must send periodic
heartbeats (in the form of AppendEntries RPCs with no log entries) in order to
maintain their state. If a follower fails to recieve communication over a period of
time called the election timeout, then it assumes that there is no leader and begins
an election to choost a new leader.

In order to initiate an election, a follower:
+ increments its current term
+ transitions to the candidate state
+ cast a vote for itself
+ issues RequestVote RPCs in parallel to all other servers in the cluster

Now, as a candidate, it remains in this state until one of the following:
+ it wins the election
+ another server establishes itself as leader
+ a period of time goes by with no winner

#### A candidate wins the election

A candidate wins an election if it receives votes from a majority of the servers in
the full cluster for the same term.

Each follower will vote for at most one candidate in a given term, on a
first-come-first-served basis. Because of a majority is required, it is garunteed
that there will be at most one candidate which will win an election. Once a candidate
has won an election, it becomes the new leader and sends heartbeat messages to all of
the other servers to establish authority and prevent new elections.

### Another server establishes itself as leader

It's possible that a candidate may recieve a heartbeat (AppendEntries RPC) from
another server which claims to be a leader. Only if the term is at least as large as
the candidates's current term will the candidate recognize the leader as legitimate
and return to the follower state, otherwise the candidate ignores (rejects?) this
request.

### A period of time goes by with no winner

It may also be that the candidate neither wins nor loses the election. In this
scenario, many followers become candidates at the same time and the vote could be
split so that no candidate obtains a majority. In this situation, each candidate's
election phase will time out and each will increment its term and initiate a new
election phase.

One each node, a randomized election timeouts from a fixed interval (typically
somewhere in the range of 150-300ms) is taken. This prevents the situation of
split-votes looping indefinitely: ideally, candidates will initiate elections with a
uniformly random probability and tuplicate timeouts will be infrequent.

### Log Replication

Once a leader has been elected, it begins servicing client requests. Each client
request contains a command to be executed by the replicated state machines. The
leader appends the command to its log as a new entry, then issues AppendEntries RPCs
in parallel to each of the other servers to replicate the entry. When the entry has
been safely replicated (as described below), the leader applies the entry to its
state machine and returns the result of that execution to the client. If followers
crash or run slowly, or if network packets are lost, the leader retries AppendEntries
RPCs indefinitely (even after it has responded to the client) until all followers
eventually store all log entries.

Logs are organized as shown in Figure 6. Each log entry stores a state machine
command along with the term number when the entry was received by the leader. The
term numbers in log entries are used to detect inconsistencies between logs and to
ensure some of the properties in Figure 3. Each log entry also has an integer index
iden

### Safety



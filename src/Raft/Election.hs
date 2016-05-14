-------------------------------------------------------------------------------
-- |
-- = Leader Election
-- Raft uses randomized timers to elect leaders. This adds only a small amount
-- of mechanism to the heartbeats already required for any consensus algorithm,
-- while resolving conflicts simply and rapidly.
--
-- === Membership changes
-- Raft's mechanism for changing the set of servers in the cluster uses a joint
-- consensus approach where the majorities of two different configurations
-- overlap during transitions. This allows the cluster to continue operating
-- normally during configuration changes.
-------------------------------------------------------------------------------



module Election where


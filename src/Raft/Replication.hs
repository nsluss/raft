-------------------------------------------------------------------------------
-- |
-- = Log Replication
-- === Leadership and logging
-- Raft uses a stronger form of leadership than other consensus algorithms. One
-- of the ways this manifests itself is that log entries only flow from the
-- Leader to other Followers and Candidates. This simplifies the management of
-- the replicated log and makes Raft easier to understand.
-------------------------------------------------------------------------------

module Replication where


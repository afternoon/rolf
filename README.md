Rolf
====

- System monitoring/graphing tool like Munin.
- Written in Erlang.
- Asynchronous data gathering.
- Sample frequency up to 1 second, configured per plug-in.
- HTTP interface for HTML, graphs and data via JSON.
  - Dynamic graphs, no static HTML.
- Compatible with Munin plug-in protocol.
- Runs anywhere Erlang runs (especially Windows).
- Nodes being monitored are an Erlang cluster.

Architecture
------------

Nodes currently just provide data for a master process which collects, stores
and serves data via HTTP to clients. Nodes could collect and store their own
data, transferring it to the master for graphing etc. Initially the architecture
is intentionally quite similar to Munin's.

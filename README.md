Rolf
====

- System monitoring and graphing tool like Munin or collectd.
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

Author
------

Ben Godfrey, ben@ben2.com, http://aftnn.org/

License
-------

Rolf - a system monitoring and graphing tool like Munin or collectd.
Copyright (C) 2011 Ben Godfrey.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

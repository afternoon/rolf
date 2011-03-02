Rolf
====

- System monitoring and graphing tool like Munin or collectd.
- Written in Erlang.
- Asynchronous data gathering.
- Sample frequency down to 1 second, configured per plug-in.
- HTTP interface for HTML, graphs and data via JSON.
  - Dynamic graphs, no static HTML.
- Writing plug-ins is simple. Not quite as simple as Munin, because plug-ins are
  kept resident between updates, as in collectd.
- Runs anywhere Erlang runs (especially Windows).
- Nodes being monitored are an Erlang cluster.

Architecture
------------

- A server is created on each machine.
- Each server has many services, which are started when the server starts.
- One or more recorders subscribe to each server.
- When a service generates an update, it sends it to all recorders.

Author
------

Ben Godfrey, ben@ben2.com, http://aftnn.org/.

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

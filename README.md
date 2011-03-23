Rolf
====

- Monitoring and graphing tool like Munin or collectd.
- Written in Erlang.
- Asynchronous data gathering.
- Sample frequency down to 1 second, configured per plug-in.
- HTTP interface for HTML, graphs and data via JSON.
- Writing plug-ins is simple. Plug-ins are kept resident between updates, as in collectd.
- Runs anywhere Erlang runs (at least Linux, OS X, Windows).

Getting started
---------------

Start the Erlang VM on a set of machines using the same cookie.

    [user@john ~] erl -sname rolf -setcookie rolf123
    [user@paul ~] erl -sname rolf -setcookie rolf123
    [user@george ~] erl -sname rolf -setcookie rolf123
    [user@ringo ~] erl -sname rolf -setcookie rolf123

Configure which nodes and services you want to run in `priv/recorder.config`.

    {nodes, [{rolf@john, all},
             {rolf@paul, [disk]},
             {rolf@george, [disk]},
             {rolf@ringo, [disk]}]}.

Start the application from the node where you would like to store your data.

    (rolf@john)1> application:start(rolf).

Architecture
------------

- A cluster is created (probably one node per machine) and a recorder started.
- The recorder starts a node server on each node.
- Each node has many services which generate data.
- When a node starts, the recorder announces to each node what services it is
  interested in recording info about. The node starts those services.
- When a service generates an update, it sends it to the recorder.

Example supervision tree:

    Recorder
    ├── Node1
    │   ├── Service1
    │   │   └── External plugin
    │   └── Service2
    └── ERRD Server
        └── rrdtool

Author
------

Ben Godfrey, ben@ben2.com, http://aftnn.org/.

License
-------

Rolf - a monitoring and graphing tool like Munin or collectd.

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

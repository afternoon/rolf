Rolf
====

'Azamat is run Big Data analytics on famous last words in devops. Most common is
"I know, I roll own monitoring tool!"'
-- @DEVOPS_BORAT

Overview
--------

- Monitoring and graphing tool like Munin or collectd.
- Written in Erlang.
- Asynchronous data gathering.
- Sample frequency down to 1 second, configured per plug-in.
- HTTP interface for HTML, graphs and data via JSON.
- Writing plug-ins is simple. Plug-ins are kept resident between updates, as in
  collectd.
- Runs anywhere Erlang runs (at least Linux, OS X, Windows).

Getting started
---------------

Start the Erlang VM on a set of machines using the same cookie.

    [user@john ~] erl -sname rolf -setcookie rolf123
    [user@paul ~] erl -sname rolf -setcookie rolf123
    [user@george ~] erl -sname rolf -setcookie rolf123
    [user@ringo ~] erl -sname rolf -setcookie rolf123

Configure which services should run on which nodes in `priv/recorder.config`.

    {nodes, [{rolf@john, all},
             {rolf@paul, [disk]},
             {rolf@george, [disk]},
             {rolf@ringo, [disk]}]}.

Start the application from the node where you would like to store your data.

    (rolf@john)1> application:start(rolf).

Architecture
------------

- A cluster is created (probably one node per machine) and a recorder started.
- The recorder starts services across the cluster.
- Services can be implemented as Erlang functions, external commands or external
  daemons.
- Services collect samples and send them to the recorder.

The supervision tree of an inflight Rolf instance looks like this:

    rolf_sup
    ├── rolf_recorder
    │   └── errd_server
    ├── rolf_service_sup (node1)
    │   ├── rolf_service (svc1)
    │   └── rolf_service (svc2)
    │       └── rolf_external
    └── rolf_service_sup (node2)
        └── rolf_service (svc1)

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

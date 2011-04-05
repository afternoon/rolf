Rolf
====

'Azamat is run Big Data analytics on famous last words in devops. Most common is
"I know, I roll own monitoring tool!"'
-- [@DEVOPS_BORAT](http://twitter.com/#!/DEVOPS_BORAT/status/51324117521141760)

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

Configure which node should be master and which services should run on which
nodes in `priv/recorder.config`.

    {recorders, [rolf@john]}.

    {services, [{rolf@john, all},
                {rolf@paul, [disk]},
                {rolf@george, [disk]},
                {rolf@ringo, [disk]}]}.

Start Rolf on each machine.

    [user@john ~] rolf start
    [user@paul ~] rolf start
    [user@george ~] rolf start
    [user@ringo ~] rolf start

Architecture
------------

- An Erlang cluster is created. Each node runs the rolf application.
- One node is designated the recorder by it's config file.
- If the cluster has no recorder, nothing happens.
- If a recorder has been started, service configuration is distributed to all
  other nodes by the recorder.
- A node manager process starts services and they start emitting samples.
- Services can be implemented as Erlang functions, external commands or external
  daemons.
- Services collect samples and send them to the recorder.

The supervision tree of an Rolf node looks like this (node has recorder):

    rolf_sup
    ├── rolf_recorder
    │   └── errd_server
    └── rolf_service_sup
        ├── rolf_service (svc1)
        └── rolf_service (svc2)
            └── rolf_external

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

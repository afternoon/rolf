Rolf
====

'Azamat is run Big Data analytics on famous last words in devops. Most common is
"I know, I roll own monitoring tool!"'
-- [@DEVOPS_BORAT](http://twitter.com/#!/DEVOPS_BORAT/status/51324117521141760)

Overview
--------

- Monitoring and graphing tool like Munin or collectd.
- Written in Erlang.
- Sample frequency down to 1 second, configured per plug-in.
- Record data on multiple nodes in parallel.
- Asynchronous data gathering.
- HTTP interface for HTML, graphs and data via JSON.
- Writing plug-ins is simple. Plug-ins are kept resident between updates, as in
  collectd.
- Runs anywhere Erlang runs (at least Linux, OS X, Windows).

Getting started
---------------

Configure which node(s) should be recorders in etc/app.config.

    [{rolf, [recorders, [rolf@john]]}].

Configure which services should run on which nodes in `etc/services.config`.

    {node, rolf@john, [disk, loadtime]}.
    {node, rolf@paul, [disk]}.
    {node, rolf@george, [disk]}.
    {node, rolf@ringo, [disk]}.

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
- Services collect samples and send them to all live recorders.
- Recorders and collectors can be added and removed from the cluster
  dynamically.

The supervision tree of an Rolf node looks like this (node has recorder):

    rolf_sup
    ├── rolf_recorder
    │   └── errd_server
    └── rolf_collector_sup
        ├── rolf_service (svc1)
        └── rolf_service (svc2)
            └── rolf_external

Plugins
-------

Plugins add additional collectors to Rolf.

- Plugins live in `plugins`.
- A plugin has a config file, see
  `plugins/loadtime/loadtime.config` for an example.
- Plugin config can be overridden per-site by customising `services.config`.
- Plugins can use an Erlang module, a command or a daemon to collect data.
- Plugin collectors written in Erlang should implement the `rolf_collector`
  behaviour.
- The `collect/1` function should capture data. The argument, `Service`, is a
  `service` record, which includes lots of useful info such as name and options
  (see `apps/rolf/include/rolf.hrl` for more info).

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

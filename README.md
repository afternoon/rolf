Rolf
====

- Monitoring and graphing tool like Munin or collectd.
- Written in Erlang.
- Asynchronous data gathering.
- Sample frequency down to 1 second, configured per plug-in.
- HTTP interface for HTML, graphs and data via JSON.
  - Dynamic graphs, no static HTML.
- Writing plug-ins is simple. Not quite as simple as Munin, because plug-ins are
  kept resident between updates, as in collectd.
- Runs anywhere Erlang runs (at least Linux, OS X, Windows).
- Nodes being monitored are an Erlang cluster.

Getting started
---------------

Starting Rolf requires a bit of Erlang knowledge at the moment - sorry.

- Start the Erlang VM on a set of machines using the same cookie.

    [user@john ~] erl -sname rolf -setcookie rolf123
    [user@paul ~] erl -sname rolf -setcookie rolf123
    [user@george ~] erl -sname rolf -setcookie rolf123
    [user@ringo ~] erl -sname rolf -setcookie rolf123

- Join the nodes into a cluster. Skip this step if you want to monitor one node.

    (rolf@john)1> net_adm:ping(rolf@paul).
    pong
    (rolf@john)2> net_adm:ping(rolf@george).
    pong
    (rolf@john)3> net_adm:ping(rolf@ringo).
    pong

- Start the application from the node where you would like to store your data.

    (rolf@john)4> application:start(rolf).

Architecture
------------

    + Recorder
    |\
    | + Node1
    | |\
    |  \+ Service1
    |   + Service2
     \
      + Node2
      |\
       \+ Service1
        + Service4

- A cluster is created (probably one node per machine) and a recorder started.
- The recorder starts a node server on each node.
- Each node has many services, which are started when the node server starts.
- When a service generates an update, it sends it to the recorder.

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

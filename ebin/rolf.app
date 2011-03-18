{application,rolf,
             [{description,"System monitoring and graphing tool like Munin or collectd"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{rolf_app,[]}},
              {env,[]},
              {modules,[rolf_app,rolf_node,rolf_recorder,rolf_rrd,
                        rolf_service,rolf_sup]}]}.

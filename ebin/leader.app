{application,leader,
             [{description,"An OTP application"},
              {vsn,"0.1.0"},
              {registered,[]},
              {mod,{leader_app,[]}},
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[coordinator_server,leader_app,leader_server,
                        leader_sup]},
              {licenses,["Apache 2.0"]},
              {links,[]}]}.
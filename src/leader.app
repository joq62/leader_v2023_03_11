{application, leader,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { leader_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, [leader,leader_app,leader_sup]},

  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.
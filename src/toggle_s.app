{application, toggle_s,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { toggle_s_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, [toggle_s,toggle_s_app,toggle_s_sup,toggle]},

  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.

{application, toggle,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { toggle_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, [toggle,toggle_app,toggle_sup]},

  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.

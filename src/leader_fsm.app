{application, leader_fsm,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { leader_fsm_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, [leader_fsm,leader_fsm_app,leader_fsm_sup]},

  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.

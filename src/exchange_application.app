{application, exchange_application,
 [{description, "exchange_application"},
  {vsn, "0.01"},
  {modules, [
    exchange_application,
    exchange_sup
  ]},
  {registered, []},
  {mod, {exchange_application, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.

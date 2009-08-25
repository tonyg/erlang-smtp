{application,smtp_server_test,
 [{description,"SMTP Server Test"},
  {vsn, "0.0"},
  {modules,[smtp_server_test]},
  {registered, []},
  {applications,[kernel,stdlib]},
  {mod, {smtp_server_test, []}},
  {env, [{listen_host, "0.0.0.0"},
         {listen_port, 8025}]}
 ]}.

{application,pop3_server_test,
 [{description,"POP3 Server Test"},
  {vsn, "0.0"},
  {modules,[pop3_server_test]},
  {registered, []},
  {applications,[kernel,stdlib]},
  {mod, {pop3_server_test, []}},
  {env, [{listen_host, "0.0.0.0"},
         {listen_port, 8110}]}
 ]}.

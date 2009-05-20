{application,pop3_server,
 [{description,"POP3 Server"},
  {vsn, "0.0"},
  {modules,[
	    generic_tcp_server,
	    pop3_server,
	    pop3_server_session
	   ]},
  {applications,[kernel,stdlib]},
  {mod, {pop3_server, []}},
  {env, [{listen_host, "0.0.0.0"},
	 {listen_port, 8110}]}
 ]}.

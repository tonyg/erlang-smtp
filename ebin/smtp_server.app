{application,smtp_server,
 [{description,"SMTP Server"},
  {vsn, "0.0"},
  {modules,[
	    generic_tcp_server,
	    smtp_server,
	    smtp_server_session
	   ]},
  {applications,[kernel,stdlib]},
  {mod, {smtp_server, []}},
  {env, [{listen_host, "0.0.0.0"},
	 {listen_port, 8025}]}
 ]}.

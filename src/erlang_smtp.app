{application,erlang_smtp,
 [{description,"Erlang POP3 and SMTP server library code"},
  {vsn, "0.0"},
  {modules,[
            generic_tcp_server,
            pop3_server_session,
            rfc2822,
            smtp_server_session,
            smtp_util
           ]},
  {registered,[]},
  {applications,[kernel,stdlib]},
  {env,[]}
 ]}.

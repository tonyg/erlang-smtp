# erlang-smtp, an SMTP and POP3 server library for Erlang

Back in 2007, I built these simple Erlang modules for generic SMTP and
POP3 services. The idea is that the programmer should instantiate a
service, providing callbacks for user authentication and for
service-specific operations like handling deliveries and scanning and
locking mailboxes. Originally, I was planning on providing
SMTP-to-AMQP and AMQP-to-POP3 gateways as part of RabbitMQ, but I
haven't had the time to seriously pursue this yet (though see
[rabbitmq-smtp](http://hg.rabbitmq.com/rabbitmq-smtp) for an
experiment in this direction).

The current status of the code is:

 - SMTP deliveries from Thunderbird and gmail work

 - POP3 retrieval from Thunderbird works, but isn't very solid,
   because I haven't implemented the stateful part of mailboxes yet.

 - The SMTP implementation is somewhat loosely based on RFC 2821. It's
   what you might generously call minimally conformant (improving this
   situation is tedious but not difficult). It doesn't address RFC
   2822 in any serious way (yet)

 - The POP3 implementation is based on RFC 1939.

 - SMTP AUTH is not yet implemented (but is not difficult)

 - I can't recall the details, but I think I might have skimped on
   something relating to POP3 UIDL.

Patches, bugfixes, contributions, comments and feedback are all very
welcome!

For a different (and much richer!) approach to the same kinds of
problems that this code addresses, see [Vagabond's impressive
`gen_smtp`](http://github.com/Vagabond/gen_smtp).

## Licensing

Copyright (c) 2007, 2010 Tony Garnock-Jones <tonygarnockjones@gmail.com>
Copyright (c) 2007 LShift Ltd. <query@lshift.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

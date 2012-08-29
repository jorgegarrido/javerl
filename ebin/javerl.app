{application, javerl,
 [
  {description, "Java <-> Erlang"},
  {vsn, "1.0"},
  {registered, []},
  {modules,[javerl_app, javerl_sup, socket]},
  {applications, [kernel,stdlib]},
  {mod, { javerl_app, []}},
  {env, [{port, 2575}, {reply, "erlang rocks!!"}]}]}.

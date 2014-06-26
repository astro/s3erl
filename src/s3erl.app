{application, s3erl,
 [
  {description, "s3erl app"},
  {vsn, "0.2.0"},
  {registered, []},
  {modules, [s3, s3app, s3pool, s3pool_sup, s3util]},
  {applications, [kernel,
                  stdlib,
                  inets,
                  ibrowse
                 ]},
  {mod, {s3app, []}},
  {env, []}
 ]}.

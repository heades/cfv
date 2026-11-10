# cfv

Declare configuration variables in a '.cfv' file and parse them.

An example of the contents of a CFV file is:

```haskell
-- a comment
greeting = 'hello';                   -- single-quoted string
empty    = "";                        -- double-quoted string
now      = `date +%s`;                -- run shell command
home'    = `echo "$HOME"`;            -- access to your shells PATH
secret api_key = "password/token/key" -- a secret variable (public is default)
```

For more information on the syntax of variable declarations see "Configuration.Cfv".
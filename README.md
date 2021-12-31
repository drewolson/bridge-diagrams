# Bridge Diagrams

A CLI, Discord bot, and Slack bot for generating plain-text bridge diagrams.

## Build

```
$ stack build
```

## Test

```
$ stack test
```

## Format

[Ormolu](https://github.com/tweag/ormolu) is used for formatting. The [Haskell
Language Server](https://github.com/haskell/haskell-language-server) should
automatically format using ormolu by default, however if you'd like to format
the code explicitly, you can use the following make tasks:

```
make format
```

```
make format-check
```

## Run

### CLI

The CLI reads input from `stdin`:

```
$ stack run bridge-cli-exe -- --help
```

```
$ stack run bridge-cli-exe <<< 'akxxx qtx - qjxxx'
♠AKxxx
♥QTx
♦
♣QJxxx

$ stack run bridge-cli-exe <<< 't987 6543 - 76532; akqj akqj ak kj9; - - q8765432 aqt84; 65432 t9872 jt9 -, r/r, imps'
Vul: R/R   ♠T987
IMPs       ♥6543
           ♦
           ♣76532
♠65432      -----     ♠AKQJ
♥T9872     |  N  |    ♥AKQJ
♦JT9       |W   E|    ♦AK
♣          |  S  |    ♣KJ9
            -----
           ♠
           ♥
           ♦Q8765432
           ♣AQT84
```

### Discord

The Discord bot needs a `DISCORD_TOKEN` with scopes that allow for connecting to
the server, sending messages, and managing messages.

```
DISCORD_TOKEN=<redacted> stack run bridge-discord-exe
```

Once associated with a server, messages prefixed with `!bridge` will be handled
by the bot.

```
!bridge akxxx qtx - qjxxx
```

### Slack

The Slack bot uses [socket mode](https://api.slack.com/apis/connections/socket)
and needs a [bot token](https://api.slack.com/authentication/token-types#bot)
and a [user token](https://api.slack.com/authentication/token-types#user).

The bot token needs the following scopes:

* chat:write
* chat:write.customize
* chat:write.public
* commands

The user token needs the following scopes:

* chat:write

```
SLACK_TOKEN=<redacted> SLACK_USER_TOKEN=<redacted> stack run bridge-slack-exe
```

The bot should be installed as a slash command. I tend to choose `/bridge` as
the command. Messages can then be sent to the bot via the slash command.

```
/bridge akxxx qtx - qjxxx
```

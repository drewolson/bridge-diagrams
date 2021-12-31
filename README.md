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

The Discord bot needs a `DISCORD_TOKEN`:

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
and needs a bot token and a user token:

```
SLACK_TOKEN=<redacted> SLACK_USER_TOKEN=<redacted> stack run bridge-slack-exe
```

The bot should be installed as a slash command. I tend to choose `/bridge` as
the command. Messages can then be sent to the bot via the slash command.

```
/bridge akxxx qtx - qjxxx
```

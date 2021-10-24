# Bridge Diagrams

A CLI, Discord bot and Slack bot for generating plain-text bridge diagrams.

## Build

```
$ stack build
```

## Test

```
$ stack test
```

## Run

The CLI reads input from `stdin`:

```
$ stack exec -- bridge-cli-exe --help
```

```
$ stack exec -- bridge-cli-exe <<< 'akxxx qtx - qjxxx'
♠AKxxx
♥QTx
♦
♣QJxxx

$ stack exec -- bridge-cli-exe <<< 't987 6543 - 76532; akqj akqj ak kj9; - - q8765432 aqt84; 65432 t9872 jt9 -'
           ♠T987
           ♥6543
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

The discord bot needs a `DISCORD_TOKEN`:

```
DISCORD_TOKEN=<redacted> stack exec -- bridge-discord-exe
```

The slack bot uses [socket mode](https://api.slack.com/apis/connections/socket)
and needs a bot token and a user token:

```
SLACK_TOKEN=<redacted> SLACK_USER_TOKEN=<redacted> stack exec -- bridge-slack-exe
```

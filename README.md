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
$ stack exec -- bridge-cli-exe <<< 'akxxx qtx - qjxxx'
♠AKxxx
♥QTx
♦
♣QJxxx
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

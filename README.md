# Counsel Spotify

Little Emacs package to control Spotify app through an Ivy interface.

> If I have seen further it is by standing on the shoulders of giants.
> Isaac Newton

This package is basically a port of [helm-spotify](https://github.com/krisajenkins/helm-spotify) and [helm-spotify-plus](https://github.com/wandersoncferreira/helm-spotify-plus)
with an Ivy interface. Many thanks to the authors of those for they amazing work!

## Dependencies

[Ivy](https://github.com/abo-abo/swiper)

## Notes on Spotify Web API authentication

You'll need to [register an application](https://developer.spotify.com/my-applications) in Spotify in order to obtain a client id and a client secret. Then, you'll have to set the variables
`counsel-spotify-client-id` and `counsel-spotify-client-secret` variables with your credentials to start using the search feature

## How to use it

This package exposes three search functions `counsel-spotify-search-track`, `counsel-spotify-search-album` and `counsel-spotify-search-artist`. The options are fetched from
Spotify REST API. Hitting `RET` while selecting an option will start playing that track or album (depending on what the search were).

### Controllers

Also, there are defined some basic controllers for the Spotify App

| Action            | Function                            |
|:----------------- |:----------------------------------- |
| Play              | `counsel-spotify-play`              |
| Toggle play/pause | `counsel-spotify-toggle-play-pause` |
| Next song         | `counsel-spotify-next`              |
| Previous song     | `counsel-spotify-previous`          |

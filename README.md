# HinfoBot

HinfoBot is purely an educational project, written in Haskell. 
The purpose of this project is to use https://newsapi.org to 
present data in a Discord server on a regular interval (likely 1 request per day)

It is entirely non-commercial code, and anyone else seeking to build a commercial 
Haskell App who would for some reason like to use my API wrapper can do so. 

## Important note
This is currently a work in progress, and is currently nowhere near a 1.0 release 

# Pre-requisites to running

1. You must have a valid NewsAPI.org API key present in the environment @ `$NEWS_API_KEY`
2. When the discord integration is finished, you will need another env variable for that.
3. For now, that's it. chmod +x the build script and run it 

# Building 
```bash
./build.sh
```

# Contributing
Freely open a PR or Issue and we can discuss any suggested changes.

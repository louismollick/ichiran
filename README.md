# Ichiran HTTP Node Server

Loosely based on [ichiran-web](https://github.com/tulerpetontidae/ichiran-web) fork of [ichiran](https://github.com/tshatrov/ichiran).
Basically an HTTP server that accepts a japanese sentence as input, and returns JSON that segments/separates each word & gives its meaning.

## Usage

`curl localhost:3000/心の膜が剥がれ落ちてゆく`

## How to run

See instructions from original repo: https://github.com/tshatrov/ichiran
1. `docker compose build`
2. `docker compose up`

## Where to deploy

1. Get a free GCP e2-micro VM, open web SSH console: https://www.youtube.com/watch?v=6meDCnIW4sU
2. `sudo apt-get update && sudo apt-get install git`
3. `curl -sS https://get.docker.com/ | sh`
4. `git clone https://github.com/louismollick/ichiran.git`
5. `cd ichiran`
6. `sudo docker compose build`
7. `sudo docker compose up`
8. Visit the website at 
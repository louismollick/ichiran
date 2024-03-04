# Ichiran-on-AWS

This repo makes small changes to the Japanese linguistic tool [`ichiran-cli`](https://github.com/tshatrov/ichiran) so it can be compiled to an executable for AWS Linux 2 (e.g, used by AWS Lambda).
Note that `ichiran-cli` requires a Postgres database to work (and some DB setup is done during `ichiran-cli` compilation time).
In the below instructions I use AWS RDS but any remote Postgres DB could work:

## Setup instructions

1. Clone this repo `git clone https://github.com/louismollick/ichiran-on-AWS.git`.

2. Create your [AWS RDS](https://aws.amazon.com/rds/) Postgres database. 
    a. Select "Publically accessible" "Yes", and uncheck "Encryption" in the DB creation wizard
    b. You'll also need to create a publically accessible security group per https://stackoverflow.com/a/52346331 and also turn off force_ssl https://stackoverflow.com/a/77787495
    c. Finally copy your database hostname and password.

3. Set the `PGHOST` and `ICHIRAN_CONNECTION` env variables in `docker-compose.yml` with your host & password values from above.

   a. if your database name is not `postgres`, change the first argument to match your default database name  
   b. you can find all the connection options in the (`postmodern` LISP package)[https://marijnhaverbeke.nl/postmodern/] docs.

4. Download the `jmdict-070124.pgdump` from the most recent `ichiran` release: https://github.com/tshatrov/ichiran/releases/download/ichiran-240107/jmdict-070124.pgdump (any version is fine)

5. Run the following command to transform the `pgdump` to an SQL file. This is necessary to remove the hardcoded `ja_JP.UTF-8` locale/encoding from the `pgdump` file. I have not yet encountered any issues with this setup, but ymmv.

```
pg_restore --file=- ~/jmdict-070124.pgdump > ~/jmdict.sql
```

6. Run `export PGPASSWORD=<password>` copied previously.

7. Replace with <host> (e.g, xxxx.us-east-1.rds.amazonaws.com) value and run the following command to connect and load data into your DB.

```
psql -h <host> --username=postgres --dbname=postgres < ~/jmdict.sql
```

8. When this finishes, you can now start compiling `ichiran-cli`. Let it run until you see `All set, awaiting commands.` printed to the console:

```
docker compose up --build
```

9. Run `docker ps` to get the container ID.

10. Then copy the copiled executable from the docker container to your computer:

```
docker cp <container ID>:/root/quicklisp/local-projects/ichiran/ichiran-cli ~/
```

11. Finally when running in AWS Lambda (or any hosting solution built on AWS Lambda e.g Vercel), set the `ICHIRAN_CONNECTION` environment variable. You can then run this `ichiran-cli` executable as a subprocess in your app:

```
const words = "心の膜が剥がれ落ちてゆく";
const cmd = path.join(process.cwd(), "src/ichiran-cli");
const output = childProcess.spawnSync(cmd, ["-f", words], {
  encoding: "utf8",
});
console.log(output.stdout)
```

```
[[[[["shin/kokoro",{"alternative":[{"reading":"\u5FC3 \u3010\u3057\u3093\u3011","text":"\u5FC3","kana":"\u3057\u3093","score":16,"seq":1595125,"gloss":[{"pos":"[n]","gloss":"heart; mind; spirit; vitality; inner strength"},{"pos":"[n]","gloss":"bottom of one's heart; core (of one's character); nature"},{"pos":"[n]","gloss":"centre; center; core; heart","info":"usu. written as \u82AF"},{"pos":"[n]","gloss":"heart (organ)"},{"pos":"[n]","gloss":"Chinese \"Heart\" constellation (one of the 28 mansions)"},{"pos":"[n]","gloss":"friend"}],"conj":[]},{"reading":"\u5FC3 \u3010\u3053\u3053\u308D\u3011","text":"\u5FC3","kana":"\u3053\u3053\u308D","score":16,"seq":1360480,"gloss":[{"pos":"[n]","gloss":"mind; heart; spirit"},{"pos":"[n]","gloss":"the meaning of a phrase (riddle, etc.)"}],"conj":[]}]},[]],["no",{"reading":"\u306E","text":"\u306E","kana":"\u306E","score":11,"seq":1469800,"gloss":[{"pos":"[prt]","gloss":"indicates possessive","info":"occasionally \u3093, orig. written \u4E43 or \u4E4B"},{"pos":"[prt]","gloss":"nominalizes verbs and adjectives"},{"pos":"[prt]","gloss":"substitutes for \"ga\" in subordinate phrases"},{"pos":"[prt]","gloss":"(at sentence-end, falling tone) indicates a confident conclusion","info":"often \u3093"},{"pos":"[prt]","gloss":"(at sentence-end) indicates emotional emphasis"},{"pos":"[prt]","gloss":"(at sentence-end, rising tone) indicates question"}],"conj":[]},[]],["maku",{"reading":"\u819C \u3010\u307E\u304F\u3011","text":"\u819C","kana":"\u307E\u304F","score":16,"seq":1524850,"gloss":[{"pos":"[n]","gloss":"membrane; film"}],"conj":[]},[]],["ga",{"reading":"\u304C","text":"\u304C","kana":"\u304C","score":11,"seq":2028930,"gloss":[{"pos":"[prt]","gloss":"indicates sentence subject (occasionally object)"},{"pos":"[prt]","gloss":"indicates possessive (esp. in literary expressions)"},{"pos":"[conj]","gloss":"but; however; still; and"},{"pos":"[conj]","gloss":"regardless of; whether (or not)","info":"after the volitional form of a verb"}],"conj":[]},[]],["hagareochite",{"reading":"\u5265\u304C\u308C\u843D\u3061\u3066 \u3010\u306F\u304C\u308C\u304A\u3061\u3066\u3011","text":"\u5265\u304C\u308C\u843D\u3061\u3066","kana":"\u306F\u304C\u308C\u304A\u3061\u3066","score":1001,"seq":10097728,"conj":[{"prop":[{"pos":"v1","type":[]}],"reading":"\u5265\u304C\u308C\u843D\u3061\u308B \u3010\u306F\u304C\u308C\u304A\u3061\u308B\u3011","gloss":[{"pos":"[vi,v1]","gloss":"to peel off and fall; to flake away"}],"readok":true}]},[]],["yuku",{"reading":"\u3086\u304F","text":"\u3086\u304F","kana":"\u3086\u304F","score":12,"seq":1578850,"gloss":[{"pos":"[v5k-s,vi]","gloss":"to go; to move (towards); to head (towards); to leave (for)"},{"pos":"[v5k-s,vi]","gloss":"to move through; to travel across; to walk along (e.g. a road)"},{"pos":"[v5k-s,vi]","gloss":"to go (well, badly, etc.); to proceed; to turn out; to get along"},{"pos":"[v5k-s,vi]","gloss":"to do (in a particular way); to go (with; a choice); to try","info":"oft. as \u3067\u301C"},{"pos":"[v5k-s,vi]","gloss":"to pass (of time, seasons, etc.); to go by"},{"pos":"[v5k-s,vi]","gloss":"to stream; to flow"},{"pos":"[v5k-s,vi]","gloss":"to die; to pass away","info":"usu. written as \u901D\u304F"},{"pos":"[v5k-s,vi]","gloss":"to reach (a stage, extent, age, etc.); to get to; to go (so far as ...)"},{"pos":"[v5k-s,vi]","gloss":"to reach (of information, instructions, wind, etc.); to arrive"},{"pos":"[aux-v,v5k-s]","gloss":"to continue ...; to go on ...; to (progress) steadily; to gradually ...; to progressively ...","info":"after the -te form of a verb; \u3044 sometimes omitted in casual speech"},{"pos":"[v5k-s,vi]","gloss":"to have an orgasm; to come; to cum","info":"oft. written as \u30A4\u30AF"},{"pos":"[vi,v5k-s]","gloss":"to trip; to get high; to have a drug-induced hallucination"}],"conj":[]},[]]],1095]]]
```

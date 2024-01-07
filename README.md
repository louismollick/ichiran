# Ichiran-AWS-HTTP-Server

Prereq: 
1. Configure a AWS Access ID / Secret with a IAM user that has Admin access: https://www.msp360.com/resources/blog/how-to-find-your-aws-access-key-id-and-secret-access-key/ 
2. Run `aws configure` to configure your AWS ~/.aws/credentials https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html#cli-configure-files-methods
3. If the `ruiisuuu/ichiran_[main/pg]` Docker images don't exist, you might need to build ichiran containers locally and push to DockerHub: `docker login`, `docker compose build`, `docker compose push`. You'll then need to replace `ruiisuuu/ichiran_[main/pg]` with `<your_docker_id>/ichiran_[main/pg]` in `docker-compose.yml`.

Deploy:  
To deploy to AWS Fargate and be accessible over HTTP, just run: `ecs-compose-x up -f docker-compose.yml -n ichiran`
Then, go to your AWS Console > EC2 > Load Balancers > ichira-publi-XXXXXXX > Copy the DNS name URL.

Query:  
Then use the URL you copied and put the Japanese sentence in the path (URL encoded):  
e.g, 心の膜が剥がれ落ちてゆく  
```
curl -s http://ichira-publi-XXXXXXX-386196307.us-east-1.elb.amazonaws.com/%E5%BF%83%E3%81%AE%E8%86%9C%E3%81%8C%E5%89%A5%E3%81%8C%E3%82%8C%E8%90%BD%E3%81%A1%E3%81%A6%E3%82%86%E3%81%8F`
```

Which returns JSON:
```
[[[[["shin/kokoro",{"alternative":[{"reading":"\u5FC3 \u3010\u3057\u3093\u3011","text":"\u5FC3","kana":"\u3057\u3093","score":16,"seq":1595125,"gloss":[{"pos":"[n]","gloss":"heart; mind; spirit; vitality; inner strength"},{"pos":"[n]","gloss":"bottom of one's heart; core (of one's character); nature"},{"pos":"[n]","gloss":"centre; center; core; heart","info":"usu. written as \u82AF"},{"pos":"[n]","gloss":"heart (organ)"},{"pos":"[n]","gloss":"Chinese \"Heart\" constellation (one of the 28 mansions)"},{"pos":"[n]","gloss":"friend"}],"conj":[]},{"reading":"\u5FC3 \u3010\u3053\u3053\u308D\u3011","text":"\u5FC3","kana":"\u3053\u3053\u308D","score":16,"seq":1360480,"gloss":[{"pos":"[n]","gloss":"mind; heart; spirit"},{"pos":"[n]","gloss":"the meaning of a phrase (riddle, etc.)"}],"conj":[]}]},[]],["no",{"reading":"\u306E","text":"\u306E","kana":"\u306E","score":11,"seq":1469800,"gloss":[{"pos":"[prt]","gloss":"indicates possessive","info":"occasionally \u3093, orig. written \u4E43 or \u4E4B"},{"pos":"[prt]","gloss":"nominalizes verbs and adjectives"},{"pos":"[prt]","gloss":"substitutes for \"ga\" in subordinate phrases"},{"pos":"[prt]","gloss":"(at sentence-end, falling tone) indicates a confident conclusion","info":"often \u3093"},{"pos":"[prt]","gloss":"(at sentence-end) indicates emotional emphasis"},{"pos":"[prt]","gloss":"(at sentence-end, rising tone) indicates question"}],"conj":[]},[]],["maku",{"reading":"\u819C \u3010\u307E\u304F\u3011","text":"\u819C","kana":"\u307E\u304F","score":16,"seq":1524850,"gloss":[{"pos":"[n]","gloss":"membrane; film"}],"conj":[]},[]],["ga",{"reading":"\u304C","text":"\u304C","kana":"\u304C","score":11,"seq":2028930,"gloss":[{"pos":"[prt]","gloss":"indicates sentence subject (occasionally object)"},{"pos":"[prt]","gloss":"indicates possessive (esp. in literary expressions)"},{"pos":"[conj]","gloss":"but; however; still; and"},{"pos":"[conj]","gloss":"regardless of; whether (or not)","info":"after the volitional form of a verb"}],"conj":[]},[]],["hagareochite",{"reading":"\u5265\u304C\u308C\u843D\u3061\u3066 \u3010\u306F\u304C\u308C\u304A\u3061\u3066\u3011","text":"\u5265\u304C\u308C\u843D\u3061\u3066","kana":"\u306F\u304C\u308C\u304A\u3061\u3066","score":1001,"seq":10186890,"conj":[{"prop":[{"pos":"v1","type":"Conjunctive (~te)"}],"reading":"\u5265\u304C\u308C\u843D\u3061\u308B \u3010\u306F\u304C\u308C\u304A\u3061\u308B\u3011","gloss":[{"pos":"[vi,v1]","gloss":"to peel off and fall; to flake away"}],"readok":true}]},[]],["yuku",{"reading":"\u3086\u304F","text":"\u3086\u304F","kana":"\u3086\u304F","score":12,"seq":1578850,"gloss":[{"pos":"[v5k-s,vi]","gloss":"to go; to move (in a direction or towards a specific location); to head (towards); to be transported (towards); to reach"},{"pos":"[v5k-s,vi]","gloss":"to proceed; to take place","info":"\u3044 sometimes omitted in auxiliary use"},{"pos":"[v5k-s,vi]","gloss":"to pass through; to come and go"},{"pos":"[v5k-s,vi]","gloss":"to walk"},{"pos":"[v5k-s,vi]","gloss":"to die; to pass away"},{"pos":"[v5k-s,vi]","gloss":"to do (in a specific way)"},{"pos":"[v5k-s,vi]","gloss":"to stream; to flow"},{"pos":"[v5k-s,aux-v]","gloss":"to continue","info":"after the -te form of a verb"},{"pos":"[v5k-s,vi]","gloss":"to have an orgasm; to come; to cum"},{"pos":"[vi,v5k-s]","gloss":"to trip; to get high; to have a drug-induced hallucination"}],"conj":[]},[]]],1095]]]
```

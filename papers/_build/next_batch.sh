#!/bin/sh
# print the next round-3 batch not yet launched, and mark it launched
cd /c/dev/stanpumpRPapers/_build
C:/Python314/python.exe -E -c "
import json
done=set(open('round3_launched.txt').read().split())
for b in json.load(open('digest_batches_3.json',encoding='utf-8')):
    if b['batch'] not in done:
        open('round3_launched.txt','a').write(b['batch']+'\n'); print(b['batch'], ' '.join(b['stems'])); break
else: print('NONE')"

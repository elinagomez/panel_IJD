import json
import sys
from pathlib import Path

script_dir = Path(__file__).resolve().parent
records = json.load(open(script_dir / "records.json", encoding="utf-8"))
q = sys.argv[1]
for rec in records:
    resp = rec.get(q)
    code = rec.get(f"codigo_{q}")
    if resp is None and code is None:
        continue
    text = str(resp or "").replace("\n", " ")
    if len(text) > 240:
        text = text[:237] + "..."
    print(f"{rec['row']:>3} | {text} | [{code}]")

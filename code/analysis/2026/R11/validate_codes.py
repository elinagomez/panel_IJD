import json
from pathlib import Path

script_dir = Path(__file__).resolve().parent
definitions = json.load(open(script_dir / "definitions.json", encoding="utf-8"))
records = json.load(open(script_dir / "records.json", encoding="utf-8"))

for q, defs in definitions.items():
    allowed = {d["category"] for d in defs}
    invalid = []
    blank = []
    used = {}
    for rec in records:
        val = rec.get(f"codigo_{q}")
        if val is None or str(val).strip() == "":
            blank.append(rec["row"])
            continue
        cats = [c.strip() for c in str(val).split(";") if c.strip()]
        for cat in cats:
            used[cat] = used.get(cat, 0) + 1
            if cat not in allowed:
                invalid.append((rec["row"], cat, val))
    print("\n", q)
    print("allowed", len(allowed), "used", used)
    print("blank", blank[:20], "n=", len(blank))
    print("invalid", invalid[:20], "n=", len(invalid))

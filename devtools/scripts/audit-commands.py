"""Audit commands across CNM-Command-Reference.csv, cnmalias.lsp, and cnmloader.lsp."""
import re, csv, os

base = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
csv_path = os.path.join(base, "devsource", "CNM-Command-Reference.csv")
alias_path = os.path.join(base, "devsource", "cnmalias.lsp")
loader_path = os.path.join(base, "devsource", "cnmloader.lsp")

# --- Parse CNM-Command-Reference.csv ---
csv_aliases = set()
csv_details = {}
with open(csv_path, "r") as f:
    reader = csv.reader(f)
    header = next(reader)
    for row_num, row in enumerate(reader, 2):
        if len(row) < 2:
            continue
        pgp = row[0].strip()
        cmd = row[1].strip()
        if not cmd:
            continue
        if cmd.startswith("*"):
            continue
        # Skip category rows (multi-word ALL CAPS)
        if " " in cmd and cmd == cmd.upper():
            continue
        try:
            float(pgp)
        except:
            continue
        csv_aliases.add(cmd.upper())
        csv_details[cmd.upper()] = f"line {row_num}"

# --- Parse cnmalias.lsp ---
alias_to_canonical = {}
alias_group = {}
canonical_in_alias = set()
with open(alias_path, "r") as f:
    for line_num, line in enumerate(f, 1):
        m = re.search(
            r'hcnm-define-(?:command|lisp)-alias\s+\x27\("([^"]+)"\s+"([^"]+)"\s+"([^"]+)"',
            line,
        )
        if m:
            canonical = m.group(1)
            alias = m.group(2).upper()
            group = m.group(3)
            alias_to_canonical[alias] = canonical
            alias_group[alias] = group
            canonical_in_alias.add(canonical.lower())

custompgp = {a for a, g in alias_group.items() if g == "Custompgp"}
standardpgp = {a for a, g in alias_group.items() if g == "Standardpgp"}
tools = {a for a, g in alias_group.items() if g == "Tools"}

# --- Parse cnmloader.lsp ---
loader_canonicals = set()
loader_filenames = set()
with open(loader_path, "r") as f:
    content = f.read()
for m in re.finditer(r'haws-autoload\s+"([^"]+)"', content):
    loader_filenames.add(m.group(1).lower())
for m in re.finditer(r'"(haws-[^"]+|hcnm-[^"]+)"', content):
    name = m.group(1).lower()
    if name not in loader_filenames:
        loader_canonicals.add(name)

# Build alias lookup from loader canonicals
loader_aliases = set()
for canon in loader_canonicals:
    for prefix in ("haws-", "hcnm-"):
        if canon.startswith(prefix):
            loader_aliases.add(canon[len(prefix):].upper())
            break
    else:
        loader_aliases.add(canon.upper())


def in_loader(alias):
    if alias in loader_aliases:
        return True
    # Check via canonical from cnmalias
    canon = alias_to_canonical.get(alias, "").lower()
    return canon in loader_canonicals


def in_cnmalias(alias):
    return alias in alias_to_canonical


def in_csv(alias):
    return alias in csv_aliases


# --- Report ---
print("=" * 85)
print("COMMAND AUDIT: Cross-referencing 3 files")
print("=" * 85)
print(f"  CSV commands:          {len(csv_aliases)}")
print(f"  cnmalias aliases:      {len(alias_to_canonical)} (Tools: {len(tools)}, Custompgp: {len(custompgp)}, Standardpgp: {len(standardpgp)})")
print(f"  cnmloader canonicals:  {len(loader_canonicals)}")

# Collect all aliases (but only tools-group + CSV + loader for cross-reference)
# PGP aliases (Custompgp/Standardpgp) are simple AutoCAD command remappings,
# not LISP commands, so they are NOT expected in cnmloader.lsp.
all_aliases = csv_aliases | tools | loader_aliases

print(f"\n{'='*85}")
print(f"{'ALIAS':<20} {'CSV':>6} {'ALIAS':>8} {'LOADER':>8}   NOTES")
print(f"{'-'*85}")

gaps = []
for a in sorted(all_aliases):
    c = in_csv(a)
    al = in_cnmalias(a)
    lo = in_loader(a)
    is_pgp = a in custompgp or a in standardpgp

    if c and al and lo:
        continue  # All 3 - passes

    # PGP aliases: only need CSV + cnmalias (loader N/A)
    if is_pgp:
        if c and al:
            continue
        notes = f"PGP alias -> {alias_to_canonical.get(a, '?')}"
        lo_s = "N/A"
    else:
        notes_parts = []
        if al:
            notes_parts.append(f"canonical: {alias_to_canonical.get(a, '?')}")
        if a in csv_details:
            notes_parts.append(f"CSV {csv_details[a]}")
        notes = "; ".join(notes_parts)
        lo_s = "YES" if lo else "-"

    c_s = "YES" if c else "-"
    al_s = "YES" if al else "-"
    if is_pgp:
        lo_s = "N/A"

    gaps.append((a, c_s, al_s, lo_s, notes))
    print(f"  {a:<20} {c_s:>6} {al_s:>8} {lo_s:>8}   {notes}")

print(f"\n  Total commands with gaps: {len(gaps)}")

# Section summaries
print(f"\n{'='*85}")
print("SECTION SUMMARIES")
print(f"{'='*85}")

in_csv_not_alias = sorted([a for a in csv_aliases if not in_cnmalias(a)])
print(f"\n  In CSV only (no cnmalias entry): {len(in_csv_not_alias)}")
for a in in_csv_not_alias:
    print(f"    {a}")

in_csv_not_loader = sorted([a for a in csv_aliases if not in_loader(a) and a not in custompgp and a not in standardpgp])
print(f"\n  In CSV but not loader (excl. PGP): {len(in_csv_not_loader)}")
for a in in_csv_not_loader:
    print(f"    {a}")

in_alias_not_csv = sorted([a for a in tools if not in_csv(a)])
print(f"\n  In cnmalias (Tools) but not CSV: {len(in_alias_not_csv)}")
for a in in_alias_not_csv:
    canon = alias_to_canonical.get(a, "?")
    print(f"    {a:<20} -> {canon}")

in_loader_not_csv = sorted([c for c in loader_canonicals
                            if (c[5:].upper() if c.startswith("haws-") else c[5:].upper() if c.startswith("hcnm-") else c.upper()) not in csv_aliases])
print(f"\n  In cnmloader but not CSV: {len(in_loader_not_csv)}")
for c in in_loader_not_csv:
    print(f"    {c}")

in_loader_not_alias = sorted([c for c in loader_canonicals if c not in canonical_in_alias])
print(f"\n  In cnmloader but not cnmalias: {len(in_loader_not_alias)}")
for c in in_loader_not_alias:
    print(f"    {c}")

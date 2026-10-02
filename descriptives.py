"""Descriptive statistics for London pubs and bars by borough, 2001-2022.

Reads the 'Pubs size' sheet of the ONS workbook and prints summaries of the
number of pubs by employee size band (fewer than 10 / 10 or more).

Usage: python descriptives.py [path/to/workbook.xlsm]
"""
import sys
from pathlib import Path

import pandas as pd

DEFAULT_FILE = "publichousesandbarslondonboroughs20012022.xlsm"
SHEET = "Pubs size"
BANDS = {"Any number of employees": "total", "Fewer than 10": "small", "10 or more": "large"}


def load(path):
    raw = pd.read_excel(path, sheet_name=SHEET, header=None, engine="openpyxl")
    header_row = raw.index[raw.iloc[:, 0] == "Area code"][0]
    df = raw.iloc[header_row + 1:].copy()
    df.columns = raw.iloc[header_row]
    df = df[df["Area code"].astype(str).str.match(r"^[EK]\d{8}$")].copy()
    df.columns = [str(int(c)) if isinstance(c, (int, float)) else str(c) for c in df.columns]
    years = [c for c in df.columns if c.isdigit()]
    df[years] = df[years].apply(pd.to_numeric, errors="coerce")
    df["band"] = df["Number of employees in public house or bar"].map(BANDS)
    return df, years


def main():
    path = Path(sys.argv[1] if len(sys.argv) > 1 else DEFAULT_FILE)
    df, years = load(path)
    first, last = years[0], years[-1]
    pd.set_option("display.width", 140)

    boroughs = df[df["Area code"].str.startswith("E09")]
    london = df[df["Area name"] == "London"].set_index("band")[years]

    print(f"== London totals by size band ({first}-{last}) ==")
    summary = pd.DataFrame({
        first: london[first],
        last: london[last],
        "change": london[last] - london[first],
        "change_%": ((london[last] / london[first] - 1) * 100).round(1),
        "peak_year": london.idxmax(axis=1),
        "peak": london.max(axis=1),
        "min_year": london.idxmin(axis=1),
        "min": london.min(axis=1),
    })
    print(summary, "\n")

    print("== Share of London pubs with 10+ employees (%) ==")
    share = (london.loc["large"] / london.loc["total"] * 100).round(1)
    print(share[[first, years[len(years) // 2], last]].to_string(), "\n")

    print(f"== Borough descriptives, total pubs (n={boroughs['Area name'].nunique()} boroughs) ==")
    tot = boroughs[boroughs["band"] == "total"].set_index("Area name")[years]
    print(tot[[first, last]].describe().round(1), "\n")

    print(f"== Borough ranking, {last} ==")
    wide = boroughs.pivot_table(index="Area name", columns="band", values=last)
    wide["large_share_%"] = (wide["large"] / wide["total"] * 100).round(1)
    wide["change_since_" + first] = tot[last] - tot[first]
    wide["change_%"] = ((tot[last] / tot[first] - 1) * 100).round(1)
    print(wide.sort_values("total", ascending=False)[
        ["total", "small", "large", "large_share_%", "change_since_" + first, "change_%"]
    ].to_string(), "\n")

    print("== Largest falls / rises in total pubs ==")
    print(wide["change_since_" + first].sort_values().head(5).to_string(), "\n")
    print(wide["change_since_" + first].sort_values().tail(5).to_string())


if __name__ == "__main__":
    main()

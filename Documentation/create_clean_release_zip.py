#!/usr/bin/env python3
"""Create a clean ZIP while excluding common macOS metadata files."""
from pathlib import Path
import argparse, zipfile

EXCLUDED_DIRS = {"__MACOSX"}

def excluded(path: Path) -> bool:
    return any(part in EXCLUDED_DIRS for part in path.parts) or path.name == ".DS_Store" or path.name.startswith("._")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("source", help="Folder to archive")
    parser.add_argument("output", help="Output ZIP path")
    args = parser.parse_args()
    source = Path(args.source).resolve()
    output = Path(args.output).resolve()
    if not source.is_dir():
        raise SystemExit(f"Source folder not found: {source}")
    output.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(output, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as zf:
        for path in sorted(source.rglob("*")):
            rel = path.relative_to(source.parent)
            if excluded(rel) or path.is_dir():
                continue
            zf.write(path, rel.as_posix())
    print(f"Created clean archive: {output}")

if __name__ == "__main__":
    main()
